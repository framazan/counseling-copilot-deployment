import os
import sys
import argparse
import pyodbc
from datetime import datetime
import pytz
import pandas as pd
import numpy as np
import re
import json
from openai import OpenAI

def get_finetuning_data_from_db(
        db_server,
        db_database,
        db_username,
        db_password,
        db_driver,
        messages_df,
        agent_names
):
    # Create the connection string
    connectionString = f'DRIVER={db_driver};SERVER={db_server};DATABASE={db_database};UID={db_username};PWD={db_password}'
    
    conn = pyodbc.connect(connectionString)
    query = "SELECT * FROM Agents1;"
    agents_df = pd.read_sql_query(query, conn)
    conn.close()
    
    agents_df.columns = [col.lower() for col in agents_df.columns]
    agents_df.rename(columns={'id': 'agent_id'}, inplace=True)
    agents_df['doj'] = pd.to_datetime(agents_df['doj'])
    agents_df.agent_id = agents_df.agent_id.astype('Int64')
    agents_df = agents_df[agents_df['name'].isin(agent_names)]
    agents_df = agents_df[['agent_id', 'name', 'doj']]
    agents_df.reset_index(drop=True, inplace=True)
    
    conn = pyodbc.connect(connectionString)
    query = f"SELECT * FROM {messages_df};"
    historical_messages = pd.read_sql_query(query, conn)
    conn.close()

    historical_messages.columns = [col.lower() for col in historical_messages.columns]
    # historical_messages = historical_messages.groupby('ticket_id').filter(lambda x: len(x) >= min_messages_per_group)
    historical_messages.rename(columns={'sent_date_and_time': 'sent_date', 'thread_content': 'message'}, inplace=True)
    historical_messages['sent_date'] = pd.to_datetime(historical_messages['sent_date'])
    historical_messages.agent_id = historical_messages.agent_id.astype('Int64')
    historical_messages.sort_values(by=['ticket_id', 'sent_date'], ascending=True, inplace=True)
    historical_messages['user'] = np.where(historical_messages['agent_id'].isnull() | (historical_messages['agent_id'] == 'None'), 'client', 'counselor')

    # Filter out ticket_id where at least one element is null or contains missed message string
    # missed_message_str = ["we missed your", "missed your chat", "due to maintenance", "high traffic", "time bound chat", "9999666555", "9999 666 555", "reach out to us again"]
    missed_message_str = ["we missed your", "missed your chat", "due to maintenance", "high traffic", "time bound chat", "reach out to us again"]
    regex_pattern_missed_message_str = '|'.join([re.escape(s) for s in missed_message_str])
    columns_to_check = ['id', 'ticket_id', 'sent_date', 'message', 'user']
    tickets_with_nulls_in_any_column = historical_messages[historical_messages[columns_to_check].isnull().any(axis=1)]['ticket_id'].unique()
    tickets_with_missed_message = historical_messages[historical_messages['message'].str.contains(regex_pattern_missed_message_str, case=False, na=False)]['ticket_id'].unique()
    tickets_with_good_classification = historical_messages[historical_messages['classifications'] == 'Good']['ticket_id'].unique()
    historical_messages = historical_messages[~historical_messages['ticket_id'].isin(tickets_with_nulls_in_any_column)]
    historical_messages = historical_messages[~historical_messages['ticket_id'].isin(tickets_with_missed_message)]
    historical_messages = historical_messages[historical_messages['ticket_id'].isin(tickets_with_good_classification)]
    
    return historical_messages, agents_df

def filter_historical_messages(
        historical_messages,
        agents_df, 
        threshold_time,
        required_xp
):
    # Define the time difference threshold for conversation groups
    threshold_time_delta = pd.Timedelta(hours=threshold_time)
    
    # Group by 'ticket_id' and calculate 'conversation_group'
    # x.diff() computes the difference between consecutive timestamp entries within each group.
    # .cumsum() cumulatively sums the True values, effectively incrementing the group identifier each time a True is encountered.
    historical_messages['conversation_group'] = historical_messages.groupby('ticket_id').sent_date.apply(lambda x: (x.diff() > threshold_time_delta).cumsum() + 1).reset_index(drop=True)

    # Assign a unique conversation_id for each group within the same ticket
    historical_messages['ticket_id'] = historical_messages['ticket_id'].astype(str)
    historical_messages['conversation_id'] = historical_messages.groupby('ticket_id').apply(
        lambda g: g['conversation_group'].astype(str).radd(g.name + '-')
    ).reset_index(level=0, drop=True)

    # Calculate agent experience at age of message
    xp_historical_messages = historical_messages.merge(agents_df[['agent_id', 'doj']], on='agent_id', how='left')
    xp_historical_messages['xp_at_msg_sent'] = ((xp_historical_messages['sent_date'].dt.year - xp_historical_messages['doj'].dt.year) * 12 
                                              + xp_historical_messages['sent_date'].dt.month - xp_historical_messages['doj'].dt.month 
                                              - ((xp_historical_messages['sent_date'].dt.day < xp_historical_messages['doj'].dt.day).astype(int)))
    not_enough_xp_msg_index = xp_historical_messages[(xp_historical_messages['xp_at_msg_sent'].notna()) & (xp_historical_messages['xp_at_msg_sent'] < required_xp)]
    full_xp_historical_messages = xp_historical_messages.drop(not_enough_xp_msg_index.index)

    # Remove conversation_id that don't have at least one agent_id from our list of agents
    valid_agent_ids = set(agents_df['agent_id'])
    filtered_historical_messages = full_xp_historical_messages.groupby('conversation_id').filter(lambda x: x['agent_id'].isin(valid_agent_ids).any())

    return filtered_historical_messages

def openai_redact_phi(
        message, 
        client
):
    system_prompt = f"""
    Redact all protected health information (PHI) from the message I share with you. If there is no PHI in the message, then return the original message. Here are some examples:
    
    Message: Hello my name is Bob, I was born in California on January 1st 1990 and my phone number is 3226258081
    Your output: Hello my name is <PHI>, I was born in <PHI> on <PHI> and my phone number is <PHI>

    Message: i spoke with devin yesterday and he told me now to worry about contacting Vandrevala Foundation
    Your output: i spoke with <PHI> yesterday and he told me now to worry about contacting <PHI>

    Message: My number is +91 33 12345678 and please tell Divya to hurry i dont know what else to do
    Your output: My number is <PHI> and please tell <PHI> to hurry i dont know what else to do

    Message: do you think you can help me with a task that will require at least 30 minutes of your time?
    Your output: do you think you can help me with a task that will require at least 30 minutes of your time?

    Message: You can contact me at my email devvrat.dgu@gmail.com or visit me in San Francisco or Los Angeles. whatever is most convenient for you i don’t care
    Your output: You can contact me at my email <PHI> or visit me in <PHI> or <PHI>. whatever is most convenient for you i don’t care

    Message: hello mam, i am in a very bad situation and i need help. i am feeling very low and i don’t know what to do. please help me
    Your output: hello mam, i am in a very bad situation and i need help. i am feeling very low and i don’t know what to do. please help me

    Message: I am feeling very suicidal right about now and i don’t know what to do.
    Your output: I am feeling very suicidal right about now and i don’t know what to do.
    """
    user_prompt = f"""
    Message: {message}
    Your output: """
    output = client.chat.completions.create(
      model='gpt-4o',
      messages=[
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": user_prompt}
      ],
      temperature=0.2
    )

    formatted_output = output.choices[0].message.content.replace('Your output: ', '').replace('Message: ', '')
    formatted_output = ' '.join(formatted_output.split())
    return formatted_output

def assign_conversation_id(
        group, 
        threshold_time
):
    group['conversation_id'] = (group['sent_date'].diff().dt.total_seconds() / 3600 > threshold_time).cumsum()
    return group

def merge_messages(
        group
):
    # Initialize a list to store the structured messages
    structured_messages = []

    # Initialize variables to store the previous message data
    previous_user = None
    combined_message = ""
    first_time = None

    # Iterate over each message in the group
    for _, row in group.iterrows():
        # Check if the current message is from the same user as the previous one
        if row['user'] == previous_user:
            # Continue combining messages
            combined_message += "\n" + row['message']
        else:
            # If the sender changes (or it's the first message), save the previous message(s) if any
            if previous_user is not None:
                structured_messages.append({
                    'conversation_id': row['conversation_id'],
                    'user': previous_user,
                    'sent_date': first_time,
                    'combined_message': combined_message
                })
            # Start a new set of combined messages
            previous_user = row['user']
            combined_message = row['message']
            first_time = row['sent_date']
    
    # Add the last set of messages to the list
    if combined_message:
        structured_messages.append({
            'conversation_id': group['conversation_id'].iloc[0],
            'user': previous_user,
            'sent_date': first_time,
            'combined_message': combined_message
        })

    merged_df = pd.DataFrame(structured_messages)

    # Convert the list of structured messages to a DataFrame
    return merged_df

def trim_conversation_messages(
        group
):
    # Check if the group is not empty
    if not group.empty:
        # Check if the last message in the group is from the client
        if group['user'].iloc[-1] == 'client':
            group = group[:-1]
        
        # After removing the last message, check again if the group is not empty
        if not group.empty and group['user'].iloc[0] == 'counselor':
            group = group[1:]
        
    return group

def prepare_json_data_to_jsonl(
        df, 
        system_prompt,
        filename
):
    # Open a file to write JSONL outputs
    with open(filename, 'w') as file:
        # Group by conversation_id
        for conversation_id, group in df.groupby('conversation_id'):
            messages = []
            # Prepend the introduction message for the assistant context
            messages.append({
                "role": "system",
                "content": f"{system_prompt}"
            })

            # Append messages, converting roles 'client' to 'user' and 'counselor' to 'assistant'
            for _, row in group.iterrows():
                role = "user" if row['user'] == 'client' else "assistant"
                messages.append({
                    "role": role,
                    "content": row['combined_message'].replace('\n', ' ')
                })

            # Construct the full conversation JSON string
            json_string = json.dumps({"messages": messages})
            # Write each JSON string as a new line in the file
            file.write(json_string + '\n')