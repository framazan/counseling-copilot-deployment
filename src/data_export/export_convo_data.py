import argparse
import os
import pandas as pd
import numpy as np
from datetime import datetime
import textwrap
import hashlib
import re
from pull_convo_table import compute_bleu as sentence_bleu_2

import pyodbc
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Database connection parameters
SERVER = 'tcp:gptdata.database.windows.net,1433'
DATABASE = 'aidata'
USERNAME = 'chatgpt'
PASSWORD = os.getenv("PROD_DB_PWD")  # Ensure this is stored securely
DRIVER = '{ODBC Driver 18 for SQL Server}'

def get_db_connection():
    """Establishes and returns a database connection."""
    connection_string = (
        f"DRIVER={DRIVER};"
        f"SERVER={SERVER};"
        f"DATABASE={DATABASE};"
        f"UID={USERNAME};"
        f"PWD={PASSWORD};"
    )
    return pyodbc.connect(connection_string)


def run_query(query):
    """Executes a given SQL query and returns the result as a pandas DataFrame."""
    conn = get_db_connection()
    try:
        df = pd.read_sql(query, conn)
    finally:
        conn.close()
    return df


def snake_case_columns(df):
    """Converts column names to snake_case."""
    df.columns = (
        df.columns.str.strip()
        .str.lower()
        .str.replace(" ", "_")
        .str.replace("-", "_")
    )
    return df


def get_log_analysis_root():
    """Get the log_analysis root folder."""
    if os.path.exists("/datadrive/vf_copilot"):
        return "/datadrive/vf_copilot/log_analysis"
    elif os.path.exists("/home/stanford/vf_copilot"):
        return "/home/stanford/vf_copilot/log_analysis"


def fetch_convo_with_scores(cutoff_date):
    """
    Pull conversation data from result_matrix + form_matrix and
    LEFT JOIN existing scores from convo_scores.

    If there are no scores for a given row, the score columns will be NaN.
    """
    query = f"""
    SELECT 
        -- IDs / linkage
        rm.openAiId       AS openai_response_id,
        rm.phone          AS patient_phone,
        fm.operatorId     AS counselor_id,

        -- Counselor response + feedback
        fm.responseSent   AS true_counselor_response,
        fm.feedbackflag   AS feedback_flag,
        fm.formbutton     AS feedback_action,

        -- AI suggestion + prompts / history
        rm.aisuggestion   AS ai_suggestion,
        rm.prompt         AS initial_prompt,
        rm.conversation   AS convo_history,
        rm.openaiFirst    AS initial_ai_suggestion,
        rm.advprompt      AS advanced_prompt,
        rm.advtext        AS advanced_text,

        -- Timing
        rm.tstart         AS generation_start_time,
        rm.tend           AS generation_end_time,
        fm.tsent          AS response_sent_time,

        -- Existing scores (can be NULL)
        cs.created_at     AS score_created_at,
        cs.hash_key,
        cs.bleu_scores,
        cs.bert_precision,
        cs.bert_recall,
        cs.bert_f1

    FROM 
        dbo.result_matrix rm
    LEFT JOIN 
        dbo.form_matrix fm 
          ON rm.openAiId = fm.openAiId
         AND rm.phone    = fm.phone

    LEFT JOIN
        dbo.convo_scores cs
          ON rm.openAiId   = cs.openai_response_id
         AND rm.phone      = cs.patient_phone
         AND fm.operatorId = cs.counselor_id

    WHERE
        rm.tstart > '{cutoff_date}'

    ORDER BY 
        patient_phone,
        counselor_id,
        generation_end_time;
    """

    return run_query(query)


def assign_conversations(df, max_hours_per_convo=3, chunk_size=1000000):
    """
    Group by visitor_phone and identify conversation boundaries where there's a gap of
    > max_hours_per_convo or a change in visitor_phone. Assign a numerical conversation_id
    and a uid by hashing the conversation.
    """
    # Check if dataframe is large enough to require chunking
    if len(df) <= chunk_size:
        return _process_conversation_assignment(df, max_hours_per_convo)
        
    # For large dataframes, process in chunks by phone number
    print(f"Large dataframe detected ({len(df)} rows). Processing in chunks...")

    # Get unique phone numbers
    unique_phones = df['visitor_phone'].unique()

    # Split phone numbers into chunks for processing
    phone_chunks = np.array_split(unique_phones, max(1, len(unique_phones) // (chunk_size // 100)))

    processed_dfs = []
    conversation_id_offset = 0

    for i, phone_chunk in enumerate(phone_chunks):
        print(f"Processing chunk {i+1}/{len(phone_chunks)} ({len(phone_chunk)} phone numbers)")

        # Filter data for current chunk of phone numbers
        chunk_df = df[df['visitor_phone'].isin(phone_chunk)].copy()

        # Skip empty chunks
        if len(chunk_df) == 0:
            continue

        # Process this chunk
        processed_chunk = _process_conversation_assignment(chunk_df, max_hours_per_convo)

        # Adjust conversation IDs to maintain uniqueness across chunks
        processed_chunk['conversation_id'] += conversation_id_offset

        # Update the offset for the next chunk
        if len(processed_chunk) > 0:
            conversation_id_offset = processed_chunk['conversation_id'].max() + 1

        processed_dfs.append(processed_chunk)

    # Combine all processed chunks
    if processed_dfs:
        return pd.concat(processed_dfs, ignore_index=True)
    return df

def _make_hash(conversation):
    return hashlib.sha256(conversation.encode('utf-8')).hexdigest()[:16]

def _make_conversation_from_messages(messages_df_group, sender_col="sender", message_col="message"):
    return "\n".join(
        messages_df_group.sort_values("msg_sequence_overall").apply(
            lambda y: y[sender_col] + ": " + y[message_col]
            , axis=1)
    )

def _process_conversation_assignment(df, max_hours_per_convo):

    df = df.sort_values(['visitor_phone', 'DateInserted'])
    df['time_diff'] = df.groupby('visitor_phone')['DateInserted'].diff()
    df['new_conversation'] = (
        df['time_diff'] > pd.Timedelta(hours=max_hours_per_convo)
    ) | (df['visitor_phone'] != df['visitor_phone'].shift())
    df['conversation_id'] = df['new_conversation'].cumsum()

    # Enumerate outgoing messages
    df["msg_sequence"] = df[df.direction == "Outgoing"].groupby("conversation_id").cumcount()

    # Enumerate all messages
    df["msg_sequence_overall"] = df.groupby("conversation_id").cumcount()

    # Create unique identifier by hashing each conversation
    phone_conversation_texts = df.groupby('conversation_id').apply(
        lambda x: f'{df['visitor_phone']}:{_make_conversation_from_messages(x)}')
    conversation_hashes = phone_conversation_texts.apply(_make_hash)
    df['uid'] = df['conversation_id'].map(conversation_hashes)

    return df


def get_conversation_summary(convos_to_review):
        return convos_to_review.groupby("uid").apply(
            lambda x: pd.Series({
                "date": x["DateInserted"].min() if "DateInserted" in x.columns else None,
                "conversation": _make_conversation_from_messages(x) if "msg_sequence_overall" in x.columns else None,
                "ai_convo": x["ai_convo"].iloc[0] if "ai_convo" in x.columns else None,
                "n_messages": x["n_messages"].iloc[0] if "n_messages" in x.columns and len(x) > 0 else 0,
                "n_ai_messages": x["n_ai_messages"].iloc[0] if "n_ai_messages" in x.columns and len(x) > 0 else None,
                "counsellor": x["counsellor"].loc[x["counsellor"].first_valid_index()] if "counsellor" in x.columns and len(x) > 0 and x["counsellor"].notna().any() else None,
            })
        ).reset_index()


def print_convo_for_review(df, msg_csv_file, convo_csv_file):
    """
    Renames and filters columns for a final CSV export at both the message and conversation level.
    """
    # Some columns might be missing. We'll keep whichever exist in this list
    wanted_cols = [
        'DateInserted', 'counsellor', 'conversation_id',
        'msg_sequence_overall', 'sender', 'message', 'ai_suggestion',
        'advanced_prompt', 'ai_response_edited', 'bleu_scores', 'uid'
    ]
    existing_cols = [c for c in wanted_cols if c in df.columns]
    df_to_print = df[existing_cols]

    df_to_print.to_csv(msg_csv_file, index=False)

    conversation_summary = get_conversation_summary(df)
    conversation_summary.to_csv(convo_csv_file, index=False)

    return df_to_print

def find_copied_ai_messages(
    convo_df,
    historical_df,
    time_window="3min",
    bleu_threshold=0.70,
    max_hist_per_ai=10,      # cap candidates per AI event for speed
    require_ai_text=True     # only consider rows where an AI suggestion exists
):
    """
    For rows in convo_df where true_counselor_response is null,
    find outgoing historical messages (same phone) within `time_window` after the AI event
    that have BLEU-2 >= `bleu_threshold` vs the AI suggestion text.

    Returns a DataFrame of matches (one row per best match).
    """
    ts_c = "generation_end_time"
    ts_h = "DateInserted"

    # Ensure datetimes
    convo_df[ts_c] = pd.to_datetime(convo_df[ts_c], errors="coerce")
    historical_df[ts_h] = pd.to_datetime(historical_df[ts_h], errors="coerce")

    # Narrow to AI rows where counselor didn't send from popup
    cond = convo_df["true_counselor_response"].isna()
    if require_ai_text and "ai_suggestion" in convo_df.columns:
        cond = cond & convo_df["ai_suggestion"].notna() & (convo_df["ai_suggestion"].str.strip() != "")

    ai_rows = convo_df.loc[
        cond,
        ["openai_response_id", "patient_phone", ts_c, "ai_suggestion"]
    ].rename(columns={ts_c: "ai_time"}).copy()

    # Historical outgoing/counselor messages
    # Adjust direction/sender columns as needed if your schema differs
    hist_cols = ["visitor_phone", ts_h, "message", "direction", "sender"]
    hist_cols = [c for c in hist_cols if c in historical_df.columns]
    hist = historical_df[hist_cols].copy()
    hist = hist.rename(columns={ts_h: "hist_time"})

    # Filter to counselor/outgoing messages
    if "direction" in hist.columns:
        hist = hist[hist["direction"].fillna("").str.lower().eq("outgoing")]
    elif "sender" in hist.columns:
        hist = hist[hist["sender"].fillna("").str.lower().eq("counselor")]

    # Join on phone; afterward we time-filter
    merged = ai_rows.merge(
        hist.rename(columns={"visitor_phone": "patient_phone", "message":"hist_message"}),
        on="patient_phone",
        how="left",
        suffixes=("", "_hist")
    )

    # Keep only messages sent AFTER the AI event and within the time window
    merged = merged[
        (merged["hist_time"] >= merged["ai_time"]) &
        (merged["hist_time"] <= merged["ai_time"] + pd.to_timedelta(time_window))
    ].copy()

    # Optional: limit candidate historical messages per AI event for speed
    merged["ai_id"] = np.arange(len(merged))  # temporary unique id so we can groupby below without losing order
    merged["ai_key"] = merged["patient_phone"].astype(str) + "|" + merged["ai_time"].astype(str)

    if max_hist_per_ai is not None:
        merged = (
            merged.sort_values(["ai_key", "hist_time"])
                  .groupby("ai_key", group_keys=False)
                  .head(max_hist_per_ai)
        )

    # Compute BLEU vs AI suggestion
    # If ai_suggestion is missing (shouldn’t be if require_ai_text=True), fall back to 0
    merged["bleu2"] = merged.apply(
        lambda r: sentence_bleu_2(r.get("ai_suggestion", ""), r.get("hist_message", "")),
        axis=1
    )

    # Keep strong matches
    strong = merged[merged["bleu2"] >= bleu_threshold].copy()
    if strong.empty:
        return strong

    # Choose best match per AI event (highest BLEU, tie-breaker earliest hist_time)
    strong = (strong.sort_values(["ai_key", "bleu2", "hist_time"], ascending=[True, False, True])
                    .groupby("ai_key", as_index=False).first())

    # Final tidy columns
    strong["time_delta_sec"] = (strong["hist_time"] - strong["ai_time"]).dt.total_seconds()
    out_cols = [
        "openai_response_id", "patient_phone", "ai_time", "ai_suggestion",
        "hist_time", "hist_message", "bleu2", "time_delta_sec"
    ]
    return strong[out_cols].sort_values(["patient_phone", "ai_time"])


def main():
    parser = argparse.ArgumentParser(
        description="Merge historical messages from SalesIQ with the latest convo_df, assign conversation IDs, sample, and export CSVs.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent('''\
            Example Usage:
            -------------
            python merge_historical.py \\
              --sample_cutoff 2024-01-09 \\
              --min_convo_length 30 --max_convo_length 200 \\
              --min_ai_msgs 3 --n_convos_per_counselor 5000
        ''')
    )
    parser.add_argument("--non_ai_tables_only", action="store_true", default=False,
                        help="Only process data from SalesIQ table, not the tables with extra details on AI messages.")
    parser.add_argument("--sample_cutoff", type=str, default="2024-01-09",
                        help="Date after which to begin sampling conversations.")
    parser.add_argument("--min_convo_length", type=int, default=30,
                        help="Minimum conversation length.")
    parser.add_argument("--max_convo_length", type=int, default=200,
                        help="Maximum conversation length.")
    parser.add_argument("--min_ai_msgs", type=int, default=3,
                        help="Minimum AI messages for an AI conversation.")
    parser.add_argument("--n_convos_per_counselor", type=int, default=5000,
                        help="Max sample size per counselor.")

    args = parser.parse_args()
    
    # -------------------------------------------------------------------------
    # Attrition trackers
    # -------------------------------------------------------------------------
    msg_attrition = []   # for salesIQ / historical messages
    rm_attrition  = []   # for result_matrix / AI convo_df

    # Baseline counts from original tables
    salesiq_total_df = run_query("SELECT COUNT(*) AS n FROM dbo.salesIQ;")
    salesiq_total = int(salesiq_total_df["n"].iloc[0])
    msg_attrition.append((
        "SalesIQ: all rows in dbo.salesIQ", 
        salesiq_total
    ))

    if not args.non_ai_tables_only:
        result_total_df = run_query("SELECT COUNT(*) AS n FROM dbo.result_matrix;")
        result_total = int(result_total_df["n"].iloc[0])
        rm_attrition.append((
            "Result_matrix: all rows in dbo.result_matrix", 
            result_total
        ))


    # -------------------------------------------------------------------------
    # 1) Load the recent convo_df directly from SQL (result_matrix + form_matrix + convo_scores)
    # -------------------------------------------------------------------------
    if not args.non_ai_tables_only:
        print("Fetching copilot data from SQL...")
        convo_df = fetch_convo_with_scores(cutoff_date=args.sample_cutoff)
        rm_attrition.append((
            f"Result_matrix-derived rows with tstart > {args.sample_cutoff} (after joins)",
            len(convo_df)
        ))
        
        if convo_df is None or convo_df.empty:
            print("No conversation data found after cutoff. Exiting.")
            return
        

    # -------------------------------------------------------------------------
    # 2) Load historical data from SalesIQ
    # -------------------------------------------------------------------------
    print("Fetching historical messages from SalesIQ...")
    historical_query = f"""
        SELECT
            id,
            DateInserted,
            event,
            message_sender_id,
            message_sender_name,
            visitor_phone,
            msg as message
        FROM dbo.salesIQ
        WHERE msg IS NOT NULL
          AND DateInserted >= '{args.sample_cutoff}'
    """
    historical_df = run_query(historical_query)
    
    msg_attrition.append((
        f"SalesIQ: msg IS NOT NULL AND DateInserted >= {args.sample_cutoff}",
        len(historical_df)
    ))

    # Convert column names to snake_case
    historical_df = snake_case_columns(historical_df)

    # Ensure consistent naming for date column
    historical_df = historical_df.rename(columns={"dateinserted": "DateInserted"})

    # Sort by id for any potential ordering requirements
    historical_df = historical_df.sort_values(by="id", ascending=True)

    # Convert message_sender_id to a string
    historical_df["message_sender_id"] = historical_df["message_sender_id"].astype("string")


    # Create a "direction" column
    historical_df["direction"] = historical_df["event"].apply(
        lambda x: "Outgoing" if x == "conversation.operator.replied" else "Incoming"
    )

    # Create a sender column
    historical_df["sender"] = historical_df["direction"].apply(
        lambda x: "Client" if x == "Incoming" else "Counselor"
    )

    # -------------------------------------------------------------------------
    # 3) Read in Stanford employee data and join on message_sender_id
    # -------------------------------------------------------------------------
    emp_data_path = os.path.join(get_log_analysis_root(), "Stanford_Emp_Data.csv")
    print(f"Reading employee data from {emp_data_path}...")
    emp_df = pd.read_csv(emp_data_path)
    emp_df = snake_case_columns(emp_df)
    # Rename "employee_name" to "counsellor"
    emp_df = emp_df.rename(columns={"employee_name": "counsellor"})
    
    # convert salesiq_id to string for consistent merging
    emp_df["salesiq_id"] = emp_df["salesiq_id"].astype("string")

    # Merge employee data with historical data
    historical_df = historical_df.merge(
        emp_df,
        how="left",
        left_on="message_sender_id",
        right_on="salesiq_id"
    )

    # Filter historical data again by sample_cutoff (in case the above merges reintroduced older rows)
    # Ensure "DateInserted" is a proper datetime
    historical_df["DateInserted"] = pd.to_datetime(historical_df["DateInserted"], errors="coerce")
    historical_df = historical_df[historical_df["DateInserted"] >= args.sample_cutoff].copy()
    
    msg_attrition.append((
        f"SalesIQ after merge with emp_df and DateInserted >= {args.sample_cutoff}",
        len(historical_df)
    ))


    # -------------------------------------------------------------------------
    # 4) Merge historical data with the new convo_df
    # -------------------------------------------------------------------------
    if args.non_ai_tables_only:
        print("Skipping load of convo_df")
        # Write historical data to csv
        #TODO: this is nisnamed, it should actually be all_messages or something that includes ai and non-ai messages
        historical_non_ai_csv = os.path.join(get_log_analysis_root(), "historical_non_ai_data/historical.csv")
        historical_df.to_csv(historical_non_ai_csv)
        merged_df = historical_df
    else:
        print("Merging historical messages with the latest convo data...")
        convo_df = snake_case_columns(convo_df)
        
        def normalize_phone(phone):
            if pd.isna(phone):
                return None
            # Convert to string and remove all non-digit characters
            digits = re.sub(r"\D", "", str(phone))
            return digits

        # Make sure phone columns are strings for the merge, remove plus signs if present
        convo_df["patient_phone"] = convo_df["patient_phone"].apply(normalize_phone)
        historical_df["visitor_phone"] = historical_df["visitor_phone"].apply(normalize_phone)
        
        # Strip whitespace from textual messages
        convo_df["true_counselor_response"] = convo_df["true_counselor_response"].dropna().apply(lambda x: x.strip())
        historical_df["message"] = historical_df["message"].dropna().apply(lambda x: x.strip())

        # Write historical data to csv
        # TODO: this is nisnamed, it should actually be all_messages or something that includes ai and non-ai messages
        historical_df.to_csv(os.path.join(get_log_analysis_root(), "historical.csv"))
        
        # Recover AI suggestions that were copied and sent outside the popup
        copied = find_copied_ai_messages(
            convo_df=convo_df,
            historical_df=historical_df,
            time_window="3min",
            bleu_threshold=0.70,
            max_hist_per_ai=5,
            require_ai_text=True,
        )
        
        print(f"Recovered 'copied AI' sends: {len(copied):,}")
        
        # Expect copied to include: openai_response_id, patient_phone, ai_time, ai_suggestion,
        #                           hist_time, hist_message, bleu2, time_delta_sec
        # Keep only what we need for imputation/annotation
        copied_map = copied[[
            "openai_response_id",       # <-- unique key in convo_df
            "hist_message",
            "bleu2",
            "time_delta_sec",
            "ai_time"
        ]].drop_duplicates("openai_response_id").rename(columns={
            "hist_message": "copied_hist_message",
            "bleu2": "copied_bleu2",
            "time_delta_sec": "copied_time_delta_sec",
            "ai_time": "copied_ai_time"
        })
        
        convo_df = convo_df.merge(copied_map, on="openai_response_id", how="left")
        
        convo_df["true_counselor_response"] = convo_df["true_counselor_response"].where(
            convo_df["true_counselor_response"].notna(),
            convo_df["copied_hist_message"]
        )
        
        convo_df["copied_ai_from_popup"] = convo_df["copied_hist_message"].notna()

        # We won't keep 'counsellor' from the right side to avoid collisions
        right_df = convo_df.drop("counsellor", axis=1, errors="ignore")
        
        # Filter out AI messages that were never sent by the counselor
        right_df = right_df[~right_df["true_counselor_response"].isna()].copy()
        
        rm_attrition.append((
            "Result_matrix-derived rows with non-null true_counselor_response (incl copied-from-popup)",
            len(right_df)
        ))

              
        # 1️⃣ Pre-join counts
        print(f"Historical messages: {len(historical_df):,}")
        print(f"AI table messages: {len(right_df):,}")

        # 2️⃣ Phone overlap
        phones_hist = set(historical_df["visitor_phone"].unique())
        phones_ai   = set(right_df["patient_phone"].unique())
        phone_overlap = len(phones_hist & phones_ai)
        print(f"Visitor phones (hist): {len(phones_hist):,}")
        print(f"Patient phones (AI):   {len(phones_ai):,}")
        print(f"Phone overlap:          {phone_overlap:,} ({phone_overlap/len(phones_ai):.1%} of hist phones)")
        
        # If there are phone numbers that don't match, print a warning
        if phone_overlap/len(phones_ai) < 1.0:
            print(f"Warning: only than {phone_overlap/len(phones_ai):.1%} phone number overlap between historical and AI tables. Check phone number formats.")

        # 3️⃣ Exact text overlap
        msg_overlap = (
            historical_df.merge(
                right_df,
                how="inner",
                left_on=["visitor_phone", "message"],
                right_on=["patient_phone", "true_counselor_response"]
            )
        )
        print(f"Exact (phone+message) overlaps: {len(msg_overlap):,} ({len(msg_overlap)/len(right_df):.1%} of historical)")
        
        unmatched_right = right_df.merge(
            historical_df,
            how="left",
            left_on=["patient_phone", "true_counselor_response"],
            right_on=["visitor_phone", "message"],
            indicator=True
        ).query("_merge == 'left_only'")

        print(f"⚠️ AI table messages with NO match: {len(unmatched_right):,} ({len(unmatched_right)/len(right_df):.1%} of AI table)")

        # Merge historical messages to AI suggestions
        # TODO: this should probably join on date as well to be safe
        merged_df = historical_df.merge(
            right_df,
            how="left",
            left_on=["visitor_phone", "message"],
            right_on=["patient_phone", "true_counselor_response"]
        )

    # -------------------------------------------------------------------------
    # 5) Assign conversation IDs to the merged data
    # -------------------------------------------------------------------------
    merged_df = assign_conversations(merged_df)
    
    msg_attrition.append((
        "Merged SalesIQ + AI data after assigning conversations",
        len(merged_df)
    ))

    # -------------------------------------------------------------------------
    # 6) (Optional) Filter out convos with more than one distinct counselor or empty messages
    # -------------------------------------------------------------------------
    print("Filter out convos with more than one distinct counselor")
    merged_df = merged_df.groupby("conversation_id").filter(
        lambda x: x["counsellor"].nunique() == 1
    )
    
    msg_attrition.append((
        "Merged SalesIQ + AI data after dropping multi-counsellor convos",
        len(merged_df)
    ))
    
    merged_df = merged_df[~merged_df["message"].isna() & (merged_df["message"].str.strip() != "")]
    
    msg_attrition.append((
        "Merged SalesIQ + AI data after dropping empty messages",
        len(merged_df)
    ))


    # -------------------------------------------------------------------------
    # 7) Export historical data (no AI tables merged) if required
    # -------------------------------------------------------------------------
    if args.non_ai_tables_only:
        print("Cutting script short at step 9 and just exporting data without AI tables joined")
        current_date = datetime.now().strftime("%Y%m%d")

        # Write historical non-AI data to CSV
        message_csv = os.path.join(get_log_analysis_root(), f"historical_non_ai_data/non_ai_all_message_level_data_{current_date}.csv")
        summary_csv = os.path.join(get_log_analysis_root(), f"historical_non_ai_data/non_ai_conversation_level_data_{current_date}.csv")
        convos_to_review = print_convo_for_review(merged_df, message_csv, summary_csv)

        print(f"Unfiltered message-level CSV saved to {message_csv}")
        print(f"Unfiltered convo-level CSV saved to {summary_csv}")
        return

    # -------------------------------------------------------------------------
    # 8) Compute conversation-level stats for sampling
    # -------------------------------------------------------------------------

    # Summarize
    convo_stats = merged_df.groupby("conversation_id").agg(
        n_messages=("conversation_id", "count"),
        n_ai_messages=("ai_suggestion", lambda x: x.notnull().sum()),
        counsellor=("counsellor", "first")
    ).reset_index()

    # Determine which convos used AI
    convo_stats["ai_convo"] = convo_stats["n_ai_messages"] >= args.min_ai_msgs
    # Convo length in range?
    convo_stats["correct_length"] = convo_stats["n_messages"].between(args.min_convo_length, args.max_convo_length)

    # Filter to correct length
    convo_stats_correct_length = convo_stats[convo_stats["correct_length"]].copy()

    # Merge stats back for filtering
    ai_merged_df_for_review = merged_df.merge(
        convo_stats.drop("counsellor", axis=1),
        on="conversation_id"
    )
    ai_merged_df_for_review = ai_merged_df_for_review[ai_merged_df_for_review["correct_length"]]

    # -------------------------------------------------------------------------
    # 9) Sample AI and non-AI conversations per counselor
    # -------------------------------------------------------------------------
    selected_convos = (
        convo_stats_correct_length
        .groupby(["counsellor", "ai_convo"], group_keys=False)
        .apply(lambda x: x.sample(n=min(args.n_convos_per_counselor, len(x)), random_state=42))
        .reset_index(drop=True)
    )

    ai_merged_df_for_review_by_counselor = ai_merged_df_for_review.merge(
        selected_convos[["conversation_id"]], on="conversation_id"
    )

    # -------------------------------------------------------------------------
    # 10) Create final CSV outputs
    # -------------------------------------------------------------------------
    current_date = datetime.now().strftime("%Y%m%d")

    # (A) CSV with all message-level data
    ai_merged_csv = os.path.join(get_log_analysis_root(), f"historical_data/all_message_level_data_{current_date}.csv")
    merged_df.to_csv(ai_merged_csv, index=False)
    
    for col in merged_df.columns:
        if merged_df[col].dtype == "object":
            merged_df[col] = merged_df[col].astype("string")
            
    merged_df.to_parquet(ai_merged_csv.replace(".csv", ".parquet"), index=False)

    # (B) CSV with the sampled message-level data
    audit_csv_msg_level = os.path.join(get_log_analysis_root(), f"historical_data/message_level_data_sampled_{current_date}.csv")

    # (C) CSV with conversation-level data
    audit_csv_convo_level = os.path.join(get_log_analysis_root(), f"historical_data/conversation_level_data_{current_date}.csv")

    # # This function typically prints out or returns a DataFrame of messages
    # convos_to_review = print_convo_for_review(ai_merged_df_for_review_by_counselor,
    #                                           audit_csv_msg_level,
    #                                           audit_csv_convo_level)

    print(f"All message-level CSV saved to {ai_merged_csv}")
    print(f"Sampled message-level CSV saved to {audit_csv_msg_level}")
    print(f"Sampled convo-level CSV saved to {audit_csv_convo_level}")
    
    # -------------------------------------------------------------------------
    # 11) Print attrition tables
    # -------------------------------------------------------------------------
    msg_attrition_df = pd.DataFrame(msg_attrition, columns=["Step", "N_rows"])
    rm_attrition_df  = pd.DataFrame(rm_attrition,  columns=["Step", "N_rows"])

    # Write attrition tables to a text file
    attrition_path = os.path.join(get_log_analysis_root(), f"attrition_{datetime.now().strftime('%Y%m%d')}.txt")

    with open(attrition_path, "w") as f:

        f.write("=== ATTRITION TABLE: SalesIQ / Messages ===\n")
        f.write(msg_attrition_df.to_string(index=False))
        f.write("\n\n")

        if not args.non_ai_tables_only:
            f.write("=== ATTRITION TABLE: result_matrix / AI ===\n")
            f.write(rm_attrition_df.to_string(index=False))
            f.write("\n")

    print(f"Attrition tables written to {attrition_path}")


if __name__ == "__main__":
    main()
