import os
import pyodbc
import pandas as pd
from dotenv import load_dotenv
from datetime import datetime
import hashlib
import nltk
from nltk.translate.bleu_score import sentence_bleu, SmoothingFunction
from evaluate import load

try:
    nltk.data.find('tokenizers/punkt_tab')
    print("punkt_tab is already downloaded")
except LookupError:
    print("Downloading punkt_tab...")
    nltk.download('punkt_tab')
    print("punkt_tab downloaded successfully")

def get_log_analysis_root():
    """Get the log_analysis root folder."""
    if os.path.exists("/datadrive/vf_copilot"):
        return "/datadrive/vf_copilot/log_analysis"
    elif os.path.exists("/home/stanford/vf_copilot"):
        return "/home/stanford/vf_copilot/log_analysis"

def get_most_recent_convo_file():
    """Find the most recent convo_df file in log_analysis/convo_dfs."""
    folder = os.path.join(get_log_analysis_root(), "convo_dfs")
    if not os.path.exists(folder):
        return None

    files = [f for f in os.listdir(folder) if f.startswith("convo_df_") and f.endswith(".csv")]
    if not files:
        return None

    # Sort files by date descending (the most recent date first)
    files.sort(reverse=True)
    return os.path.join(folder, files[0])

def load_historical_scores():
    """Load scores from the most recent convo_df file."""
    file_path = get_most_recent_convo_file()
    if file_path:
        print(f"Loading historical scores from {file_path}")
        return pd.read_csv(file_path)
    # If no file found, return an empty DataFrame (no historical scores)
    cols = ["hash_key", "bleu_scores", "bert_precision", "bert_recall", "bert_f1"]
    return pd.DataFrame(columns=cols)

# Load BERTScore once
bertscore = load("bertscore")

def get_database_connection():
    """Establish a connection to the SQL database."""
    load_dotenv()

    server = 'tcp:gptdata.database.windows.net,1433'
    database = 'aidata'
    username = 'chatgpt'
    password = os.getenv("PROD_DB_PWD")
    driver = '{ODBC Driver 18 for SQL Server}'

    connection_string = (
        f"DRIVER={driver};SERVER={server};DATABASE={database};UID={username};PWD={password};"
    )

    return pyodbc.connect(connection_string)

def fetch_convo_df(conn):
    """Fetch conversation data from the database and return as a DataFrame."""
    query = """
    SELECT 
        rm.openAiId as openai_response_id,
        rm.phone AS patient_phone,
        fm.operatorId AS counselor_id,
        fm.responseSent AS true_counselor_response,
        fm.feedbackflag as feedback_flag,
        fm.formbutton as feedback_action,
        rm.aisuggestion AS ai_suggestion,
        rm.prompt AS initial_prompt,
        rm.conversation AS convo_history,
        rm.openaiFirst AS initial_ai_suggestion,
        rm.advprompt AS advanced_prompt,
        rm.advtext AS advanced_text,
        rm.tstart AS generation_start_time,
        rm.tend AS generation_end_time,
        fm.tsent AS response_sent_time
    FROM 
        dbo.result_matrix rm
    LEFT JOIN 
        dbo.form_matrix fm 
    ON 
        rm.openAiId = fm.openAiId AND rm.phone = fm.phone
    WHERE
        rm.tstart > '2025-01-09'
    ORDER BY 
        patient_phone, counselor_id, generation_end_time;
    """
    # For PyODBC, this usage is acceptable:
    df = pd.read_sql(query, conn)
    return df

def compute_bleu(reference, candidate):
    """Compute BLEU score for a given reference and candidate."""
    # Safely handle None/empty strings
    if not reference or not candidate:
        return None

    ref_text = reference.strip().lower()
    cand_text = candidate.strip().lower()
    if not ref_text or not cand_text:
        return None

    ref_tokens = [nltk.word_tokenize(ref_text)]
    cand_tokens = nltk.word_tokenize(cand_text)
    return sentence_bleu(ref_tokens, cand_tokens, smoothing_function=SmoothingFunction().method1)

def compute_bertscore(predictions, references, model_type='distilbert-base-uncased'):
    """Compute BERTScore for given predictions and references (lists)."""
    # If any references/predictions are None or empty, replace them with "" to avoid errors.
    safe_predictions = [p if p else "" for p in predictions]
    safe_references = [r if r else "" for r in references]

    results = bertscore.compute(
        predictions=safe_predictions,
        references=safe_references,
        model_type=model_type
    )
    return results['precision'], results['recall'], results['f1']

def save_convo_df(df):
    """Save the DataFrame to a CSV file with a timestamp."""
    folder = os.path.join(get_log_analysis_root(), "convo_dfs")
    os.makedirs(folder, exist_ok=True)
    date_suffix = datetime.now().strftime("%Y%m%d")
    file_path = os.path.join(folder,f"convo_df_{date_suffix}.csv")
    df.to_csv(file_path, index=False)
    print(f"Saved conversation data to {file_path}")

def calculate_scores(convo_df):
    """Calculate BLEU and BERT scores, using cached results where possible."""
    historical_scores = load_historical_scores()

    required_cols = ["hash_key", "ai_suggestion", "true_counselor_response", 
                     "bleu_scores", "bert_precision", "bert_recall", "bert_f1"]
    for col in required_cols:
        if col not in historical_scores.columns:
            historical_scores[col] = None

    def make_hash_key(row):
        ai_sug = str(row['ai_suggestion']) if pd.notna(row['ai_suggestion']) else ""
        true_resp = str(row['true_counselor_response']) if pd.notna(row['true_counselor_response']) else ""
        return hashlib.sha256((ai_sug + true_resp).encode('utf-8')).hexdigest()

    convo_df['hash_key'] = convo_df.apply(make_hash_key, axis=1)
    merged_df = convo_df.merge(historical_scores[['hash_key', 'bleu_scores','bert_precision', 
                                                  'bert_recall', 'bert_f1']], on='hash_key', how='left')
    
    merged_df = merged_df[~merged_df.ai_suggestion.isna()]
    
    missing_scores = merged_df[merged_df['bleu_scores'].isna()].copy()
    if not missing_scores.empty:
        print(f"Computing scores for {len(missing_scores)} new entries...")
        
        print(missing_scores.columns)

        valid_missing_mask = (
            missing_scores['ai_suggestion'].fillna("").str.strip() != ""
        ) & (
            missing_scores['true_counselor_response'].notna() & (missing_scores['true_counselor_response'].str.strip() != "")
        )
        valid_missing = missing_scores[valid_missing_mask].copy()

        # If strings are identical, assign all scores as 1
        identical_mask = valid_missing['ai_suggestion'] == valid_missing['true_counselor_response']
        valid_missing.loc[identical_mask, ['bleu_scores', 'bert_precision', 'bert_recall', 'bert_f1']] = 1.0

        # Compute scores only for non-identical cases
        non_identical = valid_missing[~identical_mask]
        if not non_identical.empty:
            non_identical['bleu_scores'] = non_identical.apply(
                lambda row: compute_bleu(row['true_counselor_response'], row['ai_suggestion']),
                axis=1
            )
            precision, recall, f1 = compute_bertscore(
                non_identical['ai_suggestion'].tolist(),
                non_identical['true_counselor_response'].tolist()
            )
            non_identical['bert_precision'] = precision
            non_identical['bert_recall'] = recall
            non_identical['bert_f1'] = f1
            valid_missing.update(non_identical)

        for col in ["bleu_scores", "bert_precision", "bert_recall", "bert_f1"]:
            missing_scores.loc[valid_missing.index, col] = valid_missing[col]

        new_scores = missing_scores[['hash_key', 'bleu_scores', 'bert_precision', 'bert_recall', 'bert_f1']]
        updated_historical = pd.concat([historical_scores, new_scores]).drop_duplicates(subset=["hash_key"], keep="last")
        save_convo_df(updated_historical)
        merged_df.update(missing_scores)
    
    return merged_df

def main():
    """Main function to fetch, compute scores, and save conversation data."""
    conn = get_database_connection()
    df = fetch_convo_df(conn)
    conn.close()

    df = calculate_scores(df)

    # Finally, save the entire DataFrame (with the newly computed scores) as well.
    save_convo_df(df)

if __name__ == "__main__":
    main()
