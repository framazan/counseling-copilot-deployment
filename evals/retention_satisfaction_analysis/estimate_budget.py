import os
import sys
import subprocess
from pathlib import Path
import json
import tiktoken

# Ensure tiktoken uses the correct encoding for gpt-4.1
try:
    enc = tiktoken.get_encoding("cl100k_base")
except Exception:
    import tiktoken_ext.openai_public
    enc = tiktoken.get_encoding("cl100k_base")

# ==========================================
# USER CONFIGURABLE PRICING (Batch API)
# ==========================================
# Prices per 1,000,000 tokens (Using gpt-4o batch API pricing as placeholder)
GPT_4_1_INPUT_COST_PER_1M = 2.50
GPT_4_1_OUTPUT_COST_PER_1M = 10.00
# Estimated output tokens per conversation (JSON rubric responses are short)
EST_OUTPUT_TOKENS = 150

# Set paths
SCRIPT_DIR = Path(__file__).parent
REPO_ROOT = SCRIPT_DIR.parent.parent
PROMPTED_DIR = REPO_ROOT / "evals" / "data" / "prompted"
SOURCE_PARQUET = REPO_ROOT / "deployment_full_dataset_20260921.parquet"
BUILDER_SCRIPT = REPO_ROOT / "evals" / "static_evals" / "build_prompted_datasets.py"

def generate_prompts():
    """Run the build_prompted_datasets.py script to generate JSONL files."""
    print("Generating prompts for CR1 and CS1/CS2...")
    
    # Run CR1
    subprocess.run([
        sys.executable, str(BUILDER_SCRIPT),
        "-f", str(SOURCE_PARQUET),
        "-p", str(SCRIPT_DIR / "prompts" / "client_retention"),
        "-o", str(PROMPTED_DIR),
        "-n", "estimate_client_retention",
        "-fmt", "messages",
        "-c", "convo",
        "--id_column", "conversation_uid"
    ], check=True)
    
    # Run CS1/CS2
    subprocess.run([
        sys.executable, str(BUILDER_SCRIPT),
        "-f", str(SOURCE_PARQUET),
        "-p", str(SCRIPT_DIR / "prompts" / "client_satisfaction"),
        "-o", str(PROMPTED_DIR),
        "-n", "estimate_client_satisfaction",
        "-fmt", "messages",
        "-c", "convo",
        "--id_column", "conversation_uid"
    ], check=True)

def count_tokens_in_jsonl(jsonl_path):
    """Count input tokens in a JSONL file containing formatted prompts."""
    total_tokens = 0
    total_conversations = 0
    with open(jsonl_path, "r") as f:
        for line in f:
            if not line.strip():
                continue
            data = json.loads(line)
            # Find the actual text content being sent
            content = ""
            if "messages" in data:
                content = " ".join([m.get("content", "") for m in data["messages"]])
            elif "prompt" in data:
                content = data["prompt"]
            
            total_tokens += len(enc.encode(content))
            total_conversations += 1
            
    return total_tokens, total_conversations

def main():
    if not SOURCE_PARQUET.exists():
        print(f"Error: Could not find {SOURCE_PARQUET}")
        sys.exit(1)
        
    generate_prompts()
    
    print("\nCounting tokens...")
    # Find the newly generated files
    cr_files = list(PROMPTED_DIR.glob("estimate_client_retention_*.jsonl"))
    cs_files = list(PROMPTED_DIR.glob("estimate_client_satisfaction_*.jsonl"))
    
    if not cr_files or not cs_files:
        print("Error: Prompt files were not generated.")
        sys.exit(1)
        
    # Get the latest if multiple exist
    cr_file = max(cr_files, key=os.path.getctime)
    cs_file = max(cs_files, key=os.path.getctime)
    
    cr_tokens, cr_count = count_tokens_in_jsonl(cr_file)
    cs_tokens, cs_count = count_tokens_in_jsonl(cs_file)
    
    total_input_tokens = cr_tokens + cs_tokens
    total_convos = cr_count + cs_count
    total_est_output_tokens = total_convos * EST_OUTPUT_TOKENS
    
    input_cost = (total_input_tokens / 1_000_000) * GPT_4_1_INPUT_COST_PER_1M
    output_cost = (total_est_output_tokens / 1_000_000) * GPT_4_1_OUTPUT_COST_PER_1M
    total_cost = input_cost + output_cost
    
    print("\n" + "="*50)
    print("API BUDGET ESTIMATION (gpt-4.1 Batch API)")
    print("="*50)
    print(f"Total Conversations Evaluated : {total_convos:,} ({cr_count:,} CR1, {cs_count:,} CS1/CS2)")
    print(f"Total Input Tokens Analyzed   : {total_input_tokens:,}")
    print(f"Estimated Output Tokens       : {total_est_output_tokens:,} (Assuming {EST_OUTPUT_TOKENS} tkns/convo)")
    print("-" * 50)
    print(f"Estimated Input Cost          : ${input_cost:,.2f}")
    print(f"Estimated Output Cost         : ${output_cost:,.2f}")
    print(f"TOTAL ESTIMATED BUDGET        : ${total_cost:,.2f}")
    print("="*50)
    print("\n* You can adjust the exact pricing per 1M tokens by editing the variables at the top of this script.")

if __name__ == "__main__":
    main()
