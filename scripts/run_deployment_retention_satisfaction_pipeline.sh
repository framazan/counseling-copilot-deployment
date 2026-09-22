#!/bin/bash

set -e

# ── Load OPENAI_API_KEY from .zshrc if not already set ───────────────────────
if [ -z "$OPENAI_API_KEY" ] && [ -f "$HOME/.zshrc" ]; then
  OPENAI_API_KEY=$(grep 'export OPENAI_API_KEY=' "$HOME/.zshrc" | tail -1 | sed 's/export OPENAI_API_KEY=//' | tr -d '"'"'" | tr -d "'")
  export OPENAI_API_KEY
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DATA_DIR="$REPO_ROOT/data"
PROMPTED_DIR="$REPO_ROOT/data/prompted"
COMPLETIONS_DIR="$REPO_ROOT/data/llm_inference/completions"
DATE=$(date +%Y%m%d)
PYTHON="$HOME/.pyenv/versions/3.11.0/bin/python"

export DATA_ROOT="$REPO_ROOT/data/llm_inference"

# Force 'python' to resolve to 3.11.0 regardless of active virtualenv
TMPBIN=$(mktemp -d)
ln -sf "$HOME/.pyenv/versions/3.11.0/bin/python" "$TMPBIN/python"
export PATH="$TMPBIN:$PATH"
export VIRTUAL_ENV=""
unset PYTHONHOME

export PYTHONPATH="$REPO_ROOT/src:$PYTHONPATH"

SOURCE_PARQUET="$REPO_ROOT/deployment_20260921.parquet"
SPLITS=50

# ── Verify input file exists ──────────────────────────────────────────────────
if [ ! -f "$SOURCE_PARQUET" ]; then
  echo "ERROR: Parquet file not found at $SOURCE_PARQUET"
  exit 1
fi

# ── Step 1: Build prompted JSONL datasets directly from parquet ───────────────
echo "=== Step 1: Building prompted datasets ==="

$PYTHON "$REPO_ROOT/src/evals/build_prompted_datasets.py" \
  -f "$SOURCE_PARQUET" \
  -p "$REPO_ROOT/prompts/client_retention" \
  -o "$PROMPTED_DIR" \
  -n deployment_client_retention \
  -fmt messages \
  -c convo \
  --id_column conversation_uid

$PYTHON "$REPO_ROOT/src/evals/build_prompted_datasets.py" \
  -f "$SOURCE_PARQUET" \
  -p "$REPO_ROOT/prompts/client_satisfaction" \
  -o "$PROMPTED_DIR" \
  -n deployment_client_satisfaction \
  -fmt messages \
  -c convo \
  --id_column conversation_uid

# ── Step 2: Run LLM inference ─────────────────────────────────────────────────
echo "=== Step 2: Running LLM inference ==="

check_line_count() {
  local label="$1"
  local input_jsonl="$2"
  local output_jsonl="$3"
  local expected
  local actual
  expected=$(wc -l < "$input_jsonl")
  actual=$(wc -l < "$output_jsonl")
  if [ "$actual" -ne "$expected" ]; then
    echo "ERROR: $label completions incomplete — expected $expected rows, got $actual."
    echo "  Check split logs in $COMPLETIONS_DIR for failed batches."
    exit 1
  fi
  echo "  $label: $actual / $expected rows complete."
}

bash "$SCRIPT_DIR/run_llm_client.sh" \
  "$PROMPTED_DIR/deployment_client_retention_${DATE}.jsonl" \
  "gpt-4.1" "openai-batch" $SPLITS 500 \
  "https://api.openai.com/v1/chat/completions"

CR_COMPLETIONS=$(ls -t "$COMPLETIONS_DIR"/final_merged_gpt-4.1_max500_*.jsonl 2>/dev/null | head -1)
check_line_count "CR1" "$PROMPTED_DIR/deployment_client_retention_${DATE}.jsonl" "$CR_COMPLETIONS"

bash "$SCRIPT_DIR/run_llm_client.sh" \
  "$PROMPTED_DIR/deployment_client_satisfaction_${DATE}.jsonl" \
  "gpt-4.1" "openai-batch" $SPLITS 500 \
  "https://api.openai.com/v1/chat/completions"

CS_COMPLETIONS=$(ls -t "$COMPLETIONS_DIR"/final_merged_gpt-4.1_max500_*.jsonl 2>/dev/null | head -1)
check_line_count "CS1/CS2" "$PROMPTED_DIR/deployment_client_satisfaction_${DATE}.jsonl" "$CS_COMPLETIONS"

# ── Step 3: Parse completions and write updated parquet ──────────────────────
echo "=== Step 3: Parsing results and writing updated parquet ==="

if [ -z "$CR_COMPLETIONS" ]; then
  echo "ERROR: No client retention completions file found in $COMPLETIONS_DIR"
  exit 1
fi
if [ -z "$CS_COMPLETIONS" ]; then
  echo "ERROR: No client satisfaction completions file found in $COMPLETIONS_DIR"
  exit 1
fi

$PYTHON "$REPO_ROOT/src/evals/parse_deployment_retention_satisfaction_results.py" \
  --cr_completions "$CR_COMPLETIONS" \
  --cs_completions "$CS_COMPLETIONS"

echo "=== Pipeline complete ==="
echo "Results saved to:"
echo "  $DATA_DIR/deployment_full_dataset_20260921.parquet"
