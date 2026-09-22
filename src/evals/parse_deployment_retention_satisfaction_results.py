import sys
import json
import re
import argparse
import pandas as pd
from pathlib import Path

sys.path.append(str(Path(__file__).parent.parent.parent))
from src.evals.extract_content_from_llm_completion import parse_jsonl

DATA_DIR = Path(__file__).parent / "data"
PARQUET_PATH = Path(__file__).parent.parent.parent / "deployment_20260921.parquet"

METADATA_COLS = [
    "conversation_uid",
    "convo",
    "ai_used",
    "ai_message_count",
    "n_messages",
    "n_counselor_messages",
    "n_client_messages",
    "copilot",
    "employee_name",
    "designation_name",
    "convo_duration_mins",
    "avg_counselor_response_mins",
    "fractional_score",
    "first_message_time",
    "last_message_time",
    "is_multitasking",
    "num_simultaneous_convos",
]


def strip_markdown_fences(text: str) -> str:
    return re.sub(r"^```(?:json)?\s*|\s*```$", "", text.strip(), flags=re.MULTILINE)


def parse_json_column(df: pd.DataFrame) -> pd.DataFrame:
    parsed_rows = []
    for _, row in df.iterrows():
        base = {"uid": row["uid"]}
        try:
            raw = strip_markdown_fences(str(row["llm_eval_json"]))
            data = json.loads(raw)
            for key, val in data.items():
                if isinstance(val, list):
                    base[key] = " | ".join(str(v) for v in val)
                else:
                    base[key] = val
        except (json.JSONDecodeError, KeyError, TypeError):
            base["parse_error"] = True
        parsed_rows.append(base)
    return pd.DataFrame(parsed_rows)


def load_and_parse(completions_path: str, label: str) -> pd.DataFrame:
    raw = parse_jsonl(completions_path, output_path=None, content_name="llm_eval_json", write_to_file=False)
    parsed = parse_json_column(raw)
    # Drop duplicate uids — can occur when retry completions overlap with the original
    n_before = len(parsed)
    parsed = parsed.drop_duplicates(subset="uid", keep="last")
    if len(parsed) < n_before:
        print(f"  {label}: dropped {n_before - len(parsed)} duplicate uid(s)")
    print(f"  {label}: {len(parsed)} rows parsed")
    if "parse_error" in parsed.columns:
        n_errors = int(parsed["parse_error"].sum())
        print(f"  {label} parse errors: {n_errors} ({100*n_errors/len(parsed):.1f}%)")
    return parsed


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--cr_completions", required=True, help="Path to CR1 merged completions JSONL")
    parser.add_argument("--cs_completions", required=True, help="Path to CS1/CS2 merged completions JSONL")
    args = parser.parse_args()

    print("Loading full deployment parquet...")
    full_parquet = pd.read_parquet(PARQUET_PATH)
    full_parquet = full_parquet.rename(columns={"conversation_uid": "uid"})
    print(f"  Parquet: {len(full_parquet)} rows, {len(full_parquet.columns)} columns")

    print("\nParsing completions...")
    cr_parsed = load_and_parse(args.cr_completions, "CR1")
    cs_parsed = load_and_parse(args.cs_completions, "CS1/CS2")

    # Merge CR1 + CS1/CS2 on uid
    cr_cols = [c for c in cr_parsed.columns if c.startswith("CR") or c == "uid" or c == "parse_error"]
    cs_cols = [c for c in cs_parsed.columns if c.startswith("CS") or c == "uid" or c == "parse_error"]

    engagement = cr_parsed[cr_cols].merge(
        cs_parsed[[c for c in cs_cols if c != "parse_error"]],
        on="uid",
        how="outer",
        suffixes=("_cr", "_cs"),
    )

    # ── Write updated parquet ─────────────────────────────────────────────────
    llm_cols = [c for c in engagement.columns if c != "parse_error"]
    updated_parquet = full_parquet.merge(engagement[llm_cols], on="uid", how="left")
    updated_parquet = updated_parquet.rename(columns={"uid": "conversation_uid"})

    # CR1/CS1 arrive as strings ("true"/"false"/"NA"/NaN) — store as string so
    # pyarrow doesn't attempt boolean inference and choke on "NA" values.
    for col in ["CR1", "CS1"]:
        if col in updated_parquet.columns:
            updated_parquet[col] = updated_parquet[col].astype(str).replace("nan", None)

    parquet_out = DATA_DIR / "deployment_full_dataset_20260921.parquet"
    updated_parquet.to_parquet(parquet_out, index=False)
    print(f"\nUpdated parquet: {len(updated_parquet)} rows, {len(updated_parquet.columns)} cols -> {parquet_out}")
    new_cols = [c for c in updated_parquet.columns if c not in full_parquet.rename(columns={"uid": "conversation_uid"}).columns]
    print(f"  New columns added: {new_cols}")

    # Summary stats
    print("\n=== Summary Statistics ===")
    for metric, col in [("CR1", updated_parquet.get("CR1")), ("CS1", updated_parquet.get("CS1"))]:
        if col is not None:
            print(f"\n{metric} distribution:")
            print(col.value_counts(dropna=False))

    if "CS2" in updated_parquet.columns:
        cs2 = pd.to_numeric(updated_parquet["CS2"], errors="coerce")
        print(f"\nCS2 distribution (n={cs2.notna().sum()}):")
        print(cs2.value_counts().sort_index())
        print(f"  Mean: {cs2.mean():.2f}  Median: {cs2.median():.1f}  Std: {cs2.std():.2f}")
