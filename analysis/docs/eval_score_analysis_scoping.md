# Scoping Document: Analyzing Conversation Eval Scores at Scale

**Status:** Draft
**Owner:** Sarv Salvi
**Last updated:** 2026-06-30

---

## 1. Objective

Pull and analyze the conversation-level evaluation scores for **all** conversations
stored in the production database, to answer two questions:

- **Q1 — Quality.** Does counselor copilot usage improve counseling quality, as
  measured by the rubric eval scores?
- **Q2 — Outcomes.** Do higher eval scores track to a customer-service indicator
  (client satisfaction / retention)?

The hard constraint: **no AI system may read PHI.** The production DB co-locates
eval scores with PHI (conversation text, phone numbers). The analysis must be
designed so that AI-assisted work only ever touches a de-identified, numeric
extract.

---

## 2. What we know about the data (and what we don't)

### 2.1 Eval scores confirmed in the DB

`dbo.convo_scores` — message-level similarity scores, schema confirmed from the
`CREATE TABLE` DDL in `log_analysis/write_convo_scores_to_db.py`:

| column | meaning |
|---|---|
| `openai_response_id`, `patient_phone`, `counselor_id` | linkage keys (PHI: phone) |
| `created_at`, `hash_key` | provenance |
| `bleu_scores`, `bert_precision`, `bert_recall`, `bert_f1` | AI-suggestion-vs-sent similarity |

These measure **adherence to the AI suggestion**, not counseling quality. Not the
primary metric for Q1/Q2, but pull them anyway for a secondary "did they use the
suggestion" signal.

### 2.2 Rubric eval scores — the primary quality metric

The counseling-quality "eval report" is the rubric: `P1–P8, M1–M5, S1–S7, X0–X4,
CR1, CS1, CS2` → `total_score`, `possible_score`, `fractional_score`. These have
been seen as columns in `deployment_full_dataset_20260510.parquet` (66,236
conversations), produced by `make_obs_causal_eval_dataset.R`.

**OPEN QUESTION (owner: Sarv):** Confirm these rubric scores exist as a DB table
and get the exact table + column names. Sarv states they are in the DB but may not
have been pulled yet. Until confirmed, the parquet is the only known location.
Run and paste back (names only, no rows — no PHI):

```sql
SELECT t.name AS table_name, c.name AS column_name, ty.name AS type
FROM sys.columns c
JOIN sys.tables t  ON c.object_id = t.object_id
JOIN sys.types  ty ON c.user_type_id = ty.user_type_id
WHERE c.name IN ('fractional_score','total_score','possible_score',
                 'CR1','CS1','CS2','P1','P2','M1','S1','X0')
ORDER BY t.name, c.name;

SELECT name FROM sys.tables ORDER BY name;   -- full table list
```

### 2.3 Customer-service indicator (Q2 outcome)

- **Chosen outcome:** client satisfaction — `CS1` (satisfied? True/False, LLM-judged),
  `CS2` (1–5 experience rating, LLM-judged); with `CR1` (retention) as a behavioral
  cross-check.
- **No collected client CSAT exists** in any DB pull our code performs (verified by
  grepping every SQL query in the repo; the only `feedback` fields are counselor-on-AI
  popup signals `fm.feedbackflag` / `fm.formbutton`). This has **not** been verified
  against the live DB schema — see open question 2.2's second query.
- **⚠️ Validity threat (must carry through all Q2 reporting):** `CS1`/`CS2` and the
  quality rubric are produced by the **same LLM judge reading the same transcript**.
  Correlating them shares method variance and is partly circular. `CR1` (retention)
  is more behavioral and less circular; report it alongside.

### 2.4 Copilot-usage measure (Q1 treatment)

Already engineered in the parquet / `itt_analysis_2.R`:
`counselor_copilot_msgs_sent_so_far`, `copilot`, `copilot_msgs_sent`,
`counselor_total_msgs_sent_so_far`. Adoption is **staggered** across counselors
over time → staggered difference-in-differences is the right estimator.

---

## 3. PHI trust boundary (the central design constraint)

```
   PRODUCTION DB (PHI)                VM (Sarv runs)            LAPTOP (AI-assisted)
 ┌────────────────────┐   pull    ┌────────────────────┐  scp  ┌──────────────────┐
 │ result_matrix      │ ───────▶  │ join eval scores + │ ────▶ │ numeric parquet  │
 │ form_matrix        │           │ usage + outcomes   │       │ (NO text, NO     │
 │ salesIQ (text!)    │           │ on phone/keys;     │       │  phone, NO names)│
 │ convo_scores       │           │ STRIP all PHI      │       │  → analysis      │
 │ <rubric table?>    │           │ (evidence/justif./ │       │                  │
 └────────────────────┘           │  raw_response/name)│       └──────────────────┘
                                   └────────────────────┘
        AI writes the code for every stage.
        AI EXECUTES only the laptop stage, on the de-identified parquet.
        Sarv executes anything that reads the DB or the PHI-bearing full parquet.
```

**Rules:**
1. AI never connects to the DB and never reads any file containing conversation
   text, phone numbers, employee names, or LLM `*_evidence` / `*_justification` /
   `raw_response` fields.
2. All PHI-dependent joins (e.g., linking scores to outcomes on phone) happen on
   the VM, before de-identification.
3. The hand-off artifact is a single numeric parquet keyed only by opaque
   `conversation_uid` / `counsellor_id` + timestamp. This mirrors the existing
   `trim_dataset.py` pattern.
4. Known PHI-bearing columns to drop: `employee_name`, `CR1_evidence`,
   `CS1_evidence`, `CS2_evidence`, `*_justification`, `raw_response`, any raw
   message/phone columns. `case_categorization` is a controlled vocabulary — keep,
   but review for free-text leakage.

---

## 4. Data pipeline

### Stage A — Extract (VM; Sarv runs; PHI present)
- **A1.** Confirm rubric-score table/columns (open question 2.2).
- **A2.** Pull *all* conversations with eval scores. Extend the existing pull
  (`export_convo_data.py` / `write_convo_scores_to_db.py`) to include the rubric
  table once located. Remove the `tstart > cutoff` filter to get the full history.
- **A3.** Attach copilot-usage counts and the Q2 outcome (`CS1`/`CS2`/`CR1`)
  per conversation, joining on phone/keys inside the VM.
- **A4.** De-identify: keep only numeric rubric cols + usage + outcomes +
  `conversation_uid`, `counsellor_id`, `first_message_time`, `case_categorization`.
  Drop everything in §3 rule 4. (Extend `trim_dataset.py`.)
- **A5.** Emit `deployment_analysis_<YYYYMMDD>.parquet` and transfer to the repo
  `data/` dir. Log row counts / an attrition table (no PHI) for reproducibility.

### Stage B — Analyze (laptop; AI-assisted; no PHI)

The analysis catalog below expands Q1/Q2 into the full set of segmentation and
correlation cuts requested. Every cut runs on the de-identified extract.

**B1. Performance segmentation (descriptive).** Eval-score distributions and
trends, sliced by:
- *Counsellor attributes:* over time (weekly trend), by tenure/experience
  (derive from first-active date), and by proficiency/role (`designation_name`).
- *Case attributes:* SI/NSSI presence (see §4.6), presenting concern
  (`case_categorization`), and other case-summary fields (see §4.6 open items).

**B2. Q1 — copilot usage → quality (causal).** Callaway–Sant'Anna staggered DiD
(reuse `run_did_for_outcomes()` in `itt_analysis_2.R`), outcome = `fractional_score`
(and per-domain P/M/S/X as secondary). First check `manuscript_outputs/` — some of
this may already exist; extend rather than duplicate.

**B3. Eval scores ↔ overreliance (correlational).** *Overreliance is distinct from
copilot access.* Measure it from how little the counsellor changed the AI text:
high `bleu_scores`/`bert_f1` (`convo_scores`) and low edit distance (see
`edit_analysis/`) = high reliance / verbatim send. Correlate reliance with
`fractional_score` to test whether leaning harder on the AI tracks with better or
worse quality. Caveat: adherence is a proxy, not a validated overreliance measure.

**B4. Eval scores ↔ CSR / client outcomes (correlational).** Conversation-level
regression of `CS2` (and `CS1`, `CR1`) on `fractional_score`, with counsellor
random effects + time and presenting-concern controls. **Report the shared-method-
variance caveat (§2.3)** and lead with `CR1` (retention) as the behavioral
cross-check. Optional: mediation (copilot → quality → outcome).

**B5. Coverage & data quality.** How many of *all* conversations actually carry
eval scores (scored subset vs. full population), and how coverage varies over time
and by counsellor — this bounds how far B1–B4 generalize.

### 4.6 SI-detection analysis (net-new; candidate owner: Hanz)

Goal: measure how well **indirect** suicidal ideation is detected, and whether
counsellors (with copilot) **escalate indirect cues to direct assessment**. This
is a buy-in-critical, safety-facing analysis and is treated as a separate track.

**Finding — the current rubric cannot answer this cleanly.** The relevant item is
`high_acuity_risk_assessment` (`counselor_unit_tests.yaml`). Its prompt tells the
LLM judge to consider risks "explicitly *or implicitly* expressed," so indirect
cues are within scope — **but** the output is a *binary counsellor-behavior* score
("did they assess risk?", 1/0/-1), **gated on `X0`** (a binary "was risk present"
LLM judgment; `X1–X4` only count when `X0==1`). Consequences:
- No direct-vs-indirect SI distinction, no severity scale, no validated ground truth.
- Cannot measure *detection recall* for indirect SI, nor *escalation* (indirect→direct).

**Recommended path — a dedicated SI classifier.** Train/apply a RoBERTa-style
classifier with a **3-point scale** (e.g., none / indirect-passive / direct-active)
at the **turn level** on historical (de-identified) conversations, to produce the
graded, per-turn SI labels the rubric lacks. **No such classifier exists in the repo
today** (verified) — this is genuinely new work. With turn-level labels we can then:
- Measure detection sensitivity, especially on the indirect class.
- Detect *escalation sequences* (client gives indirect cue → later direct disclosure /
  counsellor risk assessment) and test whether copilot use raises the escalation rate.

**Sequencing.** Have Hanz complete the historical-data SI classification + this
detection analysis first; it is the prerequisite for — and de-risks — the later
**live SI-detection** work.

**PHI note.** The classifier reads conversation *text* → it is a **Stage-A (VM)**
job. Only its numeric per-turn labels (joined to `conversation_uid`/turn index)
cross the boundary to the analysis extract. The AI may write the classifier code
but does not run it or read its inputs/outputs-with-text.

---

## 5. Deliverables

1. Extended de-id extract script (`trim`-style) — written by AI, run by Sarv.
2. Extended DB pull covering all conversations + rubric table — written by AI,
   run by Sarv.
3. Segmentation + correlation analysis scripts (B1–B5): DiD (Q1), overreliance
   correlation (B3), CSR/client-outcome regression (B4) — AI, run locally.
4. SI-detection track (§4.6): a turn-level SI classifier (3-point) + escalation
   analysis — candidate owner Hanz; classifier runs on the VM.
5. Results tables/figures into `manuscript_outputs/`.
6. This scoping doc, kept current.

---

## 6. Risks & open questions

| # | Item | Owner | Blocks |
|---|---|---|---|
| 1 | Confirm rubric scores are in the DB + exact schema | Sarv | Stage A2 |
| 2 | Confirm whether any collected client CSAT exists | Sarv | Q2 outcome choice |
| 3 | Q2 shared-method variance (CS vs rubric, same judge) | — | Q2 interpretation |
| 4 | Coverage: are eval scores present for *all* convos or a scored subset? | Sarv | Q1/Q2 generalizability |
| 5 | Full-history pull volume/runtime (no cutoff filter) | Sarv | Stage A performance |
| 6 | `case_categorization` free-text leakage review | Sarv | De-id safety |
| 7 | Case-summary fields available for case-attribute segmentation (B1)? | Sarv | B1 case cuts |
| 8 | SI/NSSI flag: derive from rubric `X0`/case data, or wait for §4.6 classifier? | Hanz | B1 SI cut, §4.6 |
| 9 | Overreliance measure: is edit-distance data joinable to eval scores? | Sarv | B3 |
| 10 | Confirm `designation_name`/tenure available and non-PHI in extract | Sarv | B1 counsellor cuts |

---

## 7. Immediate next actions

1. Sarv: run the two schema queries in §2.2; paste back table/column names.
2. AI: once schema known, draft the extended pull + de-id script (Stage A),
   including the segmentation fields for B1 (tenure, `designation_name`,
   case attributes).
3. AI: draft the B1–B4 analysis scripts (segmentation, DiD, overreliance,
   CSR) against the de-identified extract.
4. Hanz: scope the §4.6 SI-detection track — decide 3-point label scheme and
   whether to fine-tune RoBERTa or apply an existing classifier on historical data.
