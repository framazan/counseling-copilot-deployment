# Are audit scores a fair measure of counsellor performance?

**Findings report** · Owner: Sarv Salvi · 2026-08-20
**Data:** 66,240 scored conversations · 543 counsellors · deployment set 2026-05-10
**Companion:** interactive floor tool `manuscript_outputs/message_floor_tool.html`
**Scripts:** `message_floor_aggregate.py`, `counsellor_score_validity_preview.py`,
`counsellor_score_reliability.py`, `counsellor_dimension_behavior.py`, `generate_analysis_figures.py`

> **Caveat.** All results use the **old 23-item rubric prompt**.

We want to share audit scores back to the foundation as a signal of counsellor performance. This
report asks whether that is valid, and under what constraints. Four findings.

---

## Finding 1 — Raw scores are confounded by conversation length

Shorter conversations score much lower, and the effect is **mechanical**: many rubric items can only
be demonstrated once a conversation reaches some depth. Mean `fractional_score` rises monotonically
with length (Spearman +0.38).

![Length distribution and score-vs-length trend](manuscript_outputs/fig1_length_score.png)

| Length (messages) | 20 | 21–25 | 26–30 | 31–40 | 41–60 | 60+ |
|---|---|---|---|---|---|---|
| Mean score | 0.64 | 0.68 | 0.72 | 0.79 | 0.87 | 0.92 |
| Share of convos | 1% | 7% | 6% | 12% | 23% | 51% |

The driver is length-gated items: laddered risk assessment (met-rate 0.02→0.53 short→long),
protective-factors-within-10 (0.14→0.88), social-support check (0.12→0.81), agenda-setting
(0.22→0.84). Tone/mechanics items (professional tone, one-question-per-message, no-medical-advice)
are flat. Short conversations mostly *miss* items for lack of opportunity, not poor counselling —
so the **absolute** score understates quality and is not comparable across sessions of different
length. The scoring set is already floored at 20 messages, so the true short-session penalty is even
steeper than shown.

---

## Finding 2 — Random case allocation neutralises the confound for *ranking*

The length confound operates almost entirely **within** counsellors (case to case), not **between**
them — because cases are assigned roughly independently of counsellor skill. Evidence:

| Test | Result | Interpretation |
|---|---|---|
| corr(counsellor mean **score**, counsellor mean **length**) | **0.008** | length does not track who scores well |
| Spearman(raw counsellor rank, length-adjusted rank) | **0.919** | rankings barely move when length is removed |
| Between-counsellor ICC of **client** messages | 0.059 | clients aren't systematically longer for anyone |
| corr(counsellor experience, case length / risk) | −0.08 / +0.02 | no skill-based triage |
| Between-counsellor ICC of **score** | 0.339 | a third of score variance is the counsellor (real signal) |

Because each counsellor's ~56 conversations average out their length mix, the confound does **not**
bias between-counsellor comparison. **Conclusion: the scores are valid as a *relative ranking* today**,
even though the absolute values are deflated (Finding 1). Counsellor message volume *does* differ
between counsellors (ICC 0.20) — a legitimate style/skill choice, not a bias to remove.

---

## Finding 3 — A counsellor mean needs ~20 conversations to be reliable

New counsellors with few conversations get unstable averages — and by chance may be weighted toward
short conversations. Both concerns resolve to a **minimum-N rule**, and the instability is mostly a
sample-size problem, not a length problem.

![Reliability vs N, and length-luck shrinking with N](manuscript_outputs/fig2_reliability.png)

| Conversations behind the average | 5 | 10 | 20 | 30 | 50 |
|---|---|---|---|---|---|
| Reliability (empirical) | 0.55 | 0.72 | **0.83** | 0.89 | 0.93 |
| 90% CI on the score | ±9 pts | ±8 pts | **±6 pts** | ±5 pts | ±4 pts |

Score drift from **length luck alone** is ±5–6 pts at N=5 but only **±3 pts by N=20**, and length is
just **16%** of case-to-case noise — so even a perfect opportunity-fix would not rescue a small
sample. The lever is volume. **Rule: publish an individual average only at N ≥ 20 (10–19 provisional,
<10 suppress), always with N and a 90% CI, and prefer tiers over precise ranks.**

---

## Finding 4 — Dimension scores behave like the overall score, except Safety

Each dimension (Productivity, Micro-skills, Style & tone, Safety) is positively length-confounded,
reliable at N≈20, and ranks counsellors broadly like the overall score (0.80–0.93) — so the same
rules extend to each. They differ in degree, and **Safety is a special case.**

![Score vs length by dimension](manuscript_outputs/fig3_dimensions.png)

| Dimension | Mean | Length swing (60+ − 20-24) | Reliability @N=20 | Ranks like overall |
|---|---|---|---|---|
| Style & tone | 0.92 | **0.13 (cleanest)** | 0.92 | 0.87 |
| Micro-skills | 0.89 | 0.31 | 0.89 | 0.88 |
| Productivity | 0.84 | **0.37 (most)** | 0.90 | 0.93 |
| Safety (risk cases only) | 0.64 | **0.55 (huge)** | 0.86\* | 0.80 |

**Style & tone** is the least length-confounded (most trustworthy on an absolute basis).
**Productivity** is the most confounded of the always-applicable dimensions (the opportunity-fix
matters most here). **Safety is not a normal dimension:** its items are *not applicable* to the 65% of
conversations with no risk, so it is defined only on the **35% risk-present** cases and must be scored
on those alone. \*A reliable Safety score therefore needs ~20 **risk** cases ≈ **55 total
conversations** — three times the others; suppress per-counsellor Safety until enough risk cases
accrue. Dimensions rank-correlate only 0.80–0.93 with the overall score, so the breakdown adds real
signal (most for Safety).

---

## What this means (recommendations)

- **A. Session floor = 20 messages** (length is ~random across counsellors, so 20 vs 30 changes only
  how many convos are excluded, not ranking fairness). Set `--min-messages 20` in the cron — and name
  it distinctly from the N≥20 *average* gate in D; they are two different thresholds that share a number.
- **B. Interpret and present scores as a RANKING**, not an absolute grade, until the opportunity-fix
  lands.
    - *↳ Product & implementation:* this shapes **mentor training** and **table design**. Mentors must
      be coached to read a score as a position relative to peers, not a pass/fail grade — and the Zoho
      table should therefore surface a **tier or rank (with N and CI), not a bare absolute number**.
      Which of tiers / percentile rank / absolute-with-band we show is an open design choice to settle
      before rollout.
- **C. Improve the prompt to score for opportunity** (`na` when a behavior had no chance to appear) —
  neither the old prompt nor the current `audit_score_v2` does this today — and upgrade audit reports to
  reflect it. This un-deflates absolute single-session scores.
    - *↳ Product & implementation:* **session-level** outputs depend on this. The per-session score in
      the training tool and the session-level audit reports are only useful and accurate once
      no-opportunity items are scored `na` rather than `missed` — until then, single-session numbers and
      audit narratives will understate short sessions. (Cross-counsellor rankings, per Finding 2, are
      already fine without it; this fix is specifically what makes single-session review trustworthy.)
- **D. Gate averages at N ≥ 20 conversations; Safety at N ≥ 20 risk-present cases**, over the risk
  subset only. Always display N and a 90% CI. (Risk trigger X0 is already captured in
  `counselor_eval_scores_v2`; validate its classification accuracy, since the Safety gate depends on it.)
    - *↳ Product & implementation:* send **Akhilesh** the per-counsellor **count of conversations with
      `risk_trigger = 1`**, and use that count as the condition (≥ 20) before any average Safety/risk
      score is displayed. This requires the risk-trigger flag to flow into the reporting layer alongside
      the scores, not just live in `counselor_eval_scores_v2`.

---

## Assumptions, limitations & further work

**Assumptions/limitations**

1. **Prompt.** Findings are on the old-rubric deployment scores; the new `audit_score_v2` prompt (cron
   target) is not yet validated at scale (small sample) and is **not** opportunity-aware either. The
   opportunity-fix (rec C) is a proposed change to the production prompt; its impact is projected, not
   measured.
2. **Random allocation** is tested (Finding 2) and holds in aggregate, but must be *monitored* and can
   fail for individuals (a short-/low-risk-queue counsellor). Not yet checked at the individual level.
3. Length adjustment here is **marginal** (subtract the length-bin mean); a full multilevel model
   (score ~ spline(length) + case-mix + shift + (1|counsellor)) would tighten the estimate.

**Open questions to resolve before the final data-sharing / Zoho-visualization proposal** (these are
the design decisions, not settled findings):

- **(2) NA-aware averaging.** Zoho must exclude `na`/no-opportunity items from the denominator, never
  treat them as 0, and store items three-state (`met`/`missed`/`na`) — or the opportunity-fix is
  silently undone at the table.
- **(3) How to convey uncertainty.** N + CI on every average; hard N-gate vs empirical-Bayes shrinkage;
  tiers/quartiles vs precise ranks.
- **(7) Ranking reference frame.** Rank against the full cohort vs tenure-matched peers; and add a
  **within-counsellor over-time** view (fair, sidesteps cross-counsellor confounds).
- **(8) Mentor/VF guidance.** A "how to read these scores" guide (ranking not absolute, N/CI, Safety
  caveat, conversation-starter not verdict) and a feedback/appeals loop.

**Further analysis**

- Phase 1e: identify counsellors on systematically short / low-risk queues (individual-level fairness).
- Simulate the opportunity-fix on existing data (slope flattening, reliability gain, ranking stability)
  before rebuilding the prompt; then a current-vs-new prompt A/B on a held-out sample.
