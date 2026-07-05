# `master_dataset_noaddr.csv` — review notes

Raw file as received from the RA (committed verbatim in `3c3378d`). **Do not edit
the CSV** — record cleaning decisions here / in analysis code instead.

It merges the intake-survey data we've already worked with (`combined-pseudonymized.csv`)
with new kit-dispatch, kit-type, lab, and blood-lead columns.

## Row structure (the "spine")

- **1624 rows, 94 columns.**
- **890 rows = usable respondents**, each with a non-NA `participant_id`
  (all have `usable = TRUE`, `eligible = TRUE`).
- **734 rows = non-usable** (`participant_id` is NA): 353 ineligible, 381
  eligible-but-not-usable. The kit/lab columns are blank for these.
- **500 of the 890 usable respondents were sent a kit** — i.e. `treatment_kit`
  ∈ {C, G, P} is non-NA. The whole kit/lab block is populated only for kit-relevant
  rows, so `is.na(treatment_kit)` is a clean "no kit sent" indicator.

## Consistency with `combined-pseudonymized.csv` — ✅ good

- `participant_id` is a **reliable join key**. Joining on it, **every shared column
  agrees 100%** except `StartDate`, `EndDate`, `RecordedDate`, which are just
  reformatted (same instants, different string format).
- 889 of the usable IDs are common to both files. One ID is unique to each file:
  `eclips2mxs` (new only) and `c2142a2e` (old only) — looks like one participant was
  re-pseudonymized; worth a one-line confirmation from the RA but immaterial to totals.
- The new file has **21 fewer non-usable rows** than `combined-pseudonymized.csv`
  (734 vs 755). Only affects NA-`participant_id` rows; the 890-usable / 500-kit spine
  is unchanged.
- Columns 1–65 match the old file; the new file **drops** the `where_learned_1..6`
  dummies and **adds** the kit/lab block below.

## ⚠️ Data-quality issues to handle in analysis (NOT in the raw file)

1. **Missing coded as `0` (the main issue).** Two flags are stored as numeric with
   **no NAs at all**, so `0` conflates "not applicable" with "didn't do it":
   - `completed_exposure_survey`: 0/1. All 1124 non-kit rows are `0`; among the 500
     kit recipients, 156 = 1 and 344 = 0. The right denominator is kit recipients —
     treat the 1124 non-kit (and ideally non-usable) rows as **NA**, not 0.
   - `valid_sample`: takes values **0 / 1 / 2** (not binary!) with no NA. All 1124
     non-kit rows are `0`. Among kit recipients: 0 = 383, 1 = 21, 2 = 96.
     **Decoded** (cross-checked against `DBSLvalidation`/`DBSRvalidation`):
     `valid_sample` = **number of valid blood spots** (left + right). So among the
     500 recipients — both spots valid = 96, exactly one valid = 21, none valid = 383.
     The `383` zeros conflate two very different cases: **37 returned-but-both-spots-
     insufficient** and **346 never-returned**. Distinguish them with the DBS columns.

2. **Three nested return outcomes (use these, not `valid_sample == 0`):**
   - Returned a kit at all = `!is.na(DBSLvalidation) | !is.na(DBSRvalidation)` = **154**
     (≡ `send_kit_back` non-NA: 153 "yes" + 1 "no").
   - At least one valid spot = `valid_sample >= 1` = **117**.
   - Both spots valid = `valid_sample == 2` = **96**.
   - Returned but unusable (both insufficient) = 154 − 117 = **37** (= `Outcome == "Invalid"`).

3. **Blood-lead numbers exist for only ~28 recipients so far** (`CC-BLC_result L/R`
   non-NA), far fewer than the 117 valid samples — most valid spots are not yet
   analysed (round 2 still arriving). `valid_sample` is *not* "a lab number exists":
   82 rows are valid (=2) with no numeric result yet, 8 have a result at `valid_sample = 0`.

3. **Lab-result columns are character, not numeric**, because of embedded text
   sentinels — these are meaningful, not typos:
   - `CC-BLC_result  R in µmol/L` contains `"no spot"`; `date_analysed` contains
     `"no spots"`; `Lab sample_ID` contains `"twin sample"`; `mix_sent` contains
     `"NO CARD"`.
   - Parse to numeric only after deciding how to treat these (insufficient/no blood
     spot → NA, but keep the reason).

4. **`kit_type = 0` is probably fine** (not missing-as-zero): among the 500 recipients
   it splits 251 (=0) / 249 (=1), matching the planned 50–50 kit randomization. Likely
   the two kit designs coded 0/1 — confirm the labels with the RA.

5. **Kit-only flags.** `disadvantaged` and `address_changed` are logical and NA for the
   1124 non-kit rows — i.e. these are *realized-allocation* variables defined only for
   kit recipients. Note that `disadvantaged` here is the kit-stage flag, not an
   all-respondent attribute.

6. **`send_kit_back`** (kit returned): among 500 recipients, 153 "yes", 1 "no", 346 NA
   (NA ≈ not returned / unknown). Roughly consistent with ~117 `valid_sample > 0`.

## Minor / housekeeping columns

- Two round columns: `round` (lowercase, survey round, shared with old file) **and**
  `Round` (capitalised, kit round) — easy to confuse.
- `comments` and `Column1` are free-text RA QA notes (almost all NA); `Column1` looks
  like a stray column.

## Suggested next step

Ask the RA for a short codebook covering `valid_sample` (0/1/2), `kit_type` (0/1),
`Outcome`, and the lab text flags; then write a cleaning script that (a) sets the
NA-as-zero flags to NA off the kit spine and (b) parses lab results to numeric while
preserving the "no spot" reasons.
