# Audit of analysis code in `~/ECLIPS-slides` (2026-07-04)

Independent audit by a review agent with access to both repos; every number below was recomputed against source data, not read off the slides. Scope: `2026-05-28-Warwick.qmd` (all chunks), `intake-survey-exploration.R`, the local CSV copies, cross-checked against `~/ECLIPS-PAP/data/processed/`. Findings already in `CODE-REVIEW-2026-06-09.md` are not re-reported.

## Findings, ranked by consequence

### 1. Deck reads the stale `combined-pseudonymized.csv`; every "clicked/started" number includes 21 pre-mailing test responses

`2026-05-28-Warwick.qmd:28` (setup chunk) reads `~/ECLIPS-PAP/data/processed/combined-pseudonymized.csv` (1,645 rows) instead of the authoritative `combined-pseudonymized2.csv` (1,624 rows). The 21 extra rows are the study team's own pre-mailing test sessions; they carry valid letter IDs and LSOAs, so they leak into every count of `raw` rows. Verified impact (recomputed both ways):

- **Phase I flowchart** (`:449-452`, hardcoded tikz): "1,645 Started" should be **1,624**; "755 Ineligible or no consent" should be **734**; "148,355 No Response" should be **148,376**; the 54% usable-given-started arrow becomes 54.8% (i.e., "55%").
- **`click-rate-forest`** (`:551`): per-cell click counts inflated by C+5/G+4 (Non-Old), C+1/G+3/P+8 (Old). Old:P shown as 16.7 per 1,000; correct is **16.3**.
- **`click-diff-forest`** (`:582`): Old (G+P)-C shown as **2.67 [0.57, 4.69]** per 1,000; correct is **2.45 [0.36, 4.46]**. Non-Old G-C: 1.37 [0.24, 2.51] becomes 1.40 [0.27, 2.53]. Direction and significance survive, but these are treatment-effect numbers co-authors have seen.
- **`click-imd-by-age`, `click-imd-by-treatment`** (appendix): quintile click counts inflated by 3-6 each.
- All *usable*-based numbers are unaffected — the 890 usable rows are identical in both files, cell-by-cell and quintile-by-quintile (verified).

Fix: change line 28 to `combined-pseudonymized2.csv` and update the four hardcoded flowchart numbers. (`intake-survey-exploration.R:5` reads the stale file too, and its own line 78 comment even notes the test rows need removing.)

### 2. "All disadvantaged got a kit" is not true for Round 2 as stated

`2026-05-28-Warwick.qmd:802` says "All disadvantaged got a kit: 100 kits / 49 disadvantaged (R1); 400 kits / 168 (R2)". The 168 from commit 2c5af9b is verified correct as the count of disadvantaged R2 *kit recipients* (and R1's 49/49 checks out; an independent reconstruction of the disadvantage rule matches the RA's flag exactly for all 500 recipients). But among the 890 usable respondents, Round 2 has **176** disadvantaged, of whom **8 did not receive a kit**. The recipients' `selection_probability` is 1.0 for all disadvantaged, so the 8 were almost certainly removed by the Script 15/16 kit-eligibility review (invalid GP / bounced email) before allocation — meaning the claim is true only of the *kit-eligible* pool. Fix: reword to "All kit-eligible disadvantaged got a kit ... 400 kits / 168 of 176 disadvantaged (R2)" and confirm the 8 exclusions with the RA.

### 3. "Rather not say" is counted as Non-White in the responder-traits slide

`2026-05-28-Warwick.qmd:756` (`responder-traits-imd`): the `Non-White` flag `!is.na(ethnicity) & !str_starts(ethnicity, "White")` sends the 21 usable respondents with ethnicity "Rather not say" into the **Non-White numerator**. This is the opposite direction from review finding E (which covered the `%in%` flags being biased down). Recomputed % Non-White by quintile, shown vs corrected (numerator fix only): Q1 14.3 to 12.2, Q2 15.5 to 15.0, Q3 19.9 to 16.3, Q4 **31.6 to 27.6**, Q5 36.2 to 34.2. Up to 4 pp inflation on a displayed panel; the gradient survives. Fix: add `& ethnicity != "Rather not say"` (and decide whether RNS/NA stay in the denominator, per finding E).

### 4. Kit chunks read the 2026-05-25 master vintage; verified equivalent today, but fragile

Chunks at `:811`, `:873`, `:913` read `2026-05-25-master_dataset_noaddr.csv`. Diffed against the newer `2026-05-27-master_dataset_noaddr.csv` beside it: **every number the deck shows is identical** (500 kits, 251/249 split, returned 79/75, valid 59/58, exposure 82/74, disadvantaged 49/168, per-LSOA kit counts). So no number is wrong. However, the 05-27 vintage renames `kit_type` to `golden_envelope` and recodes `valid_sample` to NA for the 346 never-returned kits; repointing the deck at it without edits would error on `kit_type` and make `valid_sample >= 1` propagate NAs into `sum(hit)`. Worth a comment in the qmd noting why the 05-25 file is pinned. Neither local copy is the PAP repo's broken 6,028-row `master_dataset.csv` (both are the clean 1,624-row spine).

### 5. Returned letters (752, final): denominators are honest, but one slide's text is stale and two per-LSOA graphics would visibly change

Every response-rate axis correctly says "per 1,000 letters **mailed**" — no slide claims a delivered-letter interpretation, so no error. But:

- **`:1076-1080` ("Categorizing letters")** is stale: "A few hundred outreach letters were returned... RA going through all of them; hasn't finished yet." The set is final since 2026-07-04: **752 letters**, fully categorized (`returned-letters.csv` + codebook), heavily clustered in 14 LSOAs (mostly a handful of named buildings; see the codebook).
- **Figures that would visibly change under delivered-letter denominators** (recomputed): the raw per-LSOA map `eb-map-raw` — E01011451 moves 18.7 to **23.0** per 1,000 (18.7% of its letters returned; it would become a near-maximum cell), E01011324 8.0 to 9.7, E01011661 7.1 to 7.6; only 3 of 488 LSOAs shift by more than 0.5 per 1,000. Aggregates barely move: overall 5.93 to 5.96 per 1,000; IMD quintile rates change in the second decimal only (Q5 4.99 to 5.04), so the deprivation-gradient slides are robust. The EB-shrunk map is essentially unchanged.

### 6. Exclusion appendix bullets double-count (minor, appendix)

`:1093-1098` lists exclusions summing to 5,955, but the actual exclusion total is **5,521** (`sampling-frame-codebook.md`): the categories overlap, and "Missing UPRN: 73" is a *subset* of "Missing LSOA: 524" (missing UPRN implies no LSOA lookup), not a separate bucket. 376,205 - 5,521 - 18 duplicates = 370,666 checks out. Also relevant to this slide: the PAP codebook's 2026-07-04 note that the hotel/student-residence keyword list missed ~87 mailed student addresses (Kirkstall Brewery, Lupton Flats) — worth a footnote if this slide is reused.

### 7. Kit mix-up IDs: deck is internally safe; local master copies still carry the unresolved swap (informational)

The deck never joins the intake file to the master file, so the re-pseudonymization (`c2142a2e` in the stale intake file vs `eclips2mxs` in the master and in `combined-pseudonymized2.csv`) creates no orphans or double counts in any deck number. But both local master vintages still record the kit swap raw: `mix_sent` = "06a75dc0" on eclips2mxs's row and "c2142a2e" on 06a75dc0's row, with the returned card sitting on 06a75dc0's row. Total return counts are unaffected; the *covariate attribution* (disadvantaged status etc.) of at most one returned kit in the kit-covariate slides could be swapped between these two people, pending the unresolved mix_sent question. Related data-quality note: the master's lowercase `round` column is NA for **all 327 Round-1 rows** (the NOTES file flags the round/Round confusion but not the NAs); the deck does not use it.

## Verified clean (recomputed, not just read)

- **Wilson CI (`score_ci`) and Newcombe (`score_diff`) implementations** — algebra correct; all usable-rate points/intervals reproduce exactly.
- **Empirical-Bayes machinery** — beta-binomial marginal MLE reproduces (a-hat = 9.1, b-hat = 1523); slide captions "117 of 488 LSOAs at zero", raw range "0-23", posterior range "4-9" all verified; unchanged by the stale-file issue (usable-only).
- **Dedup trap** — no `distinct()`/`unique()`/`duplicated()` anywhere in the qmd or the R script; the 35 duplicate-but-distinct non-usable rows are never collapsed.
- **Factor level strings** — every hand-typed level list in the setup chunk matches the data exactly (0 silently coerced values); `plot_bar`'s NA-to-"Rather not say" folding behaves as documented (bar totals = 890).
- **Joins and denominators** — `addresses-and-treatments.csv` is exactly 150,000 rows; cell denominators 44,406/44,406/20,396x3 correct; all 488 letter LSOAs match the geojson 1:1 in both directions (no silent map drops); zero letters and zero responses lack an IMD match; the `imd_cells` join loses no rows (click totals reconcile to the file's row count).
- **Kit slide numbers** — Phase II flowchart (500, 251/249, 79/75 returned, 59/58 valid, 82/74 exposure, "~56%") all reproduce from the file the deck reads AND from the newer vintage; kit-type randomization balanced within letter x disadvantaged strata; `treatment_kit == treatment_parsed` for all 500; `kit_forest`'s `min_n = 10` never actually drops a displayed level.
- **Other hardcoded numbers** — 370,666; 376,205; "~40%" (40.5%); EPC age missing 38.5%; 18 duplicate UPRNs; R1 kit numbers 100/49 — all verified.
- **Weighting** — no unweighted LSOA-level rate regressions exist in the deck; the two `geom_smooth(lm)` calls are on letter shares and kit counts (the letters one already flagged as mechanical in review finding D; the kits-vs-IMD slope inherits the same design mechanism, worth remembering when presenting).
