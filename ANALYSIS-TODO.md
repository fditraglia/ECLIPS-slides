# Analysis to-do list

Consolidated from Ludovica's slide feedback (#general, 2026-05-28, three posts), Frank's annotated re-post (2026-06-03, ts 1780496380.829809), and the items added by the 2026-07-04 investigation session. Organized by theme. Task numbers 1-14 are stable so they can be referred to across Slack and the slides.

**Refreshed 2026-07-10.** The earlier version grouped tasks by which data-production step they were waiting on (Caner re-running Scripts 09/14, Caner running Script 25, the fixed master dataset, receiving the blood lead levels). Those steps have all completed, so the tasks are reorganized by theme below. What each former blocker produced, and its state in ECLIPS-PAP as of this refresh:

- Scripts 09/14 re-run: `data/processed/combined-pseudonymized.csv` regenerated 2026-07-06; the drop-off fields (Progress, Finished, Duration, the twelve page-timer columns) are present in `master_dataset.csv`.
- Script 25: `data/processed/oa-response-counts.csv` committed 2026-07-06 (`a985daa`).
- Master dataset: `data/processed/master_dataset.csv` present; the merge row-explosion fix and the dropping of `mix_sent` are in (`3e7eb00`, `c721fe3`).
- Blood lead levels: arriving. 154 rows carry a `CC-BLC_result` (left/right, µmol/L) and a `date_sampled` as of 2026-07-10; 2 rows carry a `venous_BLC`. The full set is pending the remaining kit returns; item 10 is the analysis that sets the close date for that window.

The old Lasse adaptive-sampling simulation work (2025-03 DM thread) is from the design phase, predates the study going live, and is not part of this backlog; Lasse is off the project.

## A. Intake and response-rate analyses (inputs committed)

1. **Response rate by share eligible.** Does response rise with the LSOA eligible share, beyond the stratification? Scatter: each dot an LSOA, x = estimated eligible share (E_l), y = responses / letters sent. Weighted least squares (letters per LSOA vary enormously). Look for patterns by disadvantage. Inputs: `combined-pseudonymized.csv`, `lsoa-reference-table.csv`, letters per LSOA from `addresses-and-treatments.csv`.
2. **Back-of-envelope response rate per eligible household.** Same scatter, read as a rate: R / (E x L) intuition. Companion to item 1.
3. **Verify the kit-allocation slide claim.** The slide says too few disadvantaged respondents meant everyone got a kit; but Round 2 sent kits to disadvantaged respondents at a rate that needs checking (224/400 was quoted in Slack; the slides repo commit 2c5af9b already flags a corrected count of 168 — reconcile and finalize the slide).
4. **Map of responses** (appendix). Per-LSOA response rates, smoothed (the empirical-Bayes shrinkage machinery is already in the deck) to avoid stark small-cell artifacts. Mind the identifiability caveat: small LSOA cells are back-solvable; no OA-level map without an ethics check.
5. **Returned-letters denominator adjustments** (updated 2026-07-05). The randomized response analyses use mailed-letter denominators by design (eligibility is unobserved at assignment), and ECLIPS-PAP `scripts/27-denominator-sensitivity.R` (results in the returned-letters codebook) shows any adjustment is below reporting precision for them: 0.593% / 0.596% / 0.604% overall under mailed / net-of-returns / net-of-building-postcodes; no IMD quintile moves more than 2.5% relatively; ranking unchanged. What remains: (a) a one-line sensitivity note wherever response-rate slides appear; (b) the denominator construction for per-eligible estimands (items 1-2), where dead buildings inflate address counts in specific LSOAs and an adjustment may be warranted — a per-analysis choice to make when writing those analyses. Return patterns themselves: the deprivation skew (0.87% most-deprived quintile vs 0.21% least) stands; the demolition-register buildings' return spread is now explained by council decant schedules (Leafield Towers excepted); the Round 1 (0.687%) vs Round 2 (0.455%) gap remains unexplained after excluding the flagged LSOAs (0.32% vs 0.22%).

5b. **Sampling-frame building evidence** (rebuilt 2026-07-04; extended 2026-07-05 to 397 rows with council decant schedules and press articles). Evidence on buildings that may not be ordinary households (care homes, student residences, demolition blocks) is compiled in ECLIPS-PAP `data/processed/building-evidence.csv` (one row per source record, attributed and dated; codebook alongside; sources in `data/raw/frame-quality/`). No classifications are made in the file: any analysis that excludes addresses must state its own rule. This is one input into response rates among eligible households; the census eligibility shares are the other. ECLIPS-PAP Script 26 (assigned to Caner 2026-07-05) will report responses from these buildings' postcodes; whole-LSOA bounds already cap the answer at 19 of 890 usable responses.

6. **Survey drop-off analysis** (Ludovica's request; the original motivation for the whole test-response cleanup). Who started the intake survey but did not finish; how far they got (Progress, Finished, Duration); which page they stalled on (the twelve page-timer columns); timing relative to mailing. These fields are available for non-usable rows in the regenerated pseudonymized files. Test rows are confirmed removed: Script 23 ran clean at Warwick on 2026-07-05 (16 + 5 removed, no survivors, both rounds; result recorded in the script header).

## B. Spillover estimation (input committed)

7. **Spillover estimation.** The design, estimating equation, prespecified choices, and MDEs are fully written up in `ECLIPS-PAP/saturation-spillover-design-note.md` (rewritten 2026-07-04 after adversarial review). The per-OA per-round count table (oa, round, n_started, n_usable) is committed: `ECLIPS-PAP/data/processed/oa-response-counts.csv`, produced by `scripts/25-count-responses-by-oa.R` with hard-stop checks that its totals match the pseudonymized data. MDEs: 7.4% (started) / 10.0% (usable) per +1 SD of saturation.

## C. Kit and exposure-survey analyses (master dataset available)

8. **Confusion matrix: returned kit x completed exposure survey.** Marked "easy, I'll do it."
9. **Valid spot by disadvantage, conditional on returning the kit.** Currently the outcome conflates "returned and valid"; condition on return. Marked "easy, I'll do it."
10. **CDF of kit returns by days since the kit was sent.** To pick the threshold for closing the kit-return window (how long until effectively 100% of returns are in). This also sets the close date for the blood-lead set in group D, which is still filling.

## D. Blood-lead analyses (data arriving; 154 analyzed samples as of 2026-07-10, full set pending remaining kit returns)

11. **Seasonality in blood lead concentration** by sample date.
12. **Correlate the two spots' BLC difference, and number of spots completed, with education.**
13. **Confusion matrix / joint distribution of BLCs across a child's two blood spots.**
14. **Model the lab corrections** — quantify measurement error; "corrections" means things like subtracting batch-level lead estimates.

## Deck cleanup (from CODE-AUDIT-2026-07-04.md)

- The stale-file issue (audit finding 1) mostly SELF-HEALS: the deck reads `combined-pseudonymized.csv` by name, and the re-run regenerates that file. After the re-run: re-render and update the four hardcoded Phase I flowchart numbers (1,624 started / 734 ineligible-or-no-consent / 148,376 no response / 55%). See the combined-pseudonymized note under Standing caveats before relying on this.
- Fix the `Non-White` flag in `responder-traits-imd` (qmd:756): "Rather not say" currently counts as Non-White (audit finding 3; Q4 shown 31.6%, correct 27.6%).
- Reword the kit-allocation claim (qmd:802): "all KIT-ELIGIBLE disadvantaged got a kit; 168 of 176 (R2)" — confirm the 8 review-stage exclusions with Andrea (audit finding 2).
- Refresh the "Categorizing letters" slide (qmd:1076): returns are final — 752, fully categorized, clustered in a handful of named buildings.
- **Repoint the kit/blood chunks off the stale local master snapshot** (investigated 2026-07-10). Three chunks — kit-by-LSOA map (qmd:811) and the two kit-outcome slides (qmd:873, qmd:913) — read the local `2026-05-28-Warwick/2026-05-25-master_dataset_noaddr.csv`, an early RA vintage (94 cols, 1,624 rows) now superseded by ECLIPS-PAP `data/processed/master_dataset.csv` (105 cols, same 1,624-row spine). The current file adds the blood-lead columns that have since arrived (`venous_BLC`, `date_lastBloodTest`, `In clinical pathway?`, `LTHTSample_dateTime`, `Notify UKHSA?`) and roughly 5x more filled `CC-BLC_result` values, plus `oa`, the `where_learned_1..6` dummies, `send_voucher`, and `kit_review_note`; it drops `mix_sent` (removed as unreliable, ECLIPS-PAP `c721fe3`), `NewArrival?`, and the capitalized kit-round `Round`. The counts the deck currently shows are NOT wrong (the spine is identical), so this is a refresh-for-completeness, not a correction. Repointing is NOT a drop-in path swap: the current file renames `kit_type` → `golden_envelope` and recodes `valid_sample` to NA for never-returned kits, so the three chunks need edits (rename the factor, guard the `valid_sample >= 1` NA propagation). Do this together with the flowchart re-render above. The deck's other reads (`combined-pseudonymized.csv`, `addresses-and-treatments.csv`, `lsoa-reference-table.csv`) already point live at ECLIPS-PAP and self-heal. The `2026-05-27` sibling snapshot is also stale; the slides repo's own `data/` reference files (IMD scores, `household_age_data.csv`, Leeds geojson) have no ECLIPS-PAP counterpart and are not affected.
- Fix the exclusion-appendix double-count (5,521, not 5,955) if that slide is reused.

## Standing caveats

- **`combined-pseudonymized.csv` is the single combined file** (consolidation done 2026-07-10, ECLIPS-PAP commit `a722632`; the former `combined-pseudonymized2.csv` is removed). It is written by Script 14, the documented pseudonymized-release pipeline; `build_master_dataset.R` reads it and applies the kit-mixup relabel (`c2142a2e` → `eclips2mxs`) at the merge, so the relabeled ID appears only in `master_dataset.csv`. All readers point at the one file: the slides deck, `intake-survey-exploration.R`, and ECLIPS-PAP Scripts 25, 27, and the spillover verification scripts. The kit mix-up is documented in ECLIPS-PAP `data/processed/kit-mixup-correction.md`. Do not de-duplicate the identical non-usable rows (they are distinct respondents flattened by pseudonymization).
- Small-cell identifiability constrains the response map (item 4) and any OA-level presentation (item 7's outputs). The OA-on-release decision is recorded in the ECLIPS-PAP codebooks (`473c77e`).
- Do not report anything from `master_dataset.csv` in a way that presumes the blood-lead set is complete: 154 samples are analyzed as of 2026-07-10 and more kits are still returning (item 10 sets the close date).
