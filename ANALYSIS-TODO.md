# Analysis to-do list

Consolidated from Ludovica's slide feedback (#general, 2026-05-28, three posts), Frank's annotated re-post (2026-06-03, ts 1780496380.829809), and the items added by the 2026-07-04 investigation session. Organized by what each task waits on. Written 2026-07-04.

## A. Doable now (all inputs committed in ECLIPS-PAP)

1. **Response rate by share eligible.** Does response rise with the LSOA eligible share, beyond the stratification? Scatter: each dot an LSOA, x = estimated eligible share (E_l), y = responses / letters sent. Weighted least squares (letters per LSOA vary enormously). Look for patterns by disadvantage. Inputs: `combined-pseudonymized2.csv`, `lsoa-reference-table.csv`, letters per LSOA from `addresses-and-treatments.csv`.
2. **Back-of-envelope response rate per eligible household.** Same scatter, read as a rate: R / (E x L) intuition. Companion to item 1.
3. **Verify the kit-allocation slide claim.** The slide says too few disadvantaged respondents meant everyone got a kit; but Round 2 sent kits to disadvantaged respondents at a rate that needs checking (224/400 was quoted in Slack; the slides repo commit 2c5af9b already flags a corrected count of 168 — reconcile and finalize the slide).
4. **Map of responses** (appendix). Per-LSOA response rates, smoothed (the empirical-Bayes shrinkage machinery is already in the deck) to avoid stark small-cell artifacts. Mind the identifiability caveat: small LSOA cells are back-solvable; no OA-level map without an ethics check.
5. **Returned-letters denominator adjustments** (updated 2026-07-05). The randomized response analyses use mailed-letter denominators by design (eligibility is unobserved at assignment), and ECLIPS-PAP `scripts/27-denominator-sensitivity.R` (results in the returned-letters codebook) shows any adjustment is below reporting precision for them: 0.593% / 0.596% / 0.604% overall under mailed / net-of-returns / net-of-building-postcodes; no IMD quintile moves more than 2.5% relatively; ranking unchanged. What remains: (a) a one-line sensitivity note wherever response-rate slides appear; (b) the denominator construction for per-eligible estimands (items 1-2), where dead buildings inflate address counts in specific LSOAs and an adjustment may be warranted — a per-analysis choice to make when writing those analyses. Return patterns themselves: the deprivation skew (0.87% most-deprived quintile vs 0.21% least) stands; the demolition-register buildings' return spread is now explained by council decant schedules (Leafield Towers excepted); the Round 1 (0.687%) vs Round 2 (0.455%) gap remains unexplained after excluding the flagged LSOAs (0.32% vs 0.22%).

5b. **Sampling-frame building evidence** (rebuilt 2026-07-04; extended 2026-07-05 to 397 rows with council decant schedules and press articles). Evidence on buildings that may not be ordinary households (care homes, student residences, demolition blocks) is compiled in ECLIPS-PAP `data/processed/building-evidence.csv` (one row per source record, attributed and dated; codebook alongside; sources in `data/raw/frame-quality/`). No classifications are made in the file: any analysis that excludes addresses must state its own rule. This is one input into response rates among eligible households; the census eligibility shares are the other. ECLIPS-PAP Script 26 (assigned to Caner 2026-07-05) will report responses from these buildings' postcodes; whole-LSOA bounds already cap the answer at 19 of 890 usable responses.

## A2. Deck cleanup (from CODE-AUDIT-2026-07-04.md; do together with the group-B re-run)

- The stale-file issue (audit finding 1) mostly SELF-HEALS: the deck reads `combined-pseudonymized.csv` by name, and Caner's re-run regenerates that file correctly. After the re-run: re-render and update the four hardcoded Phase I flowchart numbers (1,624 started / 734 ineligible-or-no-consent / 148,376 no response / 55%).
- Fix the `Non-White` flag in `responder-traits-imd` (qmd:756): "Rather not say" currently counts as Non-White (audit finding 3; Q4 shown 31.6%, correct 27.6%).
- Reword the kit-allocation claim (qmd:802): "all KIT-ELIGIBLE disadvantaged got a kit; 168 of 176 (R2)" — confirm the 8 review-stage exclusions with Andrea (audit finding 2).
- Refresh the "Categorizing letters" slide (qmd:1076): returns are final — 752, fully categorized, clustered in a handful of named buildings.
- Comment the 05-25 master-vintage pin in the kit chunks; fix the exclusion-appendix double-count (5,521, not 5,955) if that slide is reused.

## B. Blocked on Caner re-running Scripts 09/14 (requested 2026-07-04; instructions confirmed 2026-07-05 — push all THREE files including combined-pseudonymized.csv, which the May 9 commit omitted)

6. **Survey drop-off analysis** (Ludovica's request; the original motivation for the whole test-response cleanup). Who started the intake survey but did not finish; how far they got (Progress, Finished, Duration); which page they stalled on (the twelve page-timer columns); timing relative to mailing. The re-run makes these fields available for non-usable rows in the pseudonymized files. Test rows are confirmed removed: Script 23 ran clean at Warwick on 2026-07-05 (16 + 5 removed, no survivors, both rounds; result recorded in the script header).

## C. Blocked on Caner running Script 25 (committed and assigned 2026-07-05)

7. **Spillover estimation.** The design, estimating equation, prespecified choices, and MDEs are fully written up in `ECLIPS-PAP/saturation-spillover-design-note.md` (rewritten 2026-07-04 after adversarial review). The one missing input, the per-OA per-round count table (oa, round, n_started, n_usable), now has a committed producer: ECLIPS-PAP `scripts/25-count-responses-by-oa.R`, with hard-stop checks that its totals match the pseudonymized data; Caner runs it and pushes `data/processed/oa-response-counts.csv`. MDEs: 7.4% (started) / 10.0% (usable) per +1 SD of saturation.

## D. Blocked on the fixed master dataset (Andrea: merge-bug fix + mix_sent answer, requested 2026-07-04)

8. **Confusion matrix: returned kit x completed exposure survey.** Marked "easy, I'll do it."
9. **Valid spot by disadvantage, conditional on returning the kit.** Currently the outcome conflates "returned and valid"; condition on return. Marked "easy, I'll do it."
10. **CDF of kit returns by days since the kit was sent.** To pick the threshold for closing the kit-return window (how long until effectively 100% of returns are in).

## E. Blocked on receiving the blood lead levels (BLLs, from the lab via Angela)

11. **Seasonality in blood lead concentration** by sample date.
12. **Correlate the two spots' BLC difference, and number of spots completed, with education.**
13. **Confusion matrix / joint distribution of BLCs across a child's two blood spots.**
14. **Model the lab corrections** — quantify measurement error; "corrections" means things like subtracting batch-level lead estimates.

## Standing caveats

- Do not compute anything from `master_dataset.csv` until Andrea's merge fix is in and the `mix_sent` question (whether `824ae6af`/`47c36aaf` blood results are correctly attributed) is answered.
- Small-cell identifiability constrains the response map (item 4) and any OA-level presentation (item 7's outputs).
- Use `combined-pseudonymized2.csv`, never the stale `combined-pseudonymized.csv`; do not de-duplicate the identical non-usable rows (they are distinct respondents). Revisit this caveat as soon as Caner pushes the regenerated files (asked 2026-07-05): the regenerated `combined-pseudonymized.csv` should then be correct, and the deck reads it by name. The staleness plausibly originated when the May 9 commit (`196ebc3`) pushed the two round files but not the combined one.
