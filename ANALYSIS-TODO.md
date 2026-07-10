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
2. **Back-of-envelope response rate per eligible household.** Same scatter, read as a rate: R / (E x L) intuition. Companion to item 1. The per-eligible estimand is written up in ECLIPS-PAP `phase1-analysis-plan.md` Part 2: same numerator, denominator changed to π̂_ℓ × letters mailed, under the stated assumption Pr(eligible | mailed, ℓ) = π̂_ℓ. A fuller treatment that propagates uncertainty in π̂ (a two-part model with a latent eligibility rate, the "Tier 3" of the original plan) is described in ECLIPS-PAP CLAUDE.md's analysis-plan summary but has no written specification.

2b. **External check of the census eligible-share estimates against self-reported eligibility** (added 2026-07-10). The survey observes eligibility among responders: every row of `combined-pseudonymized.csv` carries the eligibility classification (`eligible`, child aged 1-6) and an `lsoa`, so the share of started responses reporting an eligible child can be compared per LSOA with the census-based share E_ℓ in `lsoa-reference-table.csv`. At OA level the committed `oa-response-counts.csv` carries only started/usable counts (usable adds the consent and GP conditions on top of eligibility); the census counterpart there is `data/raw/census-2021-leeds-oa/rm006-youngest-child-age-oa.csv`, already used for the spillover balance tests (item 7). This check bears on the denominator construction for items 1-2 and on the Part 2 identification assumption. Its limitation must be stated wherever it is used: the comparison conditions on having responded, and eligible and ineligible households respond at different rates, so responder-reported eligibility does not estimate Pr(eligible | mailed) directly.

3. **Verify the kit-allocation slide claim.** SETTLED (recognized 2026-07-10; the answer dates to 2026-06-09). The slide now reads "all kit-eligible disadvantaged got a kit: 168 of 176 (R2)". The 8 disadvantaged non-recipients were excluded at the Script 15/16 review gate — 6 could not provide a verifiable GP, 2 had bounced emails — confirmed by Andrea in #general 2026-06-09 (thread ts 1781003461.772679) and recorded per participant in `master_dataset.csv` `kit_review_note` (2 bounced + 6 GP among these 8; 19 review-gate exclusions in total across both groups).
4. **Map of responses** (appendix). Per-LSOA response rates, smoothed (the empirical-Bayes shrinkage machinery is already in the deck) to avoid stark small-cell artifacts. Mind the identifiability caveat: small LSOA cells are back-solvable; no OA-level map without an ethics check.
5. **Returned-letters denominator adjustments** (updated 2026-07-05). The randomized response analyses use mailed-letter denominators by design (eligibility is unobserved at assignment), and ECLIPS-PAP `scripts/27-denominator-sensitivity.R` (results in the returned-letters codebook) shows any adjustment is below reporting precision for them: 0.593% / 0.596% / 0.604% overall under mailed / net-of-returns / net-of-building-postcodes; no IMD quintile moves more than 2.5% relatively; ranking unchanged. What remains: (a) a one-line sensitivity note wherever response-rate slides appear; (b) the denominator construction for per-eligible estimands (items 1-2), where dead buildings inflate address counts in specific LSOAs and an adjustment may be warranted — a per-analysis choice to make when writing those analyses. Return patterns themselves: the deprivation skew (0.87% most-deprived quintile vs 0.21% least) stands; the demolition-register buildings' return spread is now explained by council decant schedules (Leafield Towers excepted); the Round 1 (0.687%) vs Round 2 (0.455%) gap remains unexplained after excluding the flagged LSOAs (0.32% vs 0.22%).

5b. **Sampling-frame building evidence** (rebuilt 2026-07-04; extended 2026-07-05 to 397 rows with council decant schedules and press articles). Evidence on buildings that may not be ordinary households (care homes, student residences, demolition blocks) is compiled in ECLIPS-PAP `data/processed/building-evidence.csv` (one row per source record, attributed and dated; codebook alongside; sources in `data/raw/frame-quality/`). No classifications are made in the file: any analysis that excludes addresses must state its own rule. This is one input into response rates among eligible households; the census eligibility shares are the other. ECLIPS-PAP Script 26 (assigned to Caner 2026-07-05) will report responses from these buildings' postcodes; whole-LSOA bounds already cap the answer at 19 of 890 usable responses.

6. **Survey drop-off analysis** (Ludovica's request; the original motivation for the whole test-response cleanup). Who started the intake survey but did not finish; how far they got (Progress, Finished, Duration); which page they stalled on (the twelve page-timer columns); timing relative to mailing. These fields are available for non-usable rows in the regenerated pseudonymized files. Test rows are confirmed removed: Script 23 ran clean at Warwick on 2026-07-05 (16 + 5 removed, no survivors, both rounds; result recorded in the script header).

## B. Spillover estimation (input committed)

7. **Spillover estimation.** The design, estimating equation, prespecified choices, and MDEs are fully written up in `ECLIPS-PAP/saturation-spillover-design-note.md` (rewritten 2026-07-04 after adversarial review). The estimation input is `ECLIPS-PAP/data/processed/oa-response-counts-45day.csv` (Script 25b, 2026-07-10), which applies the 45-day baseline outcome to the Warwick-produced `oa-response-counts.csv` by removing the 2 late Round 1 responses; the OA attributions and their grounds are in `oa-response-counts-codebook.md`. MDEs: 7.4% (started) / 10.0% (usable) per +1 SD of saturation, unchanged by the window correction.

## C. Kit and exposure-survey analyses (master dataset available)

8. **Confusion matrix: returned kit x completed exposure survey.** Marked "easy, I'll do it."
9. **Valid spot by disadvantage, conditional on returning the kit.** Currently the outcome conflates "returned and valid"; condition on return. Marked "easy, I'll do it."
10. **CDF of kit returns by days since the kit was sent.** To pick the threshold for closing the kit-return window (how long until effectively 100% of returns are in). This also sets the close date for the blood-lead set in group D, which is still filling.

## D. Blood-lead analyses (data arriving; 154 analyzed samples as of 2026-07-10, full set pending remaining kit returns)

11. **Seasonality in blood lead concentration** by sample date.
12. **Correlate the two spots' BLC difference, and number of spots completed, with education.**
13. **Confusion matrix / joint distribution of BLCs across a child's two blood spots.**
14. **Model the lab corrections** — quantify measurement error; "corrections" means things like subtracting batch-level lead estimates.

## Deck cleanup (from CODE-AUDIT-2026-07-04.md) — largely done 2026-07-10

Completed in the 2026-07-10 revision of `2026-05-28-Warwick.qmd` (deck re-rendered against the current ECLIPS-PAP data):

- All chunks now read live ECLIPS-PAP files; the kit/blood chunks were repointed from the local `2026-05-25-master_dataset_noaddr.csv` snapshot to `data/processed/master_dataset.csv` (`golden_envelope` factor, NA-safe `returned`/`valid` definitions), and both local snapshots plus their NOTES file were `git rm`ed.
- Both flowcharts now compute their numbers from the data at render (inline R in the tikz), so the "update hardcoded numbers" task no longer exists. Current render: 1,622 started / 889 usable (45-day outcome; see Standing caveats), kit returns 79/75, valid 58/58, exposure surveys 89/77.
- Fixed: the `Non-White` flag ("Rather not say" no longer in the numerator; denominator convention stated in the caption), the kit-allocation claim ("all KIT-ELIGIBLE disadvantaged; 168 of 176 (R2)"), the "Categorizing letters" slide (752 final, categorized, denominator sensitivity), the exclusion appendix (5,521 with overlaps, plus the student-residence gap note), the E_ℓ description (households with youngest child 0-9, RM006 — was wrongly "0-4 & 5-9 bands"), the Miettinen-Nurminen-mislabeled-as-Newcombe interval (review finding A; `PropCIs` dropped), and the doubled title word.
- Added: a treatment-differences slide showing both outcomes (started AND usable — the usable contrasts were previously never shown as differences; all null), snapshot/censoring captions on every kit-outcome slide, and the 45-day-window footnote on the outcomes slide.

Still open:

- When the kit-return window closes (item 10), either condition the kit-return comparisons on round or retire the snapshot captions.
- Deferred pure-style consolidations from the 2026-06-09 review (findings F/G/H: forest/map/gradient plot helpers; also the C/G/P label constant). Error-reducing consolidations (single reads, shared `age_grp` and Newcombe helpers) are done.
- Review findings C and D (title wording on the IMD-gradient and letters-vs-deprivation slides) remain undecided in CODE-REVIEW-2026-06-09.md.

## Standing caveats

- **The baseline response outcome is response within 45 days of the round's mailing** (decided 2026-07-10; recorded in ECLIPS-PAP `round1-pseudonymized-codebook.md`, CLAUDE.md, and the analysis-plan status note). Round 1's survey stayed open until 2026-01-05, 7 days past its window; the 2 responses (1 usable) received in the gap sit in the committed data files but are excluded from analyses. Deck counts: 1,622 started / 889 usable. File counts: 1,624 / 890. Round 2 is unaffected (its close date is its 45-day mark). The spillover input applying this outcome is `oa-response-counts-45day.csv` (Script 25b).
- **`combined-pseudonymized.csv` is the single combined file** (consolidation done 2026-07-10, ECLIPS-PAP commit `a722632`; the former `combined-pseudonymized2.csv` is removed). It is written by Script 14, the documented pseudonymized-release pipeline; `build_master_dataset.R` reads it and applies the kit-mixup relabel (`c2142a2e` → `eclips2mxs`) at the merge, so the relabeled ID appears only in `master_dataset.csv`. All readers point at the one file: the slides deck, `intake-survey-exploration.R`, and ECLIPS-PAP Scripts 25, 27, and the spillover verification scripts. The kit mix-up is documented in ECLIPS-PAP `data/processed/kit-mixup-correction.md`. Do not de-duplicate the identical non-usable rows (they are distinct respondents flattened by pseudonymization).
- Small-cell identifiability constrains the response map (item 4) and any OA-level presentation (item 7's outputs). The OA-on-release decision is recorded in the ECLIPS-PAP codebooks (`473c77e`).
- Do not report anything from `master_dataset.csv` in a way that presumes the blood-lead set is complete: 154 samples are analyzed as of 2026-07-10 and more kits are still returning (item 10 sets the close date).
