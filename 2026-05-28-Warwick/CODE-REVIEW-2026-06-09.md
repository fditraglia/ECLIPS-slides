# Code review — analyses in `2026-05-28-Warwick.qmd`

Review date: 2026-06-09. Branch: `review/slide-analyses`.

Five parallel reviewers swept the deck and **verified the numbers against the actual
data** (`~/ECLIPS-PAP/data/processed/combined-pseudonymized.csv` and
`master_dataset_noaddr.csv`), not just by reading code.

**Headline:** the statistical machinery is sound. Wilson CIs, the empirical-Bayes
shrinkage, quintile cuts, response-rate denominators (letters mailed, not respondents),
kit return-outcome definitions, the "no effect of kit type" claim, and every map join
all reproduce exactly under independent recomputation. The actionable items are mostly
framing/labels plus duplicated code.

---

## How to respond

For each finding below, fill in the `RESPONSE` block. Leave the finding text untouched
so we can diff. Use this template:

```
RESPONSE:
  decision: ACCEPT | REJECT | DEFER | DISCUSS
  action:   <what you want done, if anything — e.g. "reword to X", "leave as is", "Frank to confirm with RA">
  notes:    <any reasoning, the exact wording you want, or a question back to me>
```

- **ACCEPT** = make the change as proposed (or as you specify in `action`).
- **REJECT** = leave as is (say why in `notes` if useful).
- **DEFER** = not now; revisit later.
- **DISCUSS** = you want my input before deciding.

When you're done, save the file and tell me — I'll act on the ACCEPTs, come back on the
DISCUSSes, and we can iterate.

---

# Part 1 — Wrong or misleading

## A. Confidence-interval method is mislabeled  ·  severity: BUG
`2026-05-28-Warwick.qmd` — `diff_ci` (line ~588), caption (line ~619)

`diff_ci` is captioned "Newcombe score interval" but actually calls
`PropCIs::diffscoreci`, which implements the **Miettinen–Nurminen** interval — a
different method. Meanwhile `score_diff` (line ~995) *is* the genuine Newcombe (1998)
hybrid-Wilson interval, and it *also* carries a "Newcombe" caption. So two slides claim
the same method name for two different algorithms, and their bounds differ materially.

Proposed fix: unify on the hand-rolled Newcombe (`score_diff`) in both places, so the
caption is honest and the `PropCIs` dependency is dropped. (Alternative: just relabel
line 619 as "Miettinen–Nurminen.")

RESPONSE:
  decision:
  action:
  notes:

## C. "Lower response rate in more deprived LSOAs" overstates a non-monotone gradient  ·  severity: MISLEADING TITLE
`2026-05-28-Warwick.qmd` line ~664  (chunk `usable-imd-marginal`)

Recomputed usable rate per 1,000 by quintile: Q1 = 6.89, **Q2 = 7.30 (peak)**,
Q3 = 5.28, Q4 = 5.72, Q5 = 4.99. The least-deprived quintile is not the highest, and
there's a Q3 dip / Q4 bump. The Q1-vs-Q5 endpoints do separate (CIs don't overlap), so
a net negative association is defensible — but the title implies a clean monotone
decline the data doesn't show.

Proposed fix: soften the title (e.g. "Response rate tends to fall in more deprived
LSOAs") or annotate the non-monotonicity. Editorial call — your wording.

RESPONSE:
  decision:
  action:
  notes:

## D. "Letters sent vs deprivation" is mechanically determined by the eligibility rule  ·  severity: MISLEADING FRAMING
`2026-05-28-Warwick.qmd` lines ~379–391  (chunk `letters-vs-deprivation`)

Letters were allocated proportional to estimated eligible-share `E_l` *by design*, and
the plotted x (`share = n_letters / sum`) correlates 0.9999 with `E_l`. So this slide is
effectively "share of eligible children vs deprivation," and its upward slope is a
deterministic consequence of the eligibility weighting shown two slides earlier — not a
separate targeting choice. The fact itself is fine; the framing implies a decision.

Proposed fix: retitle toward "Eligible children (hence letters) vs deprivation," or add
one line stating this is the deprivation footprint of the eligibility weighting.
Editorial call.

RESPONSE:
  decision:
  action:
  notes:

## E. Responder-traits flags fold NA + "Rather not say" into the negative  ·  severity: MINOR INCONSISTENCY
`2026-05-28-Warwick.qmd` lines ~751–756  (chunk `responder-traits-imd`)

Low-income / renting / low-education flags use `%in%`, which returns FALSE for true NAs
*and* for "Rather not say" — so item-nonresponders are counted in the denominator as
"not low-income / not renting / etc.", biasing those percentages slightly downward. This
is inconsistent with `plot_bar`, which shows "Rather not say" as its own visible bar.
The gradients are strong and monotone regardless, so the qualitative story holds.

Proposed fix: either exclude RNS/NA from the denominator
(`n = sum(!is.na(flag) & answered)`), or state that nonresponse is treated as baseline.

RESPONSE:
  decision:
  action:
  notes:

---

# Part 2 — Readability / simplicity (all formally correct)

## F. Collapse three near-identical forest-plot blocks into one helper  ·  severity: STYLE
Chunks `cell-rate-forest` (~466), `click-rate-forest` (~548), `kit-type-forest` (~868)
differ mainly in the x-aesthetic + axis label. Extract e.g.
`forest_by_cell(df, xvar, xlab)`. Removes ~30 duplicated lines and keeps the slides
visually consistent.

RESPONSE:
  decision:
  action:
  notes:

## G. Collapse duplicated map blocks into one helper  ·  severity: STYLE
`eb-map-raw` (~1322), `eb-map-shrunk` (~1341), `leeds-imd-map` (~1205) repeat the same
`geom_sf + scale_fill_viridis_c + coord_sf(datum=NA) + theme_void` stack. A small
`eb_map(col, option)` helper dedupes it.

RESPONSE:
  decision:
  action:
  notes:

## H. Collapse duplicated gradient plots into one helper  ·  severity: STYLE
`imd-by-treatment` (~1229), `click-imd-by-treatment` (~1390), `click-imd-by-age` (~1364)
repeat the same ~12-line line/pointrange/dodge/theme stanza. One
`gradient_plot(df, colour_var, outcome, ylab)` helper removes ~3 copies.

RESPONSE:
  decision:
  action:
  notes:

## I. Fix `score_ci`'s hard-coded ×1000 scaling  ·  severity: STYLE
`score_ci` (line ~83) hard-codes a per-1,000 scale, so callers undo it with magic `/10`
and `/1000` (lines ~766, ~956, ~1300). Add a `scale` argument. Also `wilson_lohi`
(line ~989) re-implements the same Wilson math — collapse to one helper. And the
`rowwise() |> mutate(ci = list(score_ci(...)))` boilerplate recurs ~7×; consider a small
`add_score_ci()` wrapper.

RESPONSE:
  decision:
  action:
  notes:

## J. Read `master_dataset_noaddr.csv` once  ·  severity: STYLE
The kit chunks read `master_dataset_noaddr.csv` 3–4× and re-define the
`returned`/`valid` outcomes twice. Read it once into a shared `include: false` prep chunk
and define the outcomes once.

RESPONSE:
  decision:
  action:
  notes:

## K. Smaller cleanups  ·  severity: STYLE / NIT
- Promote brand colors `"#3B6E8F"` / `"#B5651D"` to named constants in `setup`.
- `inner_join` → `left_join` at lines ~347, ~380 (nothing is dropped; documents intent).
- `recode()` → `case_match()` (recode is superseded) at lines ~138, ~509.
- Typo line ~279: "Council Tax data**i**" → "data".
- Typo line ~580: "Treatment **effects** *starting*" → "affects" (affect/effect).
- Slide titles at ~544 and ~580 are nearly identical — differentiate.
- `worry` panel (`kit-return-attitudes`) silently drops n=58 "Not sure" — defensible,
  but worth a one-line note or confirming it's intended.

RESPONSE (you can respond per-item in notes):
  decision:
  action:
  notes:
