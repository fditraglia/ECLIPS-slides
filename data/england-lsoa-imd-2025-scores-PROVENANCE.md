# England LSOA IMD 2025 Scores — Provenance

**File:** `england-lsoa-imd-2025-scores.csv` (33,755 rows: one per English LSOA; columns `lsoa`, `imd_score`)
**Downloaded:** 2026-05-22

## Source

MHCLG, English Indices of Deprivation 2025 — File 7 ("All Ranks, Scores,
Deciles and Population Denominators"), the full national file.

- Licence: Open Government Licence v3.0
- URL: https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025

## How it was created

Downloaded the national File 7, kept only the LSOA code and IMD score columns:

```r
library(tidyverse); library(janitor)
read_csv("File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv") |>
  clean_names() |>
  transmute(lsoa = lsoa_code_2021,
            imd_score = index_of_multiple_deprivation_imd_score) |>
  write_csv("england-lsoa-imd-2025-scores.csv")
```

## Integrity check

The 488 Leeds rows (LAD E08000035) in this national file match the committed
Leeds-only file `ECLIPS-PAP/data/raw/imd-2025/imd-2025-lsoa.csv` exactly
(maximum absolute score difference = 0).

## Notes

- Used only for the England-vs-Leeds deprivation distribution slide.
- The full ~9 MB national File 7 is not committed; only this compact
  two-column extract (~0.5 MB) is kept.
