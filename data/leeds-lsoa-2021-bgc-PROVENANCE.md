# Leeds LSOA 2021 Boundaries — Provenance

**File:** `leeds-lsoa-2021-bgc.geojson` (488 Leeds LSOAs, WGS84 / EPSG:4326)
**Downloaded:** 2026-05-22

## Source

ONS Open Geography Portal — *Lower layer Super Output Areas (December 2021)
Boundaries EW BGC* (BGC = Generalised, Clipped — small file suited to mapping).

- Licence: Open Government Licence v3.0
- Service version at download: `..._BGC_V5`

## How it was created

Queried the ArcGIS FeatureServer, filtered to Leeds via the LSOA name prefix
(ONS LSOA names are prefixed by local authority, e.g. "Leeds 035C"), and
requested GeoJSON reprojected to WGS84:

```bash
SVC="https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5/FeatureServer/0/query"
curl -s "$SVC?where=LSOA21NM%20LIKE%20%27Leeds%25%27&outFields=LSOA21CD&outSR=4326&f=geojson" \
  -o leeds-lsoa-2021-bgc.geojson
```

Returned 488 features — an exact match to the 488 Leeds LSOAs in
`ECLIPS-PAP/data/processed/lsoa-reference-table.csv`.

## Notes

- The committed `.geojson` is the static input the slides build from; no network
  access is needed at render time.
- To refresh, re-run the query above (the `_V5` service name may bump over time;
  find the current one in the ONS org's service directory at
  `https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services?f=json`).
