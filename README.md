# Quadrat Studio — interactive apps

Public-data demonstration apps for [quadratstudio.ca](https://quadratstudio.ca),
deployed to [Posit Connect Cloud](https://connect.posit.cloud). Each folder is an
independent deployment with its primary file, a `manifest.json` (R version +
package versions), and a cached `.rds` data file so it loads instantly without
re-downloading.

| Folder | Content | Primary file | Data |
|--------|---------|--------------|------|
| `sar-screening/` | Species at Risk — site screening tool | `app.R` | GBIF (cached) |
| `native-bees/` | Ontario bumble bee tracker | `app.R` | GBIF (cached) |
| `monitoring-power/` | Monitoring-design power tool | `app.R` | none (simulation) |
| `biodiversity-report/` | Reproducible biodiversity report (Quarto) | `report_biodiversity_summary.qmd` | GBIF (cached) |
| `bumblebee-decline/` | At-risk bumble bee decline & range-shift dashboard | `app.R` | GBIF (cached) |
| `bumblebee-decline-report/` | At-risk bumble bee report (Quarto) | `report_bumblebee_decline.qmd` | GBIF (cached) |

## Deploying / updating

1. In Connect Cloud, click **Publish** → choose this repo → select the folder's
   primary file → **Publish**.
2. To update a deployed app, commit and push changes here — Connect Cloud
   redeploys from GitHub.

## Regenerating a manifest (after changing packages)

```r
rsconnect::writeManifest(appDir = "sar-screening", appPrimaryDoc = "app.R")
```

## Refreshing cached data

Delete the folder's `cache_*.rds`, run the app/report once locally to rebuild it,
then commit the new cache.
