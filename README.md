# Teliti Scrapers

Automated environmental-data collection scripts for the Teliti research project.

## Data sources

- CNEMC national automatic surface-water monitoring
- NMEMC marine water quality
- Fujian Province weekly surface-water reports
- Indonesia ONLIMO water-quality monitoring

## Collection architecture

The same R scraper code is intended to run in two independent environments:

1. Local Windows PC — primary collector
2. GitHub Actions — backup collector

Research data and generated archives are not stored in this code repository.

## Repository structure

```text
fujian_surfacewater/script/
nmemc/script/
onlimo/script/
.github/workflows/