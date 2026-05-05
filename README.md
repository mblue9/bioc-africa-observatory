# Bioconductor Africa Observatory

This repository contains aggregated, country-level maps for the Bioconductor Africa Seminar Series.

The site currently includes:

- a global map of registrants by country
- an Africa-focused view of the same data

Raw registration or attendance files are not committed to the repository.

## Data privacy

Only aggregated country-level counts should be published. Do not commit files containing names, email addresses, registration timestamps, or other individual-level data.

Raw files should be stored locally in `data-raw/`, which is ignored by git.

## Render locally

```r
quarto::quarto_render()