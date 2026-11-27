> **THIS FORK AMENDS SOME OF THE DATA IN THE ORIGINAL REPO** The Final OAC reuses the Preview clusters: see [PREVIEW_VS_FINAL.md](PREVIEW_VS_FINAL.md) for how they relate, and which files are consistent with the published labels.

# UK Output Area Classification (UK OAC) - Final Version

This repository contains the **R script** and associated resources used to produce the **final version** of the UK Output Area Classification. The classification draws on Census 2021 (England & Wales) / 2022 (Scotland & Northern Ireland) data, as well as other supporting geographies, to segment the UK population into distinct clusters at the Output Area (OA) or equivelent level.

## Table of Contents

1. [Introduction](#introduction)
2. [Authors and Acknowledgments](#authors-and-acknowledgments)
3. [Project Structure](#project-structure)
4. [Data Sources](#data-sources)
5. [Prerequisites and Setup](#prerequisites-and-setup)
6. [Workflow Overview](#workflow-overview)
    - [1. Setup and Data Import](#1-setup-and-data-import)
    - [2. Census Variables and Percentages](#2-census-variables-and-percentages)
    - [3. Creating Input Measures](#3-creating-input-measures)
    - [4. Combining and Transforming Variables](#4-combining-and-transforming-variables)
    - [5. Preview vs. Final OAC Comparison](#5-preview-vs-final-oac-comparison)
    - [6. Generating Final Clusters](#6-generating-final-clusters)
    - [7. Visualization and Output](#7-visualization-and-output)
7. [Outputs](#outputs)
8. [How to Cite](#how-to-cite)
9. [License](#license)

---

## Introduction

This repository provides the complete workflow for updating and finalising the **UK Output Area Classification (UK OAC)**. This classification integrates socio-demographic variables derived from the latest available Censuses in England, Wales, Scotland, and Northern Ireland, along with spatial and other structural transformations.

This project updates the interim preview OAC [2021/22](https://github.com/jakubwyszomierski/OAC2021-2) which was generated using 2021 England and Wales census data, with 2021 data zone modelled data for Northern Ireland, and modelled OA level Scottish data. For England and Wales, the interim preview classification has been disseminated by the [ONS](https://www.ons.gov.uk/methodology/geography/geographicalproducts/areaclassifications/2021residentialbasedareaclassifications); and the full UK preview classification by the [CDRC](https://data.cdrc.ac.uk/dataset/uk-oac).

The primary goal here was to produce a finalised UK-wide classification, ensuring that Scotland and Northern Ireland were fully integrated with consistent methodology and comparability to England and Wales.

---

## Authors and Acknowledgments

**Geographic Data Service:**
- [Alex Singleton](https://www.liverpool.ac.uk/geography-and-planning/staff/alexander-singleton/), University of Liverpool
- [Owen Goodwin](https://www.liverpool.ac.uk/geography-and-planning/), University of Liverpool
- [Paul Longley](https://www.geog.ucl.ac.uk/people/academic-staff/paul-longley), University College London
- [Jakub Wyszomierski](https://github.com/jakubwyszomierski/OAC2021-2) - although not involved in this update, developed the interim preview classification.

---

## Project Structure

```
├── data
│   ├── census              # Chunked or raw census files
│   ├── lookup              # CSV lookups for variable codes, old vs. new
│   └── UKOAC               # Files with existing/preview OAC assignments
├── extra_code
│   └── ...                 # Additional scripts to chunk large CSVs, etc.
├── map
│   └── ...                 # Spatial outputs (GPKG, etc.)
├── plot
│   └── comparison          # Generated comparison plots (boxplots, alluvial diagrams)
└── README.md               # This README
```

- **`OAC_Input.parquet`:** Final input data (raw percentages) as calculated by `Code.R`. For E&W these are **not** the values the published clusters were built from.
- **`OAC_Input_IHS.parquet`:** Transformed inputs (inverse hyperbolic sine, then range-standardised) that reproduce `UK_OAC_Final.parquet` exactly:
  - **E&W rows** are the Preview OAC inputs ([OAC2021-2](https://github.com/jakubwyszomierski/OAC2021-2) `Data/Clustering/OAC_assignment.csv`, renamed to the Final variable numbering). The E&W assignments are the Preview assignments, unchanged. Classify them top-down (supergroup → group → subgroup) against the published Preview centroids, using all 60 variables.
  - **Scotland/NI rows** are the Final inputs from `Code.R`. They were assigned to the nearest Preview subgroup centroid, excluding `ex_variables`. `v40`/`v41` are empty for these rows.
  - Built by `extra_code/preview_ew_inputs.py`, which also checks both rules against `UK_OAC_Final.parquet`. Re-running `Code.R` overwrites this file with the Final values.
- **`UK_OAC_Final.parquet`:** Final assignment of Output Areas to OAC clusters.
- **`Comparison.png` / `Comparison_S_NI.png`:** Alluvial plots comparing old and new classifications.

---

## Data Sources

1. **ONS Census 2021** (England & Wales):
   - [ONS Data Service](https://www.ons.gov.uk/)
   - Downloaded from GitHub-based CSV files

2. **NRS Census 2022** (Scotland):
   - [Scotland's Census](https://www.scotlandscensus.gov.uk/)
   - Downloaded from GitHub-based CSV files

3. **NISRA Census 2021** (Northern Ireland):
   - [NISRA Website](https://www.nisra.gov.uk/)
   - Downloaded from GitHub-based CSV files

4. **ONS Geoportal** for boundaries and shapefiles, including clipped EW, Scotland, and Northern Ireland geographies.

Please see the script comments for specific file paths and download links.

---

## Prerequisites and Setup

- **R (≥ 4.0)**
- Key R packages:
  - `tidyverse`, `sf`, `magrittr`, `janitor`, `scales`, `arrow`, `purrr`, `ggalluvial`, `h2o`

Before running the script:

1. **Install** the required packages (e.g. `install.packages("tidyverse")`).
2. **Download** necessary GPkg, GeoJSON, or CSV files (some manual steps are required, as indicated in the script).
3. **Set** your working directory to the root of this repository so that file paths resolve correctly.


---

## Outputs

1. **`OAC_Input.parquet` / `OAC_Input_IHS.parquet`** – Raw and transformed input variables for each OA (see Project Structure above: the E&W rows of `OAC_Input_IHS.parquet` are the Preview inputs).
2. **`UK_OAC_Final.parquet`** – OA-level cluster assignments (Supergroup, Group, Subgroup).
4. **Diagnostic Plots** – Boxplots, alluvial charts, and distribution comparisons in `./plot/`.

---

## How to Cite

If you use or adapt this code or classification, please cite as follows:

> Wyszomierski, J., Longley, P.A., Singleton, A.D., Gale, C. & O’Brien, O. (2024) A neighbourhood Output Area Classification from the 2021 and 2022 UK censuses. The Geographical Journal, 190, e12550. Available from: https://doi.org/10.1111/geoj.12550

Additionally, please adhere to relevant census data licenses and cite the data providers (ONS, NRS, NISRA) appropriately.

