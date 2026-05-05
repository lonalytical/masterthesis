# Handling Missing Data in Multilevel Models with Few Clusters

This repository contains the files used for the simulation study reported in the master thesis.

## Folder structure

Only the main folders and files are shown here. Temporary rendering files, cache files, and automatically generated project files are omitted for readability.

``` text
masterthesis/
├── appendices/
├── backmatter/
├── bibliography/
├── chapters/
├── code/
│   └── functions/
├── figures/
├── images/
├── renv/
├── results/
├── _quarto.yml
├── index.qmd
├── references.qmd
├── renv.lock
├── session-info.txt
├── masterthesis.Rproj
└── Handling-Missing-Data-in-Multilevel-Models-with-Few-Clusters--A-Simulation-Study.pdf
```

## Folder descriptions

| Path | Description |
|------------------------------------|------------------------------------|
| `appendices/` | Quarto files for the appendices, including derivations, convergence diagnostics, complete simulation results, and sensitivity analyses. |
| `backmatter/` | LaTeX files used for final declarations and backmatter material. |
| `bibliography/` | Bibliography file used for citation management. |
| `chapters/` | Main thesis chapters written as Quarto files. |
| `code/` | R scripts used for data simulation, missing-data generation, model fitting, result aggregation, and plotting. |
| `code/functions/` | Custom R functions used by the main analysis scripts. |
| `figures/` | Figures included in the thesis. |
| `images/` | Additional image files, such as logos. |
| `renv/` and `renv.lock` | Files documenting the R package environment for reproducibility. |
| `results/` | Simulation results, convergence objects, prior-sensitivity results, and aggregated result tables. |

## Main files

| File | Description |
|---|---|
| `_quarto.yml` | Quarto project configuration file. |
| `masterthesis.Rproj` | RStudio project file. |
| `code/simulation_script.R` | Main script for running the simulation workflow. |
| `code/functions/simulate-datasets_function.R` | Custom function for generating simulated two-level data sets. |
| `code/summary_script.R` | Script for aggregating the simulation results. |
| `results/summarized_simulation-results.csv` | Aggregated simulation results used for the main result tables and figures. |
| `session-info.txt` | Information about the R session and package versions used when rendering the thesis. |
| `Handling-Missing-Data-in-Multilevel-Models-with-Few-Clusters--A-Simulation-Study.pdf` | Rendered PDF version of the thesis. |

## Reproducibility

The project uses `renv` to document the R package environment. After opening the R project, the package environment can be restored with:

``` r
renv::restore()
```
