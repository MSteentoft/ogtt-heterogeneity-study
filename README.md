This repository contains the manuscript, code and figures for the study *Heterogeneity in GLP-1, GIP and glucagon responses during an oral glucose tolerance test*.

The manuscript is built using [Quarto](https://quarto.org/) and LaTeX. The PDF layout follows the Springer Nature journal format from the [`christopherkenny/nature`](https://github.com/christopherkenny/nature) template.

* **Render PDF:** Run `quarto render manuscript.qmd` in the terminal (or click **Render** in RStudio).

#### Analysis workflow

The analysis code is organised into five R scripts that should be run in the following order:

1. `01-data-management.r` — data management and calculation of analysis variables
2. `02-clustering.r` — clustering analysis
3. `03-regression.r` — regression analyses
4. `04-sex-stratified-analyses.r` — sex-stratified analyses and interaction tests
5. `05-figures.r` — generation of main and supplementary figures

#### Tables and figures

Tables are embedded in the manuscript, while the `images/journal` folder contains the main and supplementary figures.

#### Data

The raw data is not included in this repository due to data access
and data protection restrictions.

The raw dataset filenames in the scripts reflect project-specific
data deliveries and may differ from filenames used in future data
deliveries.
