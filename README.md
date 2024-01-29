# Academic Audit Studies
Author: Abeba Birhane, Ryan Steed, Victor Ojewale, Briana Vecchione, and Inioluwa Deborah Raji

Data and replication code for our paper ["AI Auditing: The Broken Bus on the Road to AI Accountability"](https://arxiv.org/abs/2401.14462), SatML 2024.

## Contents
- `academic_audit_studies.csv`: Cleaned dataset of academic audit studies, 2018-2022, used in our analysis (only the studies collected using the principled methods detailed in our paper).
- `clean.R`: Script we used to clean and prune our raw spreadsheet of papers into the final list of academic audit studies analyzed in the paper.
- `analysis.Rmd`: Code for replicating our quantitative analysis, including bar plots and keyword frequency tables.
- `plots/`: Plots generated by `analysis.Rmd`, including those used in the paper.
- `keyword_analysis/`: Full dataset of 1- and 2-grams in the keywords and abstracts, generated by `analysis.Rmd`.

## Replication
All code for replicating our quantitative analysis, including bar plots and keyword frequency tables, can be found in `analysis.Rmd`. Requires [`pacman`](https://cran.r-project.org/web/packages/pacman/index.html).

## Updating the Dataset (for authors)
1. Save most recent raw database of papers as `papers.csv`.
2. Run `make academic-audit-studies.csv` (with R and `Rscript` installed).

