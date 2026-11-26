# The Simple Macroeconomics of AI Pessimism, Optimism, and the Wealth of Nations

**Replication code and data**

Raphael Auer and Josef Švéda

This repository contains the R code and data accompanying the research paper *The Simple Macroeconomics of AI Pessimism, Optimism, and the Wealth of Nations*.

## Abstract

How will AI affect the wealth of nations? We endogenise AI adoption and the gains from its use within Acemoglu’s (2025) framework. His static calibration treats the share of tasks worth automating and the savings from doing so as fixed. We ask how these change as AI improves. Firms adopt AI when the savings justify the costs of implementation. More capable AI increases savings where it is already used and makes adoption worthwhile for tasks that previously remained uneconomical. Starting from Acemoglu’s adoption estimates, based on the own-system AI deployment costs of Svanberg et al. (2024), this mechanism delivers substantially larger gains as technology advances. As AI becomes capable of performing the most demanding cognitive tasks, the estimated US GDP gain reaches 9.4%, compared with 3.4% when adoption and savings shares remain fixed. Allowing for lower implementation costs through general-purpose AI systems raises the estimated gain to 20.0%. We extend the analysis to 79 economies representing about 95% of world output, finding corresponding global GDP gains of 8.1% and 17.2% under own-system AI deployment and general-purpose AI system cost assumptions, respectively. The gains are, however, unevenly distributed: differences in economic structure and firms’ ability to cover implementation costs favour advanced, service-oriented economies, potentially widening global income gaps.

## Repository contents

The analysis is implemented in R. Its main components are:

- **`script.R`** — Main replication script, covering data preparation, model calibration, GDP calculations, and the generation of figures and tables.
- **`code/dat_cleaning/`** — Data preparation and processing scripts, including the construction of AI exposure and country-specific inputs.
- **`code/figures/`** — Functions for generating the theoretical and empirical figures.
- **`data/`** — Input datasets used in the analysis.

## Replicating the results

The analysis is designed to run within an RStudio project.

1. Clone or download this repository.
2. Open the repository's **RStudio project (`.Rproj`)** in RStudio.
3. Open **`script.R`** within the project.
4. Install the required R packages, listed at the beginning of `script.R`, if they are not already installed.
5. Run `script.R` from beginning to end.

The main script loads the data, sources the supporting R scripts, calibrates the model, and computes the results underlying the paper's figures and tables.

**Important:** Run the analysis from the project root directory to preserve the relative file paths.

## Citation

If you use the code or data, please cite the accompanying paper:

Auer, R., and Švéda, J. *The Simple Macroeconomics of AI Pessimism, Optimism, and the Wealth of Nations*.
