# STOR664 Project Fall 2026

This repository contains work in progress for the STOR 664 course project. The current candidate investigates educational attainment and wages using the ISLR Wage dataset. Dataset selection and the draft plan remain subject to team review.

## Team members

  —— Bangyu Yan (Github: bangyuyan17-ui)
  —— Jialiang Yao (Github: Y36366363)

## Research question

How do log wages differ across educational attainment groups after adjusting for age, recorded year, and broad job class? The primary comparison is college graduates versus high school graduates. The analysis targets conditional associations rather than causal returns to education.

## Repository structure

- `data/raw/Wage.rda`: the original data snapshot used for the current analysis.
- `data/processed/`: reserved for future derived datasets.
- `src/wage_plan_analysis.R`: data checks, exploratory plots, preliminary OLS models, and diagnostics.
- `results/figures/`: the descriptive Plan figure and preliminary model diagnostics.
- `results/tables/wage_data_checks.txt`: data checks and exploratory model output.
- `reports/plan/`: editable Word and PDF drafts of the two-page Plan.
- `reports/final/` and `slides/`: reserved for the final report and presentation.
- `references/STOR664_Plan_original.docx`: the original planning document.

The instructor's project handout and the course lecture notes are retained locally and excluded by `.gitignore`. Their full text is not distributed in this repository.

## Reproduce the current analysis

Requires R and ggplot2. Install the plotting package once if necessary:

```r
install.packages("ggplot2")
```

From the repository root, run:

```sh
Rscript src/wage_plan_analysis.R data/raw/Wage.rda
```

Outputs are written to `results/figures/` and `results/tables/`. Raw data are not modified. Plot jitter uses seed 664. If no data path is supplied, the script first looks for the bundled data, then an installed ISLR package, then downloads the official data snapshot.

## Current analytical design

The proposed additive model is:

```r
logwage ~ education + a + I(a^2) + factor(year) + jobclass
```

Here `a = age - 40`, education is an unordered categorical predictor, and high school graduation is its reference category. Planned inference includes a t test and confidence interval for the college-versus-high-school contrast, plus a partial F test of the four education coefficients jointly. An overall rejection does not establish every pairwise difference.

Data checks found 3,000 records from 2003–2009, no missing values, and `logwage = log(wage)`. Preliminary Q–Q diagnostics show tail departures from normality. Robust inference, influence sensitivity, and alternative age specifications remain future work. The included model output is exploratory; the Plan does not require final inferential results.

## Draft status and assistance

The Plan is a draft. Team names, actual contributions, and the team's verification statement must be completed before submission. ChatGPT assisted with design review, text, R code, and preliminary figures. Computational checks and figure generation have been run; team members remain responsible for reviewing and understanding the submitted analysis.

## Data source

- [Official Wage documentation](https://trevorhastie.r-universe.dev/ISLR/doc/manual.html#Wage)
- [ISLR data source in the CRAN mirror](https://github.com/cran/ISLR/blob/master/data/Wage.rda)
- [ISLR package description](https://github.com/cran/ISLR/blob/master/DESCRIPTION)

The package credits Steve Miller with assembling CPS supplementary data. Its source description refers to March 2011; the actual recorded years in the supplied data are 2003–2009. The teaching data provide no survey weights or design fields. Wage units are not specified in the package manual, so the figure is labelled Log wage.
