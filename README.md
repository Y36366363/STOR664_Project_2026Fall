# STOR664 Project Fall 2026

This repository contains the STOR 664 course project on education and wages using ISLR::Wage. The active proposal was revised after exploratory data review and instructor feedback. It is an observational study of conditional associations.

## Current research question

Does the college-versus-high-school difference in mean log wage vary between Industrial and Information job classes among workers of comparable ages and recorded years?

Education and job class are the substantive variables. Age and recorded year are the adjustment variables. Job class is not treated as a generic control that automatically removes confounding. The data contain two broad job classes, not specific occupations.

## Current plan and detailed review

- `reports/plan/STOR664_Wage_Plan_revised.docx` and `.pdf`: current two-page Plan.
- `reports/plan/STOR664_Wage_Plan_draft.docx` and `.pdf`: retained first version, superseded by the revised Plan.

The revised Plan includes the member contributions and assistance statement. Review these statements for accuracy before submission.

## Repository structure

- `data/raw/Wage.rda`: unchanged upstream data snapshot; see `data/README.md`.
- `src/02_wage_design_review.R`: current exploratory design review and analysis.
- `src/wage_plan_analysis.R`: historical first-plan additive model and figure.
- `results/figures/design_review/`: composition, stratified wage distributions, adjusted contrasts, diagnostics, and an alternative age-model exploration.
- `results/tables/design_review/`: all variable frequencies, overlap checks, model specifications, OLS/HC3 contrasts, nested tests, and sensitivity results.
- `results/figures/` and `results/tables/wage_data_checks.txt`: first-plan outputs, retained as historical material.
- `reports/final/` and `slides/`: future deliverables.
- `references/STOR664_Plan_original.docx`: original preparation document.

The instructor's project handout, course notes, and Chinese design-review memo are local reference files excluded by `.gitignore`. They are not included in public release archives.

## Reproduce the current review

Requires R, ggplot2, and sandwich. Install packages once if needed:

```r
install.packages(c("ggplot2", "sandwich"))
```

From the project root:

```sh
Rscript src/02_wage_design_review.R
```

The script uses `data/raw/Wage.rda`, writes figures and tables to the `design_review/` output directories, and does not change raw data. All explored model families are retained in the output; no automatic p-value selection is performed. Recorded identical measured profiles are retained because their equality does not establish duplicate people.

To reproduce the historical first-plan model and plot:

```sh
Rscript src/wage_plan_analysis.R data/raw/Wage.rda
```

## Primary model and estimand

```r
# a = age - 40; high school and Industrial are the reference levels
logwage ~ education * jobclass + a + I(a^2) + factor(year)
```

The interaction expands to the main effects plus four education-by-jobclass terms. If the college main coefficient is b and its Information interaction is d, the adjusted college-versus-high-school gap is b in Industrial and b+d in Information. The primary contrast is d, the difference between these two log wage gaps. A secondary partial F test assesses all four interactions jointly; it cannot establish the primary contrast on its own.

The primary conventional t interval is accompanied by HC3 sensitivity inference. Within-class intervals are pointwise, not simultaneous. Exploration of age interactions is documented as an alternative question; it is not used to select a more significant main question. These analyses are not preregistered confirmatory tests.

## Current findings and limitations

Data checks found 3,000 records from 2003–2009, no missing values, and `logwage = log(wage)`. All ten education-by-jobclass cells have at least 78 observations. Education composition differs by job class, but the interaction design matrix is full rank.

The exploratory primary difference of gaps is about 0.031 log units, with an HC3 normal-approximation 95% interval of approximately [-0.030, 0.092]. This does not provide clear evidence of a college-versus-high-school gap difference and does not establish equality. The four-interaction overall test is sensitive to the variance assumptions. Diagnostics show tail departures and changing residual spread. HC3 does not resolve unmeasured confounding, misspecified mean structure, or unverified survey dependence.

The broad job categories, historical male-worker sample, unmeasured experience and background, and absent survey-design fields limit interpretation. No total or direct causal return to education is identified. Comparing models with and without job class changes the conditional comparison; it does not estimate a mediated causal proportion.

## Sources and assistance

- [Official Wage documentation](https://trevorhastie.r-universe.dev/ISLR/doc/manual.html#Wage)
- [Upstream Wage snapshot](https://github.com/cran/ISLR/blob/master/data/Wage.rda)
- [R formula syntax](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/formula.html)
- [HC3 covariance documentation](https://sandwich.r-forge.r-project.org/reference/vcovHC.html)

The package credits Steve Miller and CPS supplementary data. Its source description refers to March 2011; recorded observation years are 2003–2009. Wage units are not specified by the package manual.

Bangyu Yan identified and confirmed the choice of dataset and proposed the initial ideas and analysis plan. Jialiang Yao drew on previous course projects to develop the approach, drafted the plan, and reviewed comments and revisions across its sections.

ChatGPT assisted with design review, exploratory code, figures, and drafting, particularly with refining wording and expressing our ideas clearly. Checks included running the code and comparing data summaries and figures with the written interpretations. Team members remain responsible for reviewing and understanding the submitted work.
