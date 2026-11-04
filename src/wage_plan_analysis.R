# Reproduce the Wage data checks and Plan figure.
# Run from the project root: Rscript src/wage_plan_analysis.R data/raw/Wage.rda
# Requires R and ggplot2. ISLR is optional; otherwise data are downloaded
# from the CRAN mirror on GitHub. The original source is documented in README.
args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep('^--file=', commandArgs(), value = TRUE)
project_root <- if (length(script_arg)) dirname(dirname(normalizePath(sub('^--file=', '', script_arg[1])))) else getwd()
out_dir <- file.path(project_root, 'results')
dir.create(file.path(out_dir, 'figures'), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, 'tables'), recursive = TRUE, showWarnings = FALSE)
if (!length(args) && file.exists(file.path(project_root, 'data/raw/Wage.rda'))) args <- file.path(project_root, 'data/raw/Wage.rda')
if (length(args)) {
  load(args[1])
} else if (requireNamespace('ISLR', quietly = TRUE)) {
  data('Wage', package = 'ISLR')
} else {
  data_file <- tempfile(fileext = '.rda')
  download.file('https://raw.githubusercontent.com/cran/ISLR/master/data/Wage.rda', data_file, mode = 'wb')
  load(data_file)
  unlink(data_file)
}
stopifnot(exists('Wage'), requireNamespace('ggplot2', quietly = TRUE))
d <- Wage
vars <- c('education', 'logwage', 'age', 'year', 'jobclass')
plot_data <- d[complete.cases(d[c('education', 'logwage')]), ]
model_data <- d[complete.cases(d[vars]), ]
model_data$a <- model_data$age - 40
model_data$education <- relevel(factor(model_data$education, ordered = FALSE), ref = '2. HS Grad')
# Fix the question, contrasts, and quadratic age specification before inspecting p-values.
M0 <- lm(logwage ~ a + I(a^2) + factor(year) + jobclass, data = model_data)
M1 <- update(M0, . ~ . + education)
M_no_job <- lm(logwage ~ education + a + I(a^2) + factor(year), data = model_data)

sink(file.path(out_dir, 'tables', 'wage_data_checks.txt'))
cat('Source: ISLR::Wage; official CRAN mirror data/Wage.rda\n')
cat('Dimensions:', dim(d), '\nYears:', sort(unique(d$year)), '\nAge range:', range(d$age), '\n')
cat('Missing values in all variables:\n'); print(colSums(is.na(d)))
cat('Raw, figure, and model samples:', nrow(d), nrow(plot_data), nrow(model_data), '\n')
cat('Maximum |logwage - log(wage)|:', max(abs(d$logwage - log(d$wage))), '\n')
cat('Region counts:\n'); print(table(d$region))
cat('Education counts:\n'); print(table(d$education))
cat('Education medians of logwage:\n'); print(tapply(d$logwage, d$education, median))
cat('Education by jobclass counts:\n'); print(table(d$education, d$jobclass))
cat('Model residual degrees of freedom:', df.residual(M1), '\n')
cat('Education joint partial F test:\n'); print(anova(M0, M1))
cat('Primary college versus high school contrast:\n')
term <- 'education4. College Grad'
print(coef(summary(M1))[term, , drop = FALSE]); print(confint(M1, term))
cat('Geometric mean percentage difference and interval:\n')
print(100 * (exp(c(estimate = coef(M1)[term], confint(M1, term))) - 1))
cat('Primary contrast without jobclass:\n'); print(coef(summary(M_no_job))[term, , drop = FALSE])
cat('Full model summary:\n'); print(summary(M1))
sink()

education_levels <- c('1. < HS Grad', '2. HS Grad', '3. Some College', '4. College Grad', '5. Advanced Degree')
labels <- c('Less than\nhigh school', 'High school\ngraduate', 'Some\ncollege', 'College\ngraduate', 'Advanced\ndegree')
plot_data$education_group <- factor(plot_data$education, levels = education_levels, labels = labels, ordered = FALSE)
stopifnot(!anyNA(plot_data$education_group))
ns <- table(plot_data$education_group)
axis_labels <- setNames(paste0(names(ns), '\n(n = ', as.integer(ns), ')'), names(ns))
library(ggplot2)
p <- ggplot(plot_data, aes(education_group, logwage)) +
  geom_point(position = position_jitter(width = 0.16, height = 0, seed = 664), alpha = 0.11, size = 0.7, color = '#355C7D') +
  geom_boxplot(width = 0.48, outlier.shape = NA, fill = '#D5E3EC', alpha = 0.8, linewidth = 0.4) +
  scale_x_discrete(labels = axis_labels) +
  labs(x = 'Educational attainment', y = 'Log wage') +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.title.x = element_text(margin = margin(t = 9)))
ggsave(file.path(out_dir, 'figures', 'wage_by_education.png'), p, width = 7.5, height = 3.8, dpi = 300, bg = 'white')
# Exploratory checks for planning; inspect rather than treating a test as proof of assumptions.
png(file.path(out_dir, 'figures', 'wage_model_diagnostics.png'), width = 1800, height = 1500, res = 180)
par(mfrow = c(2, 2)); plot(M1, which = c(1, 2, 3, 5)); dev.off()
cat('Saved data checks, Plan figure, and exploratory diagnostic plots to', out_dir, '\n')
