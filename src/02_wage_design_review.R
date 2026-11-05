# Exploratory design review for STOR 664; no automatic model selection.
# From the project root: Rscript src/02_wage_design_review.R
# Required packages: ggplot2, sandwich. All model comparisons are exploratory.
script_arg <- grep('^--file=', commandArgs(), value = TRUE)
project_root <- if (length(script_arg)) dirname(dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', script_arg[1]), fixed = TRUE)))) else getwd()
load(file.path(project_root, 'data/raw/Wage.rda'))
stopifnot(requireNamespace('ggplot2', quietly = TRUE), requireNamespace('sandwich', quietly = TRUE))
figdir <- file.path(project_root, 'results/figures/design_review')
tabdir <- file.path(project_root, 'results/tables/design_review')
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)
dir.create(tabdir, recursive = TRUE, showWarnings = FALSE)
write_table <- function(x, name) write.csv(x, file.path(tabdir, paste0(name, '.csv')), row.names = FALSE)
raw <- Wage
d <- raw
d$education <- relevel(factor(d$education, ordered = FALSE), ref = '2. HS Grad')
d$jobclass <- factor(d$jobclass)
d$a <- d$age - 40
education_order <- levels(raw$education)
education_labels <- c('Less than\nhigh school', 'High school\ngraduate', 'Some\ncollege', 'College\ngraduate', 'Advanced\ndegree')

inventory <- data.frame(variable = names(raw), storage = vapply(raw, function(x) class(x)[1], ''), observed_values = vapply(raw, function(x) length(unique(x)), 1L), missing = colSums(is.na(raw)))
write_table(inventory, 'variable_inventory')
freq <- do.call(rbind, lapply(names(raw)[vapply(raw, is.factor, TRUE)], function(n) {
  t <- table(raw[[n]])
  data.frame(variable = n, level = names(t), n = as.integer(t), percent = 100 * as.integer(t)/nrow(raw))
}))
write_table(freq, 'categorical_frequencies')

cells <- expand.grid(education = education_order, jobclass = levels(d$jobclass), stringsAsFactors = FALSE)
cell_stats <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  s <- d[d$education == cells$education[i] & d$jobclass == cells$jobclass[i], ]
  data.frame(cells[i, ], n = nrow(s), mean_logwage = mean(s$logwage), median_logwage = median(s$logwage), sd_logwage = sd(s$logwage), mean_age = mean(s$age), age_min = min(s$age), age_q05 = as.numeric(quantile(s$age, .05)), age_median = median(s$age), age_q95 = as.numeric(quantile(s$age, .95)), age_max = max(s$age))
}))
write_table(cell_stats, 'education_jobclass_cells')
tab <- table(factor(raw$education, levels = education_order), raw$jobclass)
composition <- data.frame(education = rownames(tab), industrial_n = tab[,1], information_n = tab[,2], total = rowSums(tab), information_percent = 100*tab[,2]/rowSums(tab), row.names = NULL)
write_table(composition, 'education_jobclass_composition')
d$age_band <- cut(d$age, breaks = c(17,24,34,44,54,64,80), labels = c('18–24','25–34','35–44','45–54','55–64','65–80'))
write_table(as.data.frame(table(education=d$education,jobclass=d$jobclass,age_band=d$age_band)), 'age_support_counts')
write_table(as.data.frame(table(education=d$education,jobclass=d$jobclass,year=d$year)), 'year_support_counts')
# Exact matching measured profiles do not establish duplicate persons; retain them.
dups <- duplicated(raw) | duplicated(raw, fromLast = TRUE)
write_table(cbind(source_rowname=rownames(raw)[dups], raw[dups, , drop=FALSE]), 'matching_profiles_retained')

specs <- list(
  unadjusted = logwage ~ education,
  age_year = logwage ~ education + a + I(a^2) + factor(year),
  job_additive = logwage ~ education + a + I(a^2) + factor(year) + jobclass,
  job_interaction = logwage ~ education * jobclass + a + I(a^2) + factor(year),
  age_linear_interaction = logwage ~ education * a + I(a^2) + factor(year),
  age_quadratic_interaction = logwage ~ education * (a + I(a^2)) + factor(year),
  extra_covariates_illustration = logwage ~ education + a + I(a^2) + factor(year) + jobclass + race + maritl + health + health_ins
)
models <- lapply(specs, function(f) lm(formula = f, data = d))
metrics <- do.call(rbind, lapply(names(models), function(n) {
 m <- models[[n]]; data.frame(model=n, n=nobs(m), parameters=m$rank, columns=ncol(model.matrix(m)), R_squared=summary(m)$r.squared, adjusted_R_squared=summary(m)$adj.r.squared, formula=paste(deparse(formula(m)),collapse=' '))
}))
write_table(metrics, 'model_specifications')
term <- 'education4. College Grad'
interaction_term <- 'education4. College Grad:jobclass2. Information'
contrast <- function(m, weights, label, model_name) {
 L <- setNames(rep(0,length(coef(m))),names(coef(m))); L[names(weights)] <- weights
 b <- sum(L*coef(m))
 do.call(rbind,lapply(c('OLS t interval','HC3 normal approximation'),function(method){
   v <- if(method=='OLS t interval') vcov(m) else sandwich::vcovHC(m,type='HC3')
   se <- sqrt(drop(t(L)%*%v%*%L)); critical <- if(method=='OLS t interval') qt(.975,df.residual(m)) else qnorm(.975)
   p <- if(method=='OLS t interval') 2*pt(-abs(b/se),df.residual(m)) else 2*pnorm(-abs(b/se))
   data.frame(model=model_name,contrast=label,method=method,estimate=b,se=se,lower=b-critical*se,upper=b+critical*se,p_value=p,exp_estimate=exp(b),exp_lower=exp(b-critical*se),exp_upper=exp(b+critical*se))
 }))
}
contrasts <- do.call(rbind,lapply(c('unadjusted','age_year','job_additive','extra_covariates_illustration'), function(n) contrast(models[[n]], setNames(1,term),'College minus high school',n)))
m <- models$job_interaction
contrasts <- rbind(contrasts,contrast(m,setNames(1,term),'Industrial college minus high school','job_interaction'),contrast(m,setNames(c(1,1),c(term,interaction_term)),'Information college minus high school','job_interaction'),contrast(m,setNames(1,interaction_term),'Difference of gaps Information minus Industrial','job_interaction'))
write_table(contrasts,'contrasts_OLS_and_HC3')

nested <- function(reduced,full,label){
 x<-anova(reduced,full)
 idx<-setdiff(names(coef(full)),names(coef(reduced)))
 b<-coef(full)[idx];v<-sandwich::vcovHC(full,type='HC3')[idx,idx,drop=FALSE]
 W<-drop(t(b)%*%solve(v,b))
 data.frame(comparison=label,restrictions=length(idx),classical_F=x$F[2],classical_p=x$`Pr(>F)`[2],HC3_Wald_chisq=W,HC3_asymptotic_p=pchisq(W,length(idx),lower.tail=FALSE))
}
tests <- rbind(nested(models$job_additive,models$job_interaction,'Four education by jobclass interactions'),nested(models$age_year,models$age_linear_interaction,'Four education by linear age interactions'),nested(models$age_year,models$age_quadratic_interaction,'Eight education by age curve interactions'))
write_table(tests,'exploratory_nested_tests')

# Sensitivity: preserve the target contrast while varying specified modelling assumptions.
sensitivity_models <- list(
  main_job_interaction = m,
  age25to64 = lm(specs$job_interaction, data=d[d$age>=25 & d$age<=64,]),
  additional_race = update(m,.~.+race),
  cubic_age = update(m,.~.+I(a^3)),
  education_specific_age_curves = update(m,.~.+education:a+education:I(a^2)),
  job_specific_age_curves = update(m,.~.+jobclass:a+jobclass:I(a^2))
)
cd <- cooks.distance(m)
# Flagging threshold is a diagnostic convention, not a data-cleaning rule.
sensitivity_models$exclude_Cook_flagged_stress_test <- lm(specs$job_interaction,data=d[cd<=4/nrow(d),])
sens <- do.call(rbind,lapply(names(sensitivity_models),function(n) {
 s<-contrast(sensitivity_models[[n]],setNames(1,interaction_term),'Difference of gaps Information minus Industrial',n)
 s$n<-nobs(sensitivity_models[[n]]);s
}))
write_table(sens,'primary_interaction_sensitivity')
# Age interactions describe cross-sectional age differences, not individual wage growth.
age_contrasts <- do.call(rbind,lapply(c(30,40,50,60),function(age){
 n<-'age_quadratic_interaction';s<-contrast(models[[n]],setNames(c(1,age-40,(age-40)^2),c(term,paste0(term,':a'),paste0(term,':I(a^2)'))),paste('College minus high school at age',age),n);s$age<-age;s
}))
write_table(age_contrasts,'alternative_age_question_contrasts')

# Descriptive diagnostics; HC3 addresses unequal variance, not confounding or survey dependence.
diagnostic <- data.frame(model=names(sensitivity_models), n=vapply(sensitivity_models,nobs,1L), max_Cook=vapply(sensitivity_models,function(x)max(cooks.distance(x)),0), max_leverage=vapply(sensitivity_models,function(x)max(hatvalues(x)),0))
write_table(diagnostic,'diagnostic_summary')
flagged <- data.frame(source_rowname=rownames(d),Cook=cd,leverage=hatvalues(m),studentized_residual=rstudent(m))
write_table(flagged[order(flagged$Cook,decreasing=TRUE),][1:15,],'largest_influence_records')

library(ggplot2)
comp_long <- as.data.frame(prop.table(tab,1))
names(comp_long)<-c('education','jobclass','fraction')
comp_long$education<-factor(comp_long$education,levels=education_order,labels=education_labels)
comp_long$jobclass<-factor(comp_long$jobclass,labels=c('Industrial','Information'))
p_comp <- ggplot(comp_long,aes(education,fraction,fill=jobclass)) + geom_col(width=.66,color='white',linewidth=.3) + geom_text(aes(label=paste0(round(100*fraction,1),'%')),position=position_stack(vjust=.5),color='white',size=3.5) + scale_fill_manual(values=c('#405A72','#3D8B89')) + scale_y_continuous(labels=function(x)paste0(100*x,'%'),expand=expansion(mult=c(0,.02))) + labs(x='Educational attainment',y='Within education group',fill='Job class') + theme_minimal(base_size=11) + theme(panel.grid.major.x=element_blank(),legend.position='top')
ggsave(file.path(figdir,'education_jobclass_composition.png'),p_comp,width=7.2,height=3.3,dpi=300,bg='white')

plot_d <- d
plot_d$education_display<-factor(plot_d$education,levels=education_order,labels=education_labels)
plot_d$job_display<-factor(plot_d$jobclass,labels=c('Industrial','Information'))
fig_caption <- paste('Cell sizes in education order: Industrial 190, 636, 342, 274, 102;', 'Information 78, 335, 308, 411, 324.')
p_box <- ggplot(plot_d,aes(education_display,logwage,fill=job_display)) + geom_boxplot(width=.6,outlier.alpha=.18,outlier.size=.6,linewidth=.35) + facet_wrap(~job_display,nrow=1) + scale_fill_manual(values=c('#D5E3EC','#CDE6E1'),guide='none') + labs(x='Educational attainment',y='Log wage') + theme_minimal(base_size=10.5) + theme(panel.grid.major.x=element_blank(),strip.text=element_text(face='bold'))
ggsave(file.path(figdir,'logwage_by_education_jobclass.png'),p_box,width=7.2,height=3.2,dpi=300,bg='white')

point <- contrasts[contrasts$model=='job_interaction' & contrasts$method=='HC3 normal approximation' & contrasts$contrast!='Difference of gaps Information minus Industrial',]
point$jobclass<-c('Industrial','Information')
p_gap <- ggplot(point,aes(jobclass,estimate,ymin=lower,ymax=upper,color=jobclass)) + geom_hline(yintercept=0,color='grey65',linetype=2) + geom_pointrange(linewidth=.7,size=.6) + scale_color_manual(values=c('#405A72','#3D8B89'),guide='none') + labs(x='Job class',y='College minus high school
mean log wage difference',caption='Pointwise 95% HC3 intervals. Compare gaps with the interaction contrast, not overlap of intervals.') + theme_minimal(base_size=11)
ggsave(file.path(figdir,'adjusted_college_gaps_by_jobclass.png'),p_gap,width=7.2,height=3.5,dpi=300,bg='white')

# Primary-question diagnostics.
png(file.path(figdir,'job_interaction_diagnostics.png'),width=2000,height=1500,res=180)
par(mfrow=c(2,2));plot(m,which=c(1,2,3,5));dev.off()

# Show alternative age model without implying longitudinal experience effects.
grid <- expand.grid(age=seq(25,65,length.out=81),education=levels(d$education),year=2006)
grid$a<-grid$age-40
grid$education<-factor(grid$education,levels=levels(d$education))
grid$fit<-predict(models$age_quadratic_interaction,newdata=grid)
grid$education<-factor(grid$education,levels=education_order,labels=gsub('\n',' ',education_labels))
p_age <- ggplot(grid,aes(age,fit,color=education)) + geom_line(linewidth=.7) + labs(x='Age',y='Fitted mean log wage',color='Education',subtitle='Exploratory cross-sectional curves at year 2006
No job-class adjustment; not individual wage growth') + theme_minimal(base_size=11) + guides(color=guide_legend(nrow=2,byrow=TRUE)) + theme(legend.position='bottom',legend.title=element_blank(),legend.text=element_text(size=9),plot.subtitle=element_text(size=9))
ggsave(file.path(figdir,'alternative_age_curves.png'),p_age,width=7.2,height=4.8,dpi=300,bg='white')

# A concise audit log records all explored model families instead of selecting by p-value.
sink(file.path(tabdir,'review_summary.txt'))
cat('EXPLORATORY DESIGN REVIEW\nAll findings inform a revised plan; no preregistration or untouched confirmatory sample is claimed.\n')
cat('Raw dimensions:',dim(raw),' Missing:',sum(is.na(raw)),' Exact matching measured profiles beyond first:',sum(duplicated(raw)),'\n')
cat('Identical measured profiles do not establish duplicate people; all records retained.\n')
cat('Main interaction design matrix rank/columns:',m$rank,ncol(model.matrix(m)),'\n')
chi<-chisq.test(tab)
cat('Education/jobclass descriptive chi-square:',unname(chi$statistic),' df:',unname(chi$parameter),' p:',chi$p.value,' Cramer V:',sqrt(unname(chi$statistic)/sum(tab)),'\n')
cat('Minimum education by jobclass cell:',min(tab),' Minimum education by jobclass by year cell:',min(table(d$education,d$jobclass,d$year)),'\n')
cat('Cook distance 4/n flags:',sum(cd>4/nrow(d)),' of ',nrow(d),' retained in primary model.\n')
print(composition);print(metrics);print(tests);print(contrasts);print(sens)
cat('\nSession information\n');print(sessionInfo())
sink()
cat('Design review outputs written to results/figures/design_review and results/tables/design_review\n')
