# Model of sum of ADS utterances

# Available predictors:
# agem.c
# chi_gender

# mat_ed_num3


# Notes:
# In what follows I'll only report model outcomes for models that
# significatly improve upon base/previous models
# I will also avoid putting the known correlated effects (mother_dob--mat_ed_num3
# and mother_dob--number_older_sibs) in the same model

#base model#
ads.allutts.RE <-  lmer(allutts_min ~
                           (1|Corpus), data = ads_allutts_byCHI)

# 1. Single-predictor effects ####
# Significant contributor: age
# Individual models ####
ads.allutts.age <-  lmer(allutts_min ~ agem.c +
                       (1|Corpus), data = ads_allutts_byCHI)
anova(ads.allutts.age, ads.allutts.RE)
# age improves fit over RE alone

ads.allutts.cgd <-  lmer(allutts_min ~ chi_gender +
                       (1|Corpus), data = ads_allutts_byCHI)
anova(ads.allutts.cgd, ads.allutts.RE)
# no effect of child gender


ads.allutts.med <-  lmer(allutts_min ~ mat_ed_num3 +
                       (1|Corpus), data = ads_allutts_byCHI)
anova(ads.allutts.med, ads.allutts.RE)
# no effect of maternal education


# 2. 2-way interactions with agem.c (base model) ####
# age by child gender
# Individual models ####

ads.allutts.age.agecgd <-  lmer(allutts_min ~ agem.c +
                              agem.c:chi_gender +
                              (1|Corpus), data = ads_allutts_byCHI)
anova(ads.allutts.age, ads.allutts.age.agecgd)
# improved fit

ads.allutts.age.agemed <-  lmer(allutts_min ~ agem.c +
                              agem.c:mat_ed_num3 +
                              (1|Corpus), data = ads_allutts_byCHI)
anova(ads.allutts.age, ads.allutts.age.agemed)
# no improvement



ads.allutts.age.cgdmed <-  lmer(allutts_min ~ agem.c +
                              chi_gender:mat_ed_num3 +
                              (1|Corpus), data = ads_allutts_byCHI)
anova(ads.allutts.age, ads.allutts.age.cgdmed)
# no improvement



# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####

ads.allutts.age.agecgdmed <- lmer(allutts_min ~ agem.c +
                                    agem.c:chi_gender+
                                agem.c:chi_gender:mat_ed_num3 +
                                (1|Corpus),
                              data = ads_allutts_byCHI)
anova(ads.allutts.age, ads.allutts.age.agecgdmed)
# marginal improvement




# Best model: ####
ads.allutts.best <- lmer(allutts_min ~ agem.c +
                           agem.c:chi_gender +
                       (1|Corpus),
                     data = ads_allutts_byCHI)
