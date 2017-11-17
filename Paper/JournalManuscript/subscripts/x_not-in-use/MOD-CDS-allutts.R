# Model of sum of CDS utterances

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
cds.allutts.RE <-  lmer(allutts_min ~
                          (1|Corpus), data = cds_allutts_byCHI)

# 1. Single-predictor effects ####
# Significant contributor: age alone and maternal ed alone are better than just RE
# Individual models ####
cds.allutts.age <-  lmer(allutts_min ~ agem.c +
                           (1|Corpus), data = cds_allutts_byCHI)
anova(cds.allutts.age, cds.allutts.RE)
# age improves fit over RE alone

cds.allutts.cgd <-  lmer(allutts_min ~ chi_gender +
                           (1|Corpus), data = cds_allutts_byCHI)
anova(cds.allutts.cgd, cds.allutts.RE)
# no effect of child gender


cds.allutts.med <-  lmer(allutts_min ~ mat_ed_num3 +
                           (1|Corpus), data = cds_allutts_byCHI)
anova(cds.allutts.med, cds.allutts.RE)
# maternal education improves fit over RE alone


# 2. 2-way interactions with agem.c (base model) ####
# age + med by cgd interaction
# Individual models ####

cds.allutts.age.agecgd <-  lmer(allutts_min ~ agem.c +
                                  agem.c:chi_gender +
                                  (1|Corpus), data = cds_allutts_byCHI)
anova(cds.allutts.age, cds.allutts.age.agecgd)
# no improvement

cds.allutts.age.agemed <-  lmer(allutts_min ~ agem.c +
                                  agem.c:mat_ed_num3 +
                                  (1|Corpus), data = cds_allutts_byCHI)
anova(cds.allutts.age, cds.allutts.age.agemed)
# marginal improvement for interaction of age & med 

cds.allutts.med.agemed <-  lmer(allutts_min ~ mat_ed_num3 +
                                  agem.c:mat_ed_num3 +
                                  (1|Corpus), data = cds_allutts_byCHI)
anova(cds.allutts.med, cds.allutts.med.agemed)

cds.allutts.age.cgdmed <-  lmer(allutts_min ~ agem.c +
                                  chi_gender:mat_ed_num3 +
                                  (1|Corpus), data = cds_allutts_byCHI)
anova(cds.allutts.age, cds.allutts.age.cgdmed)
# improved fit


cds.allutts.med.cgdmed <-  lmer(allutts_min ~ mat_ed_num3 +
                                  chi_gender:mat_ed_num3 +
                                  (1|Corpus), data = cds_allutts_byCHI)
anova(cds.allutts.med, cds.allutts.med.cgdmed)
#  no improvement


# 3. Try out three-way interactions ####
# eb: not actually sure how to do this
# Individual models ####

cds.allutts.age.agecgdmed <- lmer(allutts_min ~ agem.c +
                                    chi_gender:mat_ed_num3+
                                    agem.c:chi_gender:mat_ed_num3 +
                                    (1|Corpus),
                                  data = cds_allutts_byCHI)
anova(cds.allutts.age, cds.allutts.age.agecgdmed)
# marginal improvement


# Best model: ####
cds.allutts.best <- lmer(allutts_min ~ agem.c +
                           agem.c:chi_gender +
                           (1|Corpus),
                         data = cds_allutts_byCHI)
