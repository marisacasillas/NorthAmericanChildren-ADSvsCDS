# Model of ADS minutes per hour:

# Available predictors:
# agem.c
# chi_gender
# mat_ed_num3
# n_sibs
# adu_gender_m (for adsratedata.agd and adsratedata.agd.sub)

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
# - With 1 data point per child we can't include a random effect of child


#### MODEL 1: ADS MinPH overall (1 datapoint per child) ##########
# 0. Random effects only (base model) ####
ads.bas <-  lmer(ads.minph.lg ~ 1 + (1|Corpus), data = adsratedata, REML = F)

# 1. Single-predictor effects ####
# Significant contributors: child age and maternal age
# Individual models ####
ads.mph.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus), data = adsratedata, REML = F)
                # effect of child age
anova(ads.bas, ads.mph.age)
                # improved over previous model

ads.mph.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus), data = adsratedata, REML = F)
anova(ads.bas, ads.mph.cgd)
                # no improvement

ads.mph.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus), data = adsratedata, REML = F)
anova(ads.bas, ads.mph.med)
                # no improvement

ads.mph.nsb <-  lmer(ads.minph.lg ~ n_sibs +
                       (1|Corpus), data = adsratedata, REML = F)
anova(ads.bas, ads.mph.nsb)
                # no improvement

# Continue with child age as base model for comparison

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
ads.mph.age.agecgd <- lmer(ads.minph.lg ~ agem.c +
                             agem.c:chi_gender +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.agecgd)
                    # no improvement; throws convergence warning

ads.mph.age.agemed <- lmer(ads.minph.lg ~ agem.c +
                             agem.c:mat_ed_num3 +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.agemed)
                    # no improvement

ads.mph.age.agensb <- lmer(ads.minph.lg ~ agem.c +
                             agem.c:n_sibs +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.agensb)
                    # no improvement

ads.mph.age.cgdmed <- lmer(ads.minph.lg ~ agem.c +
                             chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.cgdmed)
                    # no improvement

ads.mph.age.cgdnsb <- lmer(ads.minph.lg ~ agem.c +
                             chi_gender:n_sibs +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.cgdnsb)
                    # only marginal improvement (p = 0.08914)

ads.mph.age.mednsb <- lmer(ads.minph.lg ~ agem.c +
                             mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.mednsb)
                    # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
ads.mph.age.agecgdmed <- lmer(ads.minph.lg ~ agem.c +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.agecgdmed)
                    # no improvement

ads.mph.age.agecgdnsb <- lmer(ads.minph.lg ~ agem.c +
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.agecgdnsb)
                    # no improvement

ads.mph.age.agemednsb <- lmer(ads.minph.lg ~ agem.c +
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.agemednsb)
                    # no improvement

ads.mph.age.cgdmednsb <- lmer(ads.minph.lg ~ agem.c +
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = adsratedata, REML = F)
anova(ads.mph.age, ads.mph.age.cgdmednsb)
                    # no improvement

# Best model: ####
ads.mph.best <- lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus), data = adsratedata, REML = T)


#### MODEL 2: ADS MinPH by speaker gender (2 datapoints per child) ##########
# 0. Random effects only (base model)
ads.agd.bas <-  lmer(ads.minph.lg ~ 1 + (1|Corpus) + (1|ID),
                     data = adsratedata.agd, REML = F)

# 1. Single-predictor effects ####
# Significant contributors: speaker gender
# Individual models ####
ads.agd.mph.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
                # effect of child age
anova(ads.agd.bas, ads.agd.mph.age)
                # improved over previous model

ads.agd.mph.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.bas, ads.agd.mph.cgd)
                # no improvement

ads.agd.mph.agd <-  lmer(ads.minph.lg ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
                # effect of speaker gender
anova(ads.agd.bas, ads.agd.mph.agd)
                # improvement over previous model

ads.agd.mph.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.bas, ads.agd.mph.med)
                # no improvement

ads.agd.mph.nsb <-  lmer(ads.minph.lg ~ n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.bas, ads.agd.mph.nsb)
                # no improvement

# Add both significant single predictors into a model for
# comparison with additional 2-way and 3-way effects
ads.agd.mph.age.agd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)

# 2. Try out 2-way interactions ####
# Significant additional interaction of child age and speaker gender
# Individual models ####
ads.agd.mph.age.agd.agecgd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agecgd)
                # no improvement

ads.agd.mph.age.agd.ageagd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
                # effects of child age, speaker gender, and their interaction
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.ageagd)
                # improved over previous model

ads.agd.mph.age.agd.agemed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agemed)
                # no improvement

ads.agd.mph.age.agd.agensb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agensb)
                # no improvement

ads.agd.mph.age.agd.cgdagd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.cgdagd)
                # no improvement

ads.agd.mph.age.agd.cgdmed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.cgdmed)
                # no improvement

ads.agd.mph.age.agd.cgdnsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.cgdnsb)
                # only marginal improvement (p = 0.0924)

ads.agd.mph.age.agd.agdmed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agdmed)
                # no improvement

ads.agd.mph.age.agd.agdnsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agdnsb)
                # only marginal improvement (p = 0.06321)

ads.agd.mph.age.agd.mednsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.mednsb)
                # no improvement

# Simplify syntax of best model:
ads.agd.mph.ageXagd <-  lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = F)

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
ads.agd.mph.ageXagd.agecgdagd <-lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agecgdagd)
                    # no improvement

ads.agd.mph.ageXagd.agecgdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agecgdmed)
                    # no improvement

ads.agd.mph.ageXagd.agecgdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agecgdnsb)
                    # no improvement

ads.agd.mph.ageXagd.ageagdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.ageagdmed)
                    # no improvement

ads.agd.mph.ageXagd.ageagdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.ageagdnsb)
                    # no improvement

ads.agd.mph.ageXagd.agemednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agemednsb)
                    # no improvement

ads.agd.mph.ageXagd.cgdagdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.cgdagdmed)
                    # no improvement

ads.agd.mph.ageXagd.cgdagdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.cgdagdnsb)
                    # no improvement

ads.agd.mph.ageXagd.cgdmednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.cgdmednsb)
                    # no improvement

ads.agd.mph.ageXagd.agdmednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           adu_gender_m:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd, REML = F)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agdmednsb)
                    # no improvement

# Best model: ####
ads.agd.mph.best <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd, REML = T)

#### MODEL 3: ADS MinPH by speaker gender w/ exclusions ##########
#### (up to 2 datapoints per child) ##########
# 0. Random effects only (base model)
ads.agd.s.bas <-  lmer(ads.minph.lg ~ 1 + (1|Corpus) + (1|ID),
                       data = adsratedata.agd.sub, REML = F)

# 1. Single-predictor effects ####
# Significant contributors: speaker gender
# Individual models ####
ads.agd.s.mph.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                      REML = F)
                # effect of child age
anova(ads.agd.s.bas, ads.agd.s.mph.age)
                # improved over previous model

ads.agd.s.mph.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.bas, ads.agd.s.mph.cgd)
                # no improvement

ads.agd.s.mph.agd <-  lmer(ads.minph.lg ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
                # effect of speaker gender
anova(ads.agd.s.bas, ads.agd.s.mph.agd)
                # improvement over previous model

ads.agd.s.mph.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.bas, ads.agd.s.mph.med)
                # no improvement

ads.agd.s.mph.nsb <-  lmer(ads.minph.lg ~ n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.bas, ads.agd.s.mph.nsb)
                # no improvement

# Add both significant single predictors into a model for
# comparison with additional 2-way and 3-way effects
ads.agd.s.mph.age.agd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)

# 2. Try out 2-way interactions ####
# Significant additional interaction of child age and speaker gender
# Individual models ####
ads.agd.s.mph.age.agd.agecgd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agecgd)
                # no improvement

ads.agd.s.mph.age.agd.ageagd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
                # effects of child age, speaker gender, and their interaction
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.ageagd)
                # improved over previous model

ads.agd.s.mph.age.agd.agemed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agemed)
                # no improvement

ads.agd.s.mph.age.agd.agensb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agensb)
                # no improvement

ads.agd.s.mph.age.agd.cgdagd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.cgdagd)
                # no improvement

ads.agd.s.mph.age.agd.cgdmed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.cgdmed)
                # no improvement

ads.agd.s.mph.age.agd.cgdnsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.cgdnsb)
                # only marginal improvement (p = 0.09536)

ads.agd.s.mph.age.agd.agdmed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agdmed)
                # no improvement

ads.agd.s.mph.age.agd.agdnsb <- lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agdnsb)
                # only marginal improvement (p = 0.05267)

ads.agd.s.mph.age.agd.mednsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.mednsb)
                # no improvement

# Simplify syntax of best model:
ads.agd.s.mph.ageXagd <-  lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = F)

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
ads.agd.s.mph.ageXagd.agecgdagd <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.agecgdagd)
                    # no improvement

ads.agd.s.mph.ageXagd.agecgdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.agecgdmed)
                    # no improvement

ads.agd.s.mph.ageXagd.agecgdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.agecgdnsb)
                    # no improvement

ads.agd.s.mph.ageXagd.ageagdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.ageagdmed)
                    # no improvement

ads.agd.s.mph.ageXagd.ageagdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.ageagdnsb)
                    # no improvement

ads.agd.s.mph.ageXagd.agemednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.agemednsb)
                    # no improvement

ads.agd.s.mph.ageXagd.cgdagdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.cgdagdmed)
                    # no improvement

ads.agd.s.mph.ageXagd.cgdagdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.cgdagdnsb)
                    # no improvement

ads.agd.s.mph.ageXagd.cgdmednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.cgdmednsb)
                    # no improvement

ads.agd.s.mph.ageXagd.agdmednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           adu_gender_m:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub,
                           REML = F)
anova(ads.agd.s.mph.ageXagd, ads.agd.s.mph.ageXagd.agdmednsb)
                    # no improvement

# Best model: ####
ads.agd.s.mph.best <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub,
                       REML = T)