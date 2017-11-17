# Model of ADS minutes per hour:

# Available predictors:
# agem.c
# chi_gender
# mat_ed_num3
# mother_dob
# number_older_sibs
# adu_gender_m (for adsratedata.agd and adsratedata.agd.sub)

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
# - I will also avoid putting the known correlated effects (mother_dob--mat_ed_num3
#   and mother_dob--number_older_sibs) in the same model
# - With 1 data point per child we can't include a random effect of child


#### MODEL 1: ADS MinPH overall (1 datapoint per child) ##########
# 0. Random effects only (base model) ####
ads.bas <-  lmer(ads.minph.lg ~ (1|Corpus), data = adsratedata)


# 1. Single-predictor effects ####
# Significant contributors: child age and maternal age
# Individual models ####
ads.mph.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus), data = adsratedata)
                # effect of child age
anova(ads.bas, ads.mph.age)
                # improved over previous model

ads.mph.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus), data = adsratedata)
anova(ads.bas, ads.mph.cgd)
                # no improvement

ads.mph.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus), data = adsratedata)
anova(ads.bas, ads.mph.med)
                # no improvement

ads.mph.myr <-  lmer(ads.minph.lg ~ mother_dob +
                       (1|Corpus), data = adsratedata)
                # effect of maternal age
anova(ads.bas, ads.mph.myr)
                # improved over previous model

ads.mph.nsb <-  lmer(ads.minph.lg ~ number_older_sibs +
                       (1|Corpus), data = adsratedata)
anova(ads.bas, ads.mph.nsb)
                # no improvement

# Add both significant single predictors into a model for
# comparison with additional 2-way and 3-way effects
ads.mph.age.myr <-  lmer(ads.minph.lg ~ agem.c + mother_dob +
                       (1|Corpus), data = adsratedata)


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
ads.mph.age.myr.agecgd <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                             agem.c:chi_gender +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.agecgd)
                    # no improvement

ads.mph.age.myr.agemed <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                             agem.c:mat_ed_num3 +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.agemed)
                    # no improvement

ads.mph.age.myr.agemyr <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                             agem.c:mother_dob +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.agemyr)
                    # no improvement

ads.mph.age.myr.agensb <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                             agem.c:number_older_sibs +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.agensb)
                    # no improvement

ads.mph.age.myr.cgdmed <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                             chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.cgdmed)
                    # no improvement

ads.mph.age.myr.cgdmyr <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                             chi_gender:mother_dob +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.cgdmyr)
                    # no improvement

ads.mph.age.myr.cgdnsb <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                             chi_gender:number_older_sibs +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.cgdnsb)
                    # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

ads.mph.age.myr.mednsb <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                             mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.mednsb)
                    # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
ads.mph.age.myr.agecgdmed <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.agecgdmed)
                    # no improvement

ads.mph.age.myr.agecgdmyr <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                           agem.c:chi_gender:mother_dob +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.agecgdmyr)
                    # no improvement

ads.mph.age.myr.agecgdnsb <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                           agem.c:chi_gender:number_older_sibs +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.agecgdnsb)
                    # no improvement

ads.mph.age.myr.agemednsb <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                           agem.c:mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.agemednsb)
                    # no improvement

ads.mph.age.myr.cgdmednsb <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                           chi_gender:mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = adsratedata)
anova(ads.mph.age.myr, ads.mph.age.myr.cgdmednsb)
                    # no improvement


# Best model: ####
ads.mph.best <- lmer(ads.minph.lg ~ agem.c + mother_dob +
                 (1|Corpus),
                 data = adsratedata)


#### MODEL 2: ADS MinPH by speaker gender (2 datapoints per child) ##########
# 0. Random effects only (base model)
ads.agd.bas <-  lmer(ads.minph.lg ~ (1|Corpus) + (1|ID),
                  data = adsratedata.agd)


# 1. Single-predictor effects ####
# Significant contributors: speaker gender
# Individual models ####
ads.agd.mph.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus) + (1|ID), data = adsratedata.agd)
                # effect of child age
anova(ads.agd.bas, ads.agd.mph.age)
                # improved over previous model

ads.agd.mph.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.bas, ads.agd.mph.cgd)
                # no improvement

ads.agd.mph.agd <-  lmer(ads.minph.lg ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
                # effect of speaker gender
anova(ads.agd.bas, ads.agd.mph.agd)
                # improvement over previous model

ads.agd.mph.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.bas, ads.agd.mph.med)
                # no improvement

ads.agd.mph.myr <-  lmer(ads.minph.lg ~ mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.bas, ads.agd.mph.myr)
                # only marginal improvement (p = 0.08504)

ads.agd.mph.nsb <-  lmer(ads.minph.lg ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.bas, ads.agd.mph.nsb)
                # no improvement

# Add both significant single predictors into a model for
# comparison with additional 2-way and 3-way effects
ads.agd.mph.age.agd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)


# 2. Try out 2-way interactions ####
# Significant additional interaction of child age and speaker gender
# Individual models ####
ads.agd.mph.age.agd.agecgd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agecgd)
                # no improvement

ads.agd.mph.age.agd.ageagd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
                # effects of child age, speaker gender, and their interaction
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.ageagd)
                # improved over previous model

ads.agd.mph.age.agd.agemed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agemed)
                # no improvement

ads.agd.mph.age.agd.agemyr <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agemyr)
                # no improvement

ads.agd.mph.age.agd.agensb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agensb)
                # no improvement

ads.agd.mph.age.agd.cgdagd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.cgdagd)
                # no improvement

ads.agd.mph.age.agd.cgdmed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.cgdmed)
                # no improvement

ads.agd.mph.age.agd.cgdmyr <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.cgdmyr)
                # no improvement

ads.agd.mph.age.agd.cgdnsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.cgdnsb)
                # only marginal improvement (p = 0.06532)

ads.agd.mph.age.agd.agdmed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agdmed)
                # no improvement

ads.agd.mph.age.agd.agdmyr <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agdmyr)
                # no improvement

ads.agd.mph.age.agd.agdnsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.agdnsb)
                # only marginal improvement (p = 0.06095)

# No model with mat_ed_num3:mother_dob because they are correlated

ads.agd.mph.age.agd.mednsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)
anova(ads.agd.mph.age.agd, ads.agd.mph.age.agd.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated

# Simplify syntax of best model:
ads.agd.mph.ageXagd <-  lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd)



# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
ads.agd.mph.ageXagd.agecgdagd <-lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agecgdagd)
                    # no improvement

ads.agd.mph.ageXagd.agecgdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agecgdmed)
                    # no improvement

ads.agd.mph.ageXagd.agecgdmyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agecgdmyr)
                    # no improvement

ads.agd.mph.ageXagd.agecgdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agecgdnsb)
                    # no improvement

ads.agd.mph.ageXagd.ageagdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.ageagdmed)
                    # no improvement

ads.agd.mph.ageXagd.ageagdmyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.ageagdmyr)
                    # no improvement

ads.agd.mph.ageXagd.ageagdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.ageagdnsb)
                    # no improvement

ads.agd.mph.ageXagd.agemednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agemednsb)
                    # no improvement

ads.agd.mph.ageXagd.cgdagdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.cgdagdmed)
                    # no improvement

ads.agd.mph.ageXagd.cgdagdmyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.cgdagdmyr)
                    # no improvement

ads.agd.mph.ageXagd.cgdagdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.cgdagdnsb)
                    # no improvement

ads.agd.mph.ageXagd.cgdmednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.cgdmednsb)
                    # no improvement

ads.agd.mph.ageXagd.agdmednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd)
anova(ads.agd.mph.ageXagd, ads.agd.mph.ageXagd.agdmednsb)
                    # no improvement


# Best model: ####
ads.agd.mph.best <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = adsratedata.agd)


#### MODEL 3: ADS MinPH by speaker gender w/ exclusions ##########
#### (up to 2 datapoints per child) ##########
# 0. Random effects only (base model)
ads.agd.s.bas <-  lmer(ads.minph.lg ~ (1|Corpus) + (1|ID),
                       data = adsratedata.agd.sub)


# 1. Single-predictor effects ####
# Significant contributors: speaker gender
# Individual models ####
ads.agd.s.mph.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
                # effect of child age
anova(ads.agd.s.bas, ads.agd.s.mph.age)
                # improved over previous model

ads.agd.s.mph.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.bas, ads.agd.s.mph.cgd)
                # no improvement

ads.agd.s.mph.agd <-  lmer(ads.minph.lg ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
                # effect of speaker gender
anova(ads.agd.s.bas, ads.agd.s.mph.agd)
                # improvement over previous model

ads.agd.s.mph.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.bas, ads.agd.s.mph.med)
                # no improvement

ads.agd.s.mph.myr <-  lmer(ads.minph.lg ~ mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.bas, ads.agd.s.mph.myr)
                # no improvement

ads.agd.s.mph.nsb <-  lmer(ads.minph.lg ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.bas, ads.agd.s.mph.nsb)
                # no improvement

# Add both significant single predictors into a model for
# comparison with additional 2-way and 3-way effects
ads.agd.s.mph.age.agd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)


# 2. Try out 2-way interactions ####
# Significant additional interaction of child age and speaker gender
# and of speaker gender and the number of older siblings
# Individual models ####
ads.agd.s.mph.age.agd.agecgd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agecgd)
                # no improvement

ads.agd.s.mph.age.agd.ageagd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
                # effects of child age, speaker gender, and their interaction
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.ageagd)
                # improved over previous model

ads.agd.s.mph.age.agd.agemed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agemed)
                # no improvement

ads.agd.s.mph.age.agd.agemyr <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agemyr)
                # no improvement

ads.agd.s.mph.age.agd.agensb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agensb)
                # no improvement

ads.agd.s.mph.age.agd.cgdagd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.cgdagd)
                # no improvement

ads.agd.s.mph.age.agd.cgdmed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.cgdmed)
                # no improvement

ads.agd.s.mph.age.agd.cgdmyr <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.cgdmyr)
                # no improvement

ads.agd.s.mph.age.agd.cgdnsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.cgdnsb)
                # only marginal improvement (p = 0.06523)

ads.agd.s.mph.age.agd.agdmed <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agdmed)
                # no improvement

ads.agd.s.mph.age.agd.agdmyr <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agdmyr)
                # no improvement

ads.agd.s.mph.age.agd.agdnsb <- lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
                # effects of child age, speaker gender, and a two-way
                # interaction of speaker gender and # older sibs
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.agdnsb)
                # improved over previous model

# No model with mat_ed_num3:mother_dob because they are correlated

ads.agd.s.mph.age.agd.mednsb <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                             mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)
anova(ads.agd.s.mph.age.agd, ads.agd.s.mph.age.agd.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated

# Simplify syntax of best model:
ads.agd.s.mph.ageXagd.agdnsb <-  lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata.agd.sub)


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
ads.agd.s.mph.ageXagd.agdnsb.agecgdagd <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.agecgdagd)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.agecgdmed <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.agecgdmed)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsbagecgdmyr <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           agem.c:chi_gender:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsbagecgdmyr)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.agecgdnsb <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           agem.c:chi_gender:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.agecgdnsb)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.ageagdmed <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.ageagdmed)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.ageagdmyr <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           agem.c:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.ageagdmyr)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsbageagdnsb <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           agem.c:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsbageagdnsb)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.agemednsb <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           agem.c:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.agemednsb)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.cgdagdmed <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.cgdagdmed)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.cgdagdmyr <- lmer(ads.minph.lg ~ 
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           chi_gender:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.cgdagdmyr)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.cgdagdnsb <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           chi_gender:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.cgdagdnsb)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.cgdmednsb <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           chi_gender:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.cgdmednsb)
                    # no improvement

ads.agd.s.mph.ageXagd.agdnsb.agdmednsb <- lmer(ads.minph.lg ~
                           agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata.agd.sub)
anova(ads.agd.s.mph.ageXagd.agdnsb, ads.agd.s.mph.ageXagd.agdnsb.agdmednsb)
                    # no improvement


# Best model: ####
ads.agd.s.mph.best <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                            (1|Corpus) + (1|ID),
                            data = adsratedata.agd.sub)

# Matching model to non-logged best model: ####
ads.agd.s.mph.match <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                            (1|Corpus) + (1|ID),
                            data = adsratedata.agd.sub)
