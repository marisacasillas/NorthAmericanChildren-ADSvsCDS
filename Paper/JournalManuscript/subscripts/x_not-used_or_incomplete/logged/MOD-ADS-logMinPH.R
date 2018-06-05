# Model of log(ADS minutes per hour):

# Available predictors:
# agem.c
# chi_gender
# adu_gender_m
# mat_ed_num3
# mother_dob
# number_older_sibs

# Notes:
# In what follows I'll only report model outcomes for models that
# significatly improve upon base/previous models
# I will also avoid putting the known correlated effects (mother_dob--mat_ed_num3
# and mother_dob--number_older_sibs) in the same model

# 1. Single-predictor effects ####
# Significant contributors: child age, adult gender, and maternal age
# Individual models ####
ads.mph.lg.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus) + (1|ID), data = adsratedata)
                # effect of child age

ads.mph.lg.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus) + (1|ID), data = adsratedata)
                # no effect of child gender

ads.mph.lg.agd <-  lmer(ads.minph.lg ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata)
                # effect of adult gender

ads.mph.lg.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = adsratedata)
                # no effect of maternal education

ads.mph.lg.myr <-  lmer(ads.minph.lg ~ mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata)
                # effect of maternal age

ads.mph.lg.nsb <-  lmer(ads.minph.lg ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = adsratedata)
                # no effect of num siblings


# 2. Age + other significant single predictors (age is the model base) ####
# Adult gender contributes significantly over child age alone
# Individual models ####
ads.mph.lg.age.agd <-  lmer(ads.minph.lg ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = adsratedata)
                    # effects of child age and adult gender
anova(ads.mph.lg.age, ads.mph.lg.age.agd)
                    # improved over previous model

ads.mph.lg.age.myr <-  lmer(ads.minph.lg ~ agem.c + mother_dob +
                       (1|Corpus) + (1|ID), data = adsratedata)
anova(ads.mph.lg.age, ads.mph.lg.age.myr)
                    # no improvement


# 3. 2-way interactions with age and other significant single predictors ####
# Adult gender and its interaction with child contributes significantly over
# child age alone
# Individual models ####
ads.mph.lg.age.agd.i1 <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
                      # effects of child age, adult gender, and their interaction
anova(ads.mph.lg.age.agd, ads.mph.lg.age.agd.i1)
                    # improved over previous model

ads.mph.lg.age.myr.i1 <- lmer(ads.minph.lg ~ agem.c * mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
                      # no effects
anova(ads.mph.lg.age.agd, ads.mph.lg.age.myr.i1)
                    # no improvement


# 4. Try as-yet untested two-way interactions ####
# Nothing to add
# Individual models ####
ads.mph.lg.age.agd.i1.agecgd <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             agem.c:chi_gender +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agecgd)
                    # no improvement

ads.mph.lg.age.agd.i1.agemed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             agem.c:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agemed)
                    # no improvement

ads.mph.lg.age.agd.i1.agemyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             agem.c:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agemyr)
                    # no improvement

ads.mph.lg.age.agd.i1.agensb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             agem.c:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agensb)
                    # no improvement

ads.mph.lg.age.agd.i1.cgdagd <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.cgdagd)
                    # no improvement
       
ads.mph.lg.age.agd.i1.cgdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.cgdmed)
                    # no improvement

ads.mph.lg.age.agd.i1.cgdmyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             chi_gender:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.cgdmyr)
                    # no improvement

ads.mph.lg.age.agd.i1.cgdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             chi_gender:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.cgdnsb)
                    # only marginal improvement (p=0.07155)

ads.mph.lg.age.agd.i1.agdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agdmed)
                    # no improvement

ads.mph.lg.age.agd.i1.agdmyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agdmyr)
                    # only marginal improvement (p=0.08752)

ads.mph.lg.age.agd.i1.agdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agdnsb)
                    # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

ads.mph.lg.age.agd.i1.mednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                             mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.mednsb)
                    # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 5. Try out three-way interactions ####
# Nothing to add
# Individual models ####
ads.mph.lg.age.agd.i1.agecgdagd <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agecgdagd)
                    # no improvement

ads.mph.lg.age.agd.i1.agecgdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agecgdmed)
                    # no improvement

ads.mph.lg.age.agd.i1.agecgdmyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agecgdmyr)
                    # no improvement

ads.mph.lg.age.agd.i1.agecgdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agecgdnsb)
                    # no improvement

ads.mph.lg.age.agd.i1.ageagdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +                           agem.c:adu_gender_m:mat_ed_num3 +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.ageagdmed)
                    # no improvement

ads.mph.lg.age.agd.i1.ageagdmyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.ageagdmyr)
                    # no improvement

ads.mph.lg.age.agd.i1.ageagdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.ageagdnsb)
                    # no improvement

ads.mph.lg.age.agd.i1.agemednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           agem.c:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agemednsb)
                    # no improvement

ads.mph.lg.age.agd.i1.cgdagdmed <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.cgdagdmed)
                    # no improvement

ads.mph.lg.age.agd.i1.cgdagdmyr <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.cgdagdmyr)
                    # no improvement

ads.mph.lg.age.agd.i1.cgdagdnsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.cgdagdnsb)
                    # no improvement

ads.mph.lg.age.agd.i1.cgdmednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           chi_gender:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.cgdmednsb)
                    # no improvement

ads.mph.lg.age.agd.i1.agdmednsb <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = adsratedata)
anova(ads.mph.lg.age.agd.i1, ads.mph.lg.age.agd.i1.agdmednsb)
                    # no improvement


# Best model: ####
ads.mph.lg.best <- lmer(ads.minph.lg ~ agem.c * adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = adsratedata)