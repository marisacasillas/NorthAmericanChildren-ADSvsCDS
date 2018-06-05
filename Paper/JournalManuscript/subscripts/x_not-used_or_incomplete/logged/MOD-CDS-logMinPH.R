# Model of log(CDS minutes per hour):

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
# Significant contributors: adult gender
# Individual models ####
cds.mph.lg.age <-  lmer(cds.minph.lg ~ agem.c +
                      (1|Corpus) + (1|ID), data = cdsratedata)
                # no effect of child age

cds.mph.lg.cgd <-  lmer(cds.minph.lg ~ chi_gender +
                       (1|Corpus) + (1|ID), data = cdsratedata)
                # no effect of child gender

cds.mph.lg.agd <-  lmer(cds.minph.lg ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata)
                # effect of adult gender

cds.mph.lg.med <-  lmer(cds.minph.lg ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata)
                # no effect of maternal education

cds.mph.lg.myr <-  lmer(cds.minph.lg ~ mother_dob +
                       (1|Corpus) + (1|ID), data = cdsratedata)
                # no effect of maternal age

cds.mph.lg.nsb <-  lmer(cds.minph.lg ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata)
                # no effect of num siblings


# 2. 2-way interactions with adult gender (base model) ####
# Nothing to add
# Individual models ####
cds.mph.lg.agd.ageagd <-  lmer(cds.minph.lg ~ adu_gender_m +
                        agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.ageagd)
                    # no improvement

cds.mph.lg.agd.agecgd <-  lmer(cds.minph.lg ~ adu_gender_m +
                             agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agecgd)
                    # no improvement

cds.mph.lg.agd.agemed <-  lmer(cds.minph.lg ~ adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agemed)
                    # no improvement

cds.mph.lg.agd.agemyr <-  lmer(cds.minph.lg ~ adu_gender_m +
                             agem.c:mother_dob +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agemyr)
                    # no improvement

cds.mph.lg.agd.agensb <-  lmer(cds.minph.lg ~ adu_gender_m +
                             agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agensb)
                    # no improvement; but anova throws a warning

cds.mph.lg.agd.cgdagd <-  lmer(cds.minph.lg ~ adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.cgdagd)
                    # no improvement

cds.mph.lg.agd.cgdmed <-  lmer(cds.minph.lg ~ adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.cgdmed)
                    # no improvement

cds.mph.lg.agd.cgdmyr <-  lmer(cds.minph.lg ~ adu_gender_m +
                             chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.cgdmyr)
                    # no improvement

cds.mph.lg.agd.cgdnsb <-  lmer(cds.minph.lg ~ adu_gender_m +
                             chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.cgdnsb)
                    # no improvement

cds.mph.lg.agd.agdmed <-  lmer(cds.minph.lg ~ adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agdmed)
                    # no improvement

cds.mph.lg.agd.agdmyr <-  lmer(cds.minph.lg ~ adu_gender_m +
                             adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agdmyr)
                    # no improvement

cds.mph.lg.agd.agdnsb <-  lmer(cds.minph.lg ~ adu_gender_m +
                             adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agdnsb)
                    # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.mph.lg.agd.mednsb <-  lmer(cds.minph.lg ~ adu_gender_m +
                             mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.mednsb)
                    # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.mph.lg.agd.agecgdagd <- lmer(cds.minph.lg ~ adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agecgdagd)
                    # no improvement

cds.mph.lg.agd.agecgdmed <- lmer(cds.minph.lg ~ adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agecgdmed)
                    # no improvement

cds.mph.lg.agd.agecgdmyr <- lmer(cds.minph.lg ~ adu_gender_m +
                           agem.c:chi_gender:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agecgdmyr)
                    # no improvement

cds.mph.lg.agd.agecgdnsb <- lmer(cds.minph.lg ~ adu_gender_m +
                           agem.c:chi_gender:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agecgdnsb)
                    # no improvement

cds.mph.lg.agd.ageagdmed <- lmer(cds.minph.lg ~ adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.ageagdmed)
                    # no improvement

cds.mph.lg.agd.ageagdmyr <- lmer(cds.minph.lg ~ adu_gender_m +
                           agem.c:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.ageagdmyr)
                    # only marginal improvement (p=0.08454)

cds.mph.lg.agd.ageagdnsb <- lmer(cds.minph.lg ~ adu_gender_m +
                           agem.c:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.ageagdnsb)
                    # no improvement

cds.mph.lg.agd.agemednsb <- lmer(cds.minph.lg ~ adu_gender_m +
                           agem.c:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agemednsb)
                    # no improvement

cds.mph.lg.agd.cgdagdmed <- lmer(cds.minph.lg ~ adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.cgdagdmed)
                    # no improvement

cds.mph.lg.agd.cgdagdmyr <- lmer(cds.minph.lg ~ adu_gender_m +
                           chi_gender:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.cgdagdmyr)
                    # no improvement

cds.mph.lg.agd.cgdagdnsb <- lmer(cds.minph.lg ~ adu_gender_m +
                           chi_gender:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.cgdagdnsb)
                    # no improvement

cds.mph.lg.agd.cgdmednsb <- lmer(cds.minph.lg ~ adu_gender_m +
                           chi_gender:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.cgdmednsb)
                    # no improvement

cds.mph.lg.agd.agdmednsb <- lmer(cds.minph.lg ~ adu_gender_m +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata)
anova(cds.mph.lg.agd, cds.mph.lg.agd.agdmednsb)
                    # no improvement



# Best model: ####
cds.mph.lg.best <- lmer(cds.minph.lg ~ adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = cdsratedata)