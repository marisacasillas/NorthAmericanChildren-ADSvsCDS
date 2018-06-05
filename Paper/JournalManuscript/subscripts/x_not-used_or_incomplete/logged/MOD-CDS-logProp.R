# Model of proportion CDS:

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
cds.prp.lg.age <-  lmer(prp.cds.lg ~ agem.c +
                      (1|Corpus) + (1|ID), data = propCDS)
                # effect of child age

cds.prp.lg.cgd <-  lmer(prp.cds.lg ~ chi_gender +
                       (1|Corpus) + (1|ID), data = propCDS)
                # no effect of child gender

cds.prp.lg.agd <-  lmer(prp.cds.lg ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS)
                # effect of adult gender

cds.prp.lg.med <-  lmer(prp.cds.lg ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS)
                # no effect of maternal education (near-marginal)

cds.prp.lg.myr <-  lmer(prp.cds.lg ~ mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS)
                # no effect of maternal age

cds.prp.lg.nsb <-  lmer(prp.cds.lg ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS)
                # no effect of num siblings


# 2. Age + other significant single predictors (age is the model base) ####
# Adult gender contributes significantly over child age alone
# Individual models ####
cds.prp.lg.age.agd <-  lmer(prp.cds.lg ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS)
                    # effects of child age and adult gender
anova(cds.prp.lg.age, cds.prp.lg.age.agd)
                    # improved over previous model


# 3. 2-way interactions with age and other significant single predictors ####
# Adult gender and its interaction with child contributes significantly over
# child age alone
# Individual models ####
cds.prp.lg.age.agd.i1 <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           (1|Corpus) + (1|ID), data = propCDS)
                      # effects of child age, adult gender, and their interaction
anova(cds.prp.lg.age.agd, cds.prp.lg.age.agd.i1)
                    # improved over previous model


# 4. Try as-yet untested two-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.lg.age.agd.i1.agecgd <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agecgd)
                    # no improvement

cds.prp.lg.age.agd.i1.agemed <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agemed)
                    # no improvement

cds.prp.lg.age.agd.i1.agemyr <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             agem.c:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agemyr)
                    # no improvement

cds.prp.lg.age.agd.i1.agensb <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agensb)
                    # no improvement

cds.prp.lg.age.agd.i1.cgdagd <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.cgdagd)
                    # no improvement

cds.prp.lg.age.agd.i1.cgdmed <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.cgdmed)
                    # no improvement

cds.prp.lg.age.agd.i1.cgdmyr <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.cgdmyr)
                    # no improvement

cds.prp.lg.age.agd.i1.cgdnsb <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.cgdnsb)
                    # no improvement

cds.prp.lg.age.agd.i1.agdmed <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agdmed)
                    # no improvement

cds.prp.lg.age.agd.i1.agdmyr <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agdmyr)
                    # no improvement

cds.prp.lg.age.agd.i1.agdnsb <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agdnsb)
                    # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.prp.lg.age.agd.i1.mednsb <-  lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                             mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.mednsb)
                    # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 5. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.lg.age.agd.i1.agecgdagd <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agecgdagd)
                    # no improvement

cds.prp.lg.age.agd.i1.agecgdmed <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agecgdmed)
                    # no improvement

cds.prp.lg.age.agd.i1.agecgdmyr <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:mother_dob +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agecgdmyr)
                    # no improvement

cds.prp.lg.age.agd.i1.agecgdnsb <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           agem.c:chi_gender:number_older_sibs +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agecgdnsb)
                    # no improvement

cds.prp.lg.age.agd.i1.ageagdmed <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.ageagdmed)
                    # no improvement

cds.prp.lg.age.agd.i1.ageagdmyr <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.ageagdmyr)
                    # no improvement

cds.prp.lg.age.agd.i1.ageagdnsb <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           agem.c:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.ageagdnsb)
                    # no improvement

cds.prp.lg.age.agd.i1.agemednsb <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           agem.c:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agemednsb)
                    # no improvement

cds.prp.lg.age.agd.i1.cgdagdmed <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.cgdagdmed)
                    # no improvement

cds.prp.lg.age.agd.i1.cgdagdmyr <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:mother_dob +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.cgdagdmyr)
                    # no improvement

cds.prp.lg.age.agd.i1.cgdagdnsb <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           chi_gender:adu_gender_m:number_older_sibs +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.cgdagdnsb)
                    # no improvement

cds.prp.lg.age.agd.i1.cgdmednsb <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           chi_gender:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.cgdmednsb)
                    # no improvement

cds.prp.lg.age.agd.i1.agdmednsb <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                           (1|Corpus) + (1|ID), data = propCDS)
anova(cds.prp.lg.age.agd.i1, cds.prp.lg.age.agd.i1.agdmednsb)
                    # no improvement


# Best model: ####
cds.prp.lg.best <- lmer(prp.cds.lg ~ agem.c * adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = propCDS)