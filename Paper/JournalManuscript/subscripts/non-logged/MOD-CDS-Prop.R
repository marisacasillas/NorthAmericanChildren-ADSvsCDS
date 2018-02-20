# Model of proportion CDS:

# Available predictors:
# agem.c
# chi_gender
# mat_ed_num3
# mother_dob
# number_older_sibs
# adu_gender_m (for propCDS.agd.sub)

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
# - I will also avoid putting the known correlated effects (mother_dob--mat_ed_num3
#   and mother_dob--number_older_sibs) in the same model
# - With 1 data point per child we can't include a random effect of child


#### MODEL 1: proportion CDS overall (1 datapoint per child) ##########
# 0. Random effects only (base model) ####
cds.prp.bas <-  lmer(prp.cds ~ (1|Corpus), data = propCDS)


# 1. Single-predictor effects ####
# Significant contributors: child age and mother age
# Individual models ####
cds.prp.age <-  lmer(prp.cds ~ agem.c +
                      (1|Corpus), data = propCDS)
                # effect of child age
anova(cds.prp.bas, cds.prp.age)
                # improved over previous model

cds.prp.cgd <-  lmer(prp.cds ~ chi_gender +
                       (1|Corpus), data = propCDS)
anova(cds.prp.bas, cds.prp.cgd)
                # no improvement

cds.prp.med <-  lmer(prp.cds ~ mat_ed_num3 +
                       (1|Corpus), data = propCDS)
anova(cds.prp.bas, cds.prp.med)
                # no improvement

cds.prp.myr <-  lmer(prp.cds ~ mother_dob +
                       (1|Corpus), data = propCDS)
                # effect of maternal age
anova(cds.prp.bas, cds.prp.myr)
                # improved over previous model

cds.prp.nsb <-  lmer(prp.cds ~ number_older_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.bas, cds.prp.nsb)
                # no improvement

# Add both significant single predictors into a model for
# comparison with additional 2-way and 3-way effects
cds.prp.age.myr <-  lmer(prp.cds ~ agem.c + mother_dob +
                       (1|Corpus), data = propCDS)


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.age.myr.agecgd <-  lmer(prp.cds ~ agem.c + mother_dob +
                           agem.c:chi_gender +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.agecgd)
                # no improvement

cds.prp.age.myr.agemed <-  lmer(prp.cds ~ agem.c + mother_dob +
                           agem.c:mat_ed_num3 +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.agemed)
                # no improvement

cds.prp.age.myr.agemyr <-  lmer(prp.cds ~ agem.c + mother_dob +
                           agem.c:mother_dob +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.agemyr)
                # no improvement

cds.prp.age.myr.agensb <-  lmer(prp.cds ~ agem.c + mother_dob +
                           agem.c:number_older_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.agensb)
                # no improvement

cds.prp.age.myr.cgdmed <-  lmer(prp.cds ~ agem.c + mother_dob +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.cgdmed)
                # no improvement

cds.prp.age.myr.cgdmyr <-  lmer(prp.cds ~ agem.c + mother_dob +
                           chi_gender:mother_dob +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.cgdmyr)
                # no improvement

cds.prp.age.myr.cgdnsb <-  lmer(prp.cds ~ agem.c + mother_dob +
                           chi_gender:number_older_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.cgdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.prp.age.myr.mednsb <-  lmer(prp.cds ~ agem.c + mother_dob +
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.age.myr.agecgdmed <-  lmer(prp.cds ~ agem.c + mother_dob +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.agecgdmed)
                # no improvement

cds.prp.age.myr.agecgdmyr <-  lmer(prp.cds ~ agem.c + mother_dob +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.agecgdmyr)
                # no improvement

cds.prp.age.myr.agecgdnsb <-  lmer(prp.cds ~ agem.c + mother_dob +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.agecgdnsb)
                # no improvement

cds.prp.age.myr.agemednsb <-  lmer(prp.cds ~ agem.c + mother_dob +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.agemednsb)
                # no improvement

cds.prp.age.myr.cgdmednsb <-  lmer(prp.cds ~ agem.c + mother_dob +
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age.myr, cds.prp.age.myr.cgdmednsb)
                # no improvement
                             

# Best model: ####
cds.prp.best <- lmer(prp.cds ~ agem.c + mother_dob +
                 (1|Corpus),
                 data = propCDS)



#### MODEL 2: Proportion CDS by speaker gender (2 datapoints per child) ##########
# 0. Random effects only (base model) ####
cds.prp.agd.bas <-  lmer(prp.cds ~ (1|Corpus) + (1|ID), data = propCDS.agd)


# 1. Single-predictor effects ####
# Significant contributors: child age and mother age
# Individual models ####
cds.prp.agd.age <-  lmer(prp.cds ~ agem.c +
                      (1|Corpus) + (1|ID), data = propCDS.agd)
                # effect of child age
anova(cds.prp.agd.bas, cds.prp.agd.age)
                # improved over previous model

cds.prp.agd.cgd <-  lmer(prp.cds ~ chi_gender +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.bas, cds.prp.agd.cgd)
                # no improvement

cds.prp.agd.agd <-  lmer(prp.cds ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
                # effect of speaker gender
anova(cds.prp.agd.bas, cds.prp.agd.agd)
                # improved over previous model

cds.prp.agd.med <-  lmer(prp.cds ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.bas, cds.prp.agd.med)
                # no improvement

cds.prp.agd.myr <-  lmer(prp.cds ~ mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.bas, cds.prp.agd.myr)
                # no improvement

cds.prp.agd.nsb <-  lmer(prp.cds ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.bas, cds.prp.agd.nsb)
                # no improvement

# Add both significant single predictors into a model for
# comparison with additional 2-way and 3-way effects
cds.prp.agd.age.agd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.agd.age.agd.agecgd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agecgd)
                # no improvement

cds.prp.agd.age.agd.ageagd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
                # effect of child age, speaker gender, and a two-way
                # interaction between child age and speaker gender
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.ageagd)
                # improved over previous model

cds.prp.agd.age.agd.agemed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agemed)
                # no improvement

cds.prp.agd.age.agd.agemyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agemyr)
                # no improvement

cds.prp.agd.age.agd.agensb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agensb)
                # no improvement

cds.prp.agd.age.agd.cgdagd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.cgdagd)
                # no improvement

cds.prp.agd.age.agd.cgdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.cgdmed)
                # no improvement

cds.prp.agd.age.agd.cgdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.cgdmyr)
                # no improvement

cds.prp.agd.age.agd.cgdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.cgdnsb)
                # no improvement

cds.prp.agd.age.agd.agdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agdmed)
                # no improvement

cds.prp.agd.age.agd.agdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agdmyr)
                # no improvement

cds.prp.agd.age.agd.agdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.prp.agd.age.agd.mednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.agd.age.agd.ageagd.agecgdagd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.ageagd.agecgdagd)
                # no improvement

cds.prp.agd.age.agd.ageagd.agecgdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.ageagd.agecgdmed)
                # no improvement

cds.prp.agd.age.agd.ageagd.agecgdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.agecgdmyr)
                # no improvement

cds.prp.agd.age.agd.ageagd.agecgdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.agecgdnsb)
                # no improvement

cds.prp.agd.age.agd.ageagd.ageagdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.ageagdmed)
                # no improvement

cds.prp.agd.age.agd.ageagd.ageagdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.ageagdmyr)
                # no improvement

cds.prp.agd.age.agd.ageagd.ageagdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.ageagdnsb)
                # no improvement

cds.prp.agd.age.agd.ageagd.agemednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.agemednsb)
                # no improvement

cds.prp.agd.age.agd.ageagd.cgdagdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.cgdagdmed)
                # no improvement
                             
cds.prp.agd.age.agd.ageagd.cgdagdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           chi_gender:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.cgdagdmyr)
                # no improvement
                             
cds.prp.agd.age.agd.ageagd.cgdagdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           chi_gender:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.cgdagdnsb)
                # no improvement
                             
ccds.prp.agd.age.agd.ageagd.cgdmednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:mat_ed_num3:number_older_sibs +
                           agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.cgdmednsb)
                # no improvement
                             
cds.prp.agd.age.agd.ageagd.agdmednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd.ageagd, cds.prp.agd.age.agd.agdmednsb)
                # no improvement


# Best model: ####
cds.prp.agd.best <- lmer(prp.cds ~ agem.c + adu_gender_m +
                  agem.c:adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = propCDS.agd)

cds.prp.agd.best.parallel <- lmer(prp.cds ~ agem.c + adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = propCDS.agd)



#### MODEL 3: Proportion CDS by speaker gender gender w/ exclusions ##########
#### (up to 2 datapoints per child) ##########
# 0. Random effects only (base model) ####
cds.prp.agd.s.bas <-  lmer(prp.cds ~ (1|Corpus) + (1|ID), data = propCDS.agd.sub)


# 1. Single-predictor effects ####
# Significant contributors: child age and mother age
# Individual models ####
cds.prp.agd.s.age <-  lmer(prp.cds ~ agem.c +
                      (1|Corpus) + (1|ID), data = propCDS.agd.sub)
                # effect of child age
anova(cds.prp.agd.s.bas, cds.prp.agd.s.age)
                # improved over previous model

cds.prp.agd.s.cgd <-  lmer(prp.cds ~ chi_gender +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.bas, cds.prp.agd.s.cgd)
                # no improvement

cds.prp.agd.s.agd <-  lmer(prp.cds ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
                # effect of speaker gender
anova(cds.prp.agd.s.bas, cds.prp.agd.s.agd)
                # improved over previous model

cds.prp.agd.s.med <-  lmer(prp.cds ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.bas, cds.prp.agd.s.med)
                # no improvement

cds.prp.agd.s.myr <-  lmer(prp.cds ~ mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.bas, cds.prp.agd.s.myr)
                # no improvement

cds.prp.agd.s.nsb <-  lmer(prp.cds ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.bas, cds.prp.agd.s.nsb)
                # no improvement

# Add both significant single predictors into a model for
# comparison with additional 2-way and 3-way effects
cds.prp.agd.s.age.agd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.agd.s.age.agd.agecgd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agecgd)
                # no improvement

cds.prp.agd.s.age.agd.ageagd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.ageagd)
                # no improvement

cds.prp.agd.s.age.agd.agemed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agemed)
                # no improvement

cds.prp.agd.s.age.agd.agemyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agemyr)
                # no improvement

cds.prp.agd.s.age.agd.agensb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agensb)
                # no improvement

cds.prp.agd.s.age.agd.cgdagd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdagd)
                # no improvement

cds.prp.agd.s.age.agd.cgdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdmed)
                # no improvement

cds.prp.agd.s.age.agd.cgdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdmyr)
                # no improvement

cds.prp.agd.s.age.agd.cgdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdnsb)
                # no improvement

cds.prp.agd.s.age.agd.agdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agdmed)
                # no improvement

cds.prp.agd.s.age.agd.agdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agdmyr)
                # no improvement

cds.prp.agd.s.age.agd.agdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.prp.agd.s.age.agd.mednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.agd.s.age.agd.agecgdagd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
                # model is rank deficient
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agecgdagd)
                # no improvement

cds.prp.agd.s.age.agd.agecgdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agecgdmed)
                # no improvement

cds.prp.agd.s.age.agd.agecgdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agecgdmyr)
                # no improvement

cds.prp.agd.s.age.agd.agecgdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agecgdnsb)
                # no improvement

cds.prp.agd.s.age.agd.ageagdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.ageagdmed)
                # no improvement

cds.prp.agd.s.age.agd.ageagdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.ageagdmyr)
                # no improvement

cds.prp.agd.s.age.agd.ageagdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.ageagdnsb)
                # no improvement

cds.prp.agd.s.age.agd.agemednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agemednsb)
                # no improvement

cds.prp.agd.s.age.agd.cgdagdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdagdmed)
                # no improvement
                             
cds.prp.agd.s.age.agd.cgdagdmyr <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdagdmyr)
                # no improvement
                             
cds.prp.agd.s.age.agd.cgdagdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdagdnsb)
                # no improvement
                             
cds.prp.agd.s.age.agd.cgdmednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdmednsb)
                # no improvement
                             
cds.prp.agd.s.age.agd.agdmednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agdmednsb)
                # no improvement


# Best model: ####
cds.prp.agd.s.best <- lmer(prp.cds ~ agem.c + adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = propCDS.agd.sub)
