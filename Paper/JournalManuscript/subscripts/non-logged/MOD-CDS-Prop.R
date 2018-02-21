# Model of proportion CDS:

# Available predictors:
# agem.c
# chi_gender
# mat_ed_num3
# n_sibs
# adu_gender_m (for propCDS.agd.sub)

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
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

cds.prp.nsb <-  lmer(prp.cds ~ n_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.bas, cds.prp.nsb)
                # no improvement

# Model with child age becomes our new base model

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.age.agecgd <-  lmer(prp.cds ~ agem.c +
                           agem.c:chi_gender +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.agecgd)
                # no improvement

cds.prp.age.agemed <-  lmer(prp.cds ~ agem.c +
                           agem.c:mat_ed_num3 +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.agemed)
                # no improvement

cds.prp.age.agensb <-  lmer(prp.cds ~ agem.c +
                           agem.c:n_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.agensb)
                # no improvement

cds.prp.age.cgdmed <-  lmer(prp.cds ~ agem.c +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.cgdmed)
                # no improvement

cds.prp.age.cgdnsb <-  lmer(prp.cds ~ agem.c +
                           chi_gender:n_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.cgdnsb)
                # no improvement

cds.prp.age.mednsb <-  lmer(prp.cds ~ agem.c +
                           mat_ed_num3:n_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.mednsb)
                # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.age.agecgdmed <-  lmer(prp.cds ~ agem.c +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.agecgdmed)
                # no improvement

cds.prp.age.agecgdnsb <-  lmer(prp.cds ~ agem.c +
                           agem.c:chi_gender:n_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.agecgdnsb)
                # no improvement

cds.prp.age.agemednsb <-  lmer(prp.cds ~ agem.c +
                           agem.c:mat_ed_num3:n_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.agemednsb)
                # no improvement

cds.prp.age.cgdmednsb <-  lmer(prp.cds ~ agem.c +
                           chi_gender:mat_ed_num3:n_sibs +
                       (1|Corpus), data = propCDS)
anova(cds.prp.age, cds.prp.age.cgdmednsb)
                # no improvement
                             
# Best model: ####
cds.prp.best <- cds.prp.age


#### MODEL 2: Proportion CDS by speaker gender (2 datapoints per child) ##########
# 0. Random effects only (base model) ####
cds.prp.agd.bas <-  lmer(prp.cds ~ (1|Corpus) + (1|ID),
                         data = propCDS.agd)

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

cds.prp.agd.nsb <-  lmer(prp.cds ~ n_sibs +
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

cds.prp.agd.age.agd.agensb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:n_sibs +
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

cds.prp.agd.age.agd.cgdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.cgdnsb)
                # no improvement

cds.prp.agd.age.agd.agdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agdmed)
                # no improvement

cds.prp.agd.age.agd.agdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.agdnsb)
                # no improvement

cds.prp.agd.age.agd.mednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.agd.age.agd, cds.prp.agd.age.agd.mednsb)
                # no improvement

# Simplify syntax of the best model
cds.prp.ageXagd <-  lmer(prp.cds ~ agem.c * adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.prp.agd.age.agd.ageagd.agecgdagd <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.agecgdagd)
                # no improvement

cds.prp.agd.age.agd.ageagd.agecgdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.agecgdmed)
                # no improvement

cds.prp.agd.age.agd.ageagd.agecgdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:chi_gender:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.agecgdnsb)
                # no improvement

cds.prp.agd.age.agd.ageagd.ageagdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.ageagdmed)
                # no improvement

cds.prp.agd.age.agd.ageagd.ageagdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.ageagdnsb)
                # no improvement; throws convergence warning

cds.prp.agd.age.agd.ageagd.agemednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           agem.c:mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.agemednsb)
                # no improvement

cds.prp.agd.age.agd.ageagd.cgdagdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.cgdagdmed)
                # no improvement
                             
cds.prp.agd.age.agd.ageagd.cgdagdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           chi_gender:adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.cgdagdnsb)
                # no improvement
                             
ccds.prp.agd.age.agd.ageagd.cgdmednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:mat_ed_num3:n_sibs +
                           agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, ccds.prp.agd.age.agd.ageagd.cgdmednsb)
                # no improvement
                             
cds.prp.agd.age.agd.ageagd.agdmednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m +
                           adu_gender_m:mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd)
anova(cds.prp.ageXagd, cds.prp.agd.age.agd.ageagd.agdmednsb)
                # no improvement

# Best model: ####
cds.prp.agd.best <- cds.prp.ageXagd

cds.prp.agd.best.parallel <- lmer(prp.cds ~ agem.c + adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = propCDS.agd)


#### MODEL 3: Proportion CDS by speaker gender gender w/ exclusions ##########
#### (up to 2 datapoints per child) ##########
# 0. Random effects only (base model) ####
cds.prp.agd.s.bas <-  lmer(prp.cds ~ (1|Corpus) + (1|ID),
                           data = propCDS.agd.sub)

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

cds.prp.agd.s.nsb <-  lmer(prp.cds ~ n_sibs +
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

cds.prp.agd.s.age.agd.agensb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:n_sibs +
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

cds.prp.agd.s.age.agd.cgdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdnsb)
                # no improvement

cds.prp.agd.s.age.agd.agdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agdmed)
                # no improvement

cds.prp.agd.s.age.agd.agdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agdnsb)
                # no improvement

cds.prp.agd.s.age.agd.mednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.mednsb)
                # no improvement

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

cds.prp.agd.s.age.agd.agecgdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:chi_gender:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agecgdnsb)
                # no improvement

cds.prp.agd.s.age.agd.ageagdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.ageagdmed)
                # no improvement

cds.prp.agd.s.age.agd.ageagdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.ageagdnsb)
                # no improvement

cds.prp.agd.s.age.agd.agemednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           agem.c:mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agemednsb)
                # no improvement

cds.prp.agd.s.age.agd.cgdagdmed <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdagdmed)
                # no improvement
                             
cds.prp.agd.s.age.agd.cgdagdnsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdagdnsb)
                # no improvement
                             
cds.prp.agd.s.age.agd.cgdmednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           chi_gender:mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.cgdmednsb)
                # no improvement
                             
cds.prp.agd.s.age.agd.agdmednsb <-  lmer(prp.cds ~ agem.c + adu_gender_m +
                           adu_gender_m:mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub)
anova(cds.prp.agd.s.age.agd, cds.prp.agd.s.age.agd.agdmednsb)
                # no improvement

# Best model: ####
cds.prp.agd.s.best <- cds.prp.agd.s.age.agd