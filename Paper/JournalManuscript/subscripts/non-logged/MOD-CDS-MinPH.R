# Model of CDS minutes per hour:

# Available predictors:
# agem.c
# chi_gender
# mat_ed_num3
# n_sibs
# adu_gender_m (for cdsratedata.agd and cdsratedata.agd.sub)

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
# - With 1 data point per child we can't include a random effect of child


#### MODEL 1: CDS MinPH overall (1 datapoint per child) ##########
# 0. Random effects only (base model) ####
cds.bas <-  lmer(cds.minph ~ (1|Corpus), data = cdsratedata)

# 1. Single-predictor effects ####
# Significant contributors: maternal education
# Individual models ####
cds.mph.age <-  lmer(cds.minph ~ agem.c +
                      (1|Corpus), data = cdsratedata)
anova(cds.bas, cds.mph.age)
                # no improvement

cds.mph.cgd <-  lmer(cds.minph ~ chi_gender +
                       (1|Corpus), data = cdsratedata)
anova(cds.bas, cds.mph.cgd)
                # no improvement

cds.mph.med <-  lmer(cds.minph ~ mat_ed_num3 +
                       (1|Corpus), data = cdsratedata)
                # effect of maternal education
anova(cds.bas, cds.mph.med)
                # improved over previous model

cds.mph.nsb <-  lmer(cds.minph ~ n_sibs +
                       (1|Corpus), data = cdsratedata)
anova(cds.bas, cds.mph.nsb)
                # no improvement

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.mph.med.agecgd <-  lmer(cds.minph ~ mat_ed_num3 +
                             agem.c:chi_gender +
                       (1|Corpus), data = cdsratedata)
anova(cds.mph.med, cds.mph.med.agecgd)
                    # no improvement

cds.mph.med.agemed <-  lmer(cds.minph ~ mat_ed_num3 +
                             agem.c:mat_ed_num3 +
                       (1|Corpus), data = cdsratedata)
anova(cds.mph.med, cds.mph.med.agemed)
                    # no improvement

cds.mph.med.agensb <-  lmer(cds.minph ~ mat_ed_num3 +
                             agem.c:n_sibs +
                       (1|Corpus), data = cdsratedata)
anova(cds.mph.med, cds.mph.med.agensb)
                    # no improvement

cds.mph.med.cgdmed <-  lmer(cds.minph ~ mat_ed_num3 +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus), data = cdsratedata)
anova(cds.mph.med, cds.mph.med.cgdmed)
                    # no improvement

cds.mph.med.cgdnsb <-  lmer(cds.minph ~ mat_ed_num3 +
                             chi_gender:n_sibs +
                       (1|Corpus), data = cdsratedata)
anova(cds.mph.med, cds.mph.med.cgdnsb)
                    # no improvement

cds.mph.med.mednsb <-  lmer(cds.minph ~ mat_ed_num3 +
                             mat_ed_num3:n_sibs +
                       (1|Corpus), data = cdsratedata)
anova(cds.mph.med, cds.mph.med.mednsb)
                    # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.mph.med.agecgdmed <- lmer(cds.minph ~ mat_ed_num3 +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = cdsratedata)
anova(cds.mph.med, cds.mph.med.agecgdmed)
                    # no improvement

cds.mph.med.agecgdnsb <- lmer(cds.minph ~ mat_ed_num3 +
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus),
                           data = cdsratedata)
anova(cds.mph.med, cds.mph.med.agecgdnsb)
                    # no improvement

cds.mph.med.agemednsb <- lmer(cds.minph ~ mat_ed_num3 +
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = cdsratedata)
anova(cds.mph.med, cds.mph.med.agemednsb)
                    # no improvement

cds.mph.med.cgdmednsb <- lmer(cds.minph ~ mat_ed_num3 +
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = cdsratedata)
anova(cds.mph.med, cds.mph.med.cgdmednsb)
                    # no improvement

# Best model: ####
cds.mph.best <- cds.mph.med


#### MODEL 2: CDS MinPH by speaker gender (2 datapoints per child) ##########
# 0. Random effects only (base model)
cds.agd.bas <-  lmer(cds.minph ~ (1|Corpus) + (1|ID),
                     data = cdsratedata.agd)

# 1. Single-predictor effects ####
# Significant contributors: speaker gender
# Individual models ####
cds.agd.mph.age <-  lmer(cds.minph ~ agem.c +
                      (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.bas, cds.agd.mph.age)
                # no improvement

cds.agd.mph.cgd <-  lmer(cds.minph ~ chi_gender +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.bas, cds.agd.mph.cgd)
                # no improvement

cds.agd.mph.agd <-  lmer(cds.minph ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
                # effect of speaker gender
anova(cds.agd.bas, cds.agd.mph.agd)
                # improvement over previous model

cds.agd.mph.med <-  lmer(cds.minph ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.bas, cds.agd.mph.med)
                # no improvement

cds.agd.mph.nsb <-  lmer(cds.minph ~ n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.bas, cds.agd.mph.nsb)
                # no improvement

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.agd.mph.agd.agecgd <-  lmer(cds.minph ~ adu_gender_m +
                             agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agecgd)
                # no improvement

cds.agd.mph.agd.ageagd <-  lmer(cds.minph ~ adu_gender_m +
                             agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.ageagd)
                # no improvement; throws convergence warning

cds.agd.mph.agd.agemed <-  lmer(cds.minph ~ adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agemed)
                # no improvement

cds.agd.mph.agd.agensb <-  lmer(cds.minph ~ adu_gender_m +
                             agem.c:n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agensb)
                # no improvement

cds.agd.mph.agd.cgdagd <-  lmer(cds.minph ~ adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.cgdagd)
                # no improvement

cds.agd.mph.agd.cgdmed <-  lmer(cds.minph ~ adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.cgdmed)
                # no improvement

cds.agd.mph.agd.cgdnsb <-  lmer(cds.minph ~ adu_gender_m +
                             chi_gender:n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.cgdnsb)
                # no improvement

cds.agd.mph.agd.agdmed <-  lmer(cds.minph ~ adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agdmed)
                # no improvement

cds.agd.mph.agd.agdnsb <-  lmer(cds.minph ~ adu_gender_m +
                             adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agdnsb)
                # no improvement

cds.agd.mph.agd.mednsb <-  lmer(cds.minph ~ adu_gender_m +
                             mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.mednsb)
                # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.agd.mph.agd.agecgdagd <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agecgdagd)
                    # no improvement

cds.agd.mph.agd.agecgdmed <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agecgdmed)
                    # no improvement

cds.agd.mph.agd.agecgdnsb <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agecgdnsb)
                    # no improvement

cds.agd.mph.agd.ageagdmed <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.ageagdmed)
                    # no improvement

cds.agd.mph.agd.ageagdnsb <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:adu_gender_m:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.ageagdnsb)
                    # no improvement

cds.agd.mph.agd.agemednsb <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agemednsb)
                    # no improvement

cds.agd.mph.agd.cgdagdmed <- lmer(cds.minph ~ adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.cgdagdmed)
                    # no improvement

cds.agd.mph.agd.cgdagdnsb <- lmer(cds.minph ~ adu_gender_m +
                           chi_gender:adu_gender_m:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.cgdagdnsb)
                    # no improvement

cds.agd.mph.agd.cgdmednsb <- lmer(cds.minph ~ adu_gender_m +
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.cgdmednsb)
                    # no improvement

cds.agd.mph.agd.agdmednsb <- lmer(cds.minph ~ adu_gender_m +
                           adu_gender_m:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd)
anova(cds.agd.mph.agd, cds.agd.mph.agd.agdmednsb)
                    # no improvement

# Best model: ####
cds.agd.mph.best <- cds.agd.mph.agd


#### MODEL 3: CDS MinPH by speaker gender w/ exclusions ##########
#### (up to 2 datapoints per child) ##########
# 0. Random effects only (base model)
cds.agd.s.bas <-  lmer(cds.minph ~ (1|Corpus) + (1|ID),
                       data = cdsratedata.agd.sub)

# 1. Single-predictor effects ####
# Significant contributors: speaker gender
# Individual models ####
cds.agd.s.mph.age <-  lmer(cds.minph ~ agem.c +
                      (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.bas, cds.agd.s.mph.age)
                # no improvement

cds.agd.s.mph.cgd <-  lmer(cds.minph ~ chi_gender +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.bas, cds.agd.s.mph.cgd)
                # no improvement

cds.agd.s.mph.agd <-  lmer(cds.minph ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
                # effect of speaker gender
anova(cds.agd.s.bas, cds.agd.s.mph.agd)
                # improvement over previous model

cds.agd.s.mph.med <-  lmer(cds.minph ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.bas, cds.agd.s.mph.med)
                # no improvement

cds.agd.s.mph.nsb <-  lmer(cds.minph ~ n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.bas, cds.agd.s.mph.nsb)
                # no improvement

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.agd.s.mph.agd.agecgd <-  lmer(cds.minph ~ adu_gender_m +
                             agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agecgd)
                # no improvement

cds.agd.s.mph.agd.ageagd <-  lmer(cds.minph ~ adu_gender_m +
                             agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.ageagd)
                # no improvement

cds.agd.s.mph.agd.agemed <-  lmer(cds.minph ~ adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agemed)
                # no improvement

cds.agd.s.mph.agd.agensb <-  lmer(cds.minph ~ adu_gender_m +
                             agem.c:n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agensb)
                # no improvement

cds.agd.s.mph.agd.cgdagd <-  lmer(cds.minph ~ adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.cgdagd)
                # no improvement

cds.agd.s.mph.agd.cgdmed <-  lmer(cds.minph ~ adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.cgdmed)
                # no improvement

cds.agd.s.mph.agd.cgdnsb <-  lmer(cds.minph ~ adu_gender_m +
                             chi_gender:n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.cgdnsb)
                # no improvement

cds.agd.s.mph.agd.agdmed <-  lmer(cds.minph ~ adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agdmed)
                # no improvement

cds.agd.s.mph.agd.agdnsb <-  lmer(cds.minph ~ adu_gender_m +
                             adu_gender_m:n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agdnsb)
                # no improvement

cds.agd.s.mph.agd.mednsb <-  lmer(cds.minph ~ adu_gender_m +
                             mat_ed_num3:n_sibs +
                       (1|Corpus) + (1|ID), data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.mednsb)
                # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.agd.s.mph.agd.agecgdagd <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agecgdagd)
                    # no improvement

cds.agd.s.mph.agd.agecgdmed <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agecgdmed)
                    # no improvement

cds.agd.s.mph.agd.agecgdnsb <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agecgdnsb)
                    # no improvement

cds.agd.s.mph.agd.ageagdmed <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.ageagdmed)
                    # no improvement

cds.agd.s.mph.agd.ageagdnsb <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:adu_gender_m:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.ageagdnsb)
                    # no improvement

cds.agd.s.mph.agd.agemednsb <- lmer(cds.minph ~ adu_gender_m +
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agemednsb)
                    # no improvement

cds.agd.s.mph.agd.cgdagdmed <- lmer(cds.minph ~ adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.cgdagdmed)
                    # no improvement

cds.agd.s.mph.agd.cgdagdnsb <- lmer(cds.minph ~ adu_gender_m +
                           chi_gender:adu_gender_m:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.cgdagdnsb)
                    # no improvement

cds.agd.s.mph.agd.cgdmednsb <- lmer(cds.minph ~ adu_gender_m +
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.cgdmednsb)
                    # no improvement

cds.agd.s.mph.agd.agdmednsb <- lmer(cds.minph ~ adu_gender_m +
                           adu_gender_m:mat_ed_num3:n_sibs +
                           (1|Corpus) + (1|ID),
                           data = cdsratedata.agd.sub)
anova(cds.agd.s.mph.agd, cds.agd.s.mph.agd.agdmednsb)
                    # no improvement

# Best model: ####
cds.agd.s.mph.best <- cds.agd.s.mph.agd