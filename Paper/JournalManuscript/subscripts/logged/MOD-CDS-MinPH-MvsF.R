# Model of CDS minutes per hour:

# Available predictors:
# agem.c (or age.bin or age.qrt)
# chi_gender
# mat_ed_num3
# n_sibs

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
# - With 1 data point per child we can't include a random effect of child

# Separate models by speaker gender ####
f.cdsrd <- filter(cdsratedata.agd.sub, adu_gender_m == "FEMALE")
m.cdsrd <- filter(cdsratedata.agd.sub, adu_gender_m == "MALE")


# FEMALES ####
# 0. Random effects only (base model) ####
fcd.bas <-  lmer(cds.minph.lg ~ 1 + (1|Corpus), data = f.cdsrd, REML = F)

# 1. Single-predictor effects ####
# Significant contributors: adult gender
# Individual models ####
fcd.mph.age <-  lmer(cds.minph.lg ~ agem.c +
                      (1|Corpus), data = f.cdsrd, REML = F)
                # effect of child age
anova(fcd.bas, fcd.mph.age)
                # no improvement

fcd.mph.cgd <-  lmer(cds.minph.lg ~ chi_gender +
                       (1|Corpus), data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.cgd)
                # no improvement

fcd.mph.med <-  lmer(cds.minph.lg ~ mat_ed_num3 +
                       (1|Corpus), data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.med)
                # no improvement

fcd.mph.nsb <-  lmer(cds.minph.lg ~ n_sibs +
                       (1|Corpus), data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.nsb)
                # no improvement

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
fcd.mph.agecgd <- lmer(cds.minph.lg ~
                             agem.c:chi_gender +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.agecgd)
                    # no improvement

fcd.mph.agemed <- lmer(cds.minph.lg ~
                             agem.c:mat_ed_num3 +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.agemed)
                    # no improvement

fcd.mph.agensb <- lmer(cds.minph.lg ~
                             agem.c:n_sibs +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.agensb)
                    # no improvement

fcd.mph.cgdmed <- lmer(cds.minph.lg ~
                             chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.cgdmed)
                    # no improvement

fcd.mph.cgdnsb <- lmer(cds.minph.lg ~
                             chi_gender:n_sibs +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.cgdnsb)
                    # no improvement

fcd.mph.mednsb <- lmer(cds.minph.lg ~
                             mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.mednsb)
                    # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
fcd.mph.agecgdmed <- lmer(cds.minph.lg ~
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.agecgdmed)
                    # no improvement

fcd.mph.agecgdnsb <- lmer(cds.minph.lg ~
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.agecgdnsb)
                    # no improvement

fcd.mph.agemednsb <- lmer(cds.minph.lg ~
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.agemednsb)
                    # no improvement

fcd.mph.cgdmednsb <- lmer(cds.minph.lg ~
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = f.cdsrd, REML = F)
anova(fcd.bas, fcd.mph.cgdmednsb)
                    # no improvement

# Best model: ####
fcd.mph.best <- lmer(cds.minph.lg ~ 1 + (1|Corpus), data = f.cdsrd, REML = T)


# MALES ####
# 0. Random effects only (base model) ####
mcd.bas <-  lmer(cds.minph.lg ~ 1 + (1|Corpus), data = m.cdsrd, REML = F)

# 1. Single-predictor effects ####
# Significant contributors: adult gender
# Individual models ####
mcd.mph.age <-  lmer(cds.minph.lg ~ agem.c +
                      (1|Corpus), data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.age)
                # no improvement

mcd.mph.cgd <-  lmer(cds.minph.lg ~ chi_gender +
                       (1|Corpus), data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.cgd)
                # no improvement

mcd.mph.med <-  lmer(cds.minph.lg ~ mat_ed_num3 +
                       (1|Corpus), data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.med)
                # no improvement

mcd.mph.nsb <-  lmer(cds.minph.lg ~ n_sibs +
                       (1|Corpus), data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.nsb)
                # no improvement

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
mcd.mph.agecgd <- lmer(cds.minph.lg ~
                             agem.c:chi_gender +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.agecgd)
                    # no improvement

mcd.mph.agemed <- lmer(cds.minph.lg ~
                             agem.c:mat_ed_num3 +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.agemed)
                    # no improvement

mcd.mph.agensb <- lmer(cds.minph.lg ~
                             agem.c:n_sibs +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.agensb)
                    # no improvement

mcd.mph.cgdmed <- lmer(cds.minph.lg ~
                             chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.cgdmed)
                    # no improvement

mcd.mph.cgdnsb <- lmer(cds.minph.lg ~
                             chi_gender:n_sibs +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.cgdnsb)
                    # no improvement

mcd.mph.mednsb <- lmer(cds.minph.lg ~
                             mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.mednsb)
                    # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
mcd.mph.agecgdmed <- lmer(cds.minph.lg ~
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.agecgdmed)
                    # no improvement

mcd.mph.agecgdnsb <- lmer(cds.minph.lg ~
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.agecgdnsb)
                    # no improvement

mcd.mph.agemednsb <- lmer(cds.minph.lg ~
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.agemednsb)
                    # no improvement

mcd.mph.cgdmednsb <- lmer(cds.minph.lg ~
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = m.cdsrd, REML = F)
anova(mcd.bas, mcd.mph.cgdmednsb)
                    # no improvement

# Best model: ####
mcd.mph.best <- lmer(cds.minph.lg ~ 1 + (1|Corpus), data = m.cdsrd, REML = T)