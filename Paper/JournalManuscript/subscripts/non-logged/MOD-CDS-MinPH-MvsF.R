# Model of CDS minutes per hour:

# Available predictors:
# agem.c (or age.bin or age.qrt)
# chi_gender
# mat_ed_num3
# mother_dob
# number_older_sibs

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
# - I will also avoid putting the known correlated effects (mother_dob--mat_ed_num3
#   and mother_dob--number_older_sibs) in the same model
# - With 1 data point per child we can't include a random effect of child

# Separate models by speaker gender ####
f.cdsrd <- filter(cdsratedata.agd.sub, adu_gender_m == "FEMALE")
m.cdsrd <- filter(cdsratedata.agd.sub, adu_gender_m == "MALE")


# FEMALES ####
# 0. Random effects only (base model) ####
fcd.bas <-  lmer(cds.minph ~ (1|Corpus), data = f.cdsrd)

# 1. Single-predictor effects ####
# Significant contributors: adult gender
# Individual models ####
fcd.mph.age <-  lmer(cds.minph ~ agem.c +
                      (1|Corpus), data = f.cdsrd)
                # effect of child age
anova(fcd.bas, fcd.mph.age)
                # only marginal improvement (p = 0.07721)

fcd.mph.cgd <-  lmer(cds.minph ~ chi_gender +
                       (1|Corpus), data = f.cdsrd)
anova(fcd.bas, fcd.mph.cgd)
                # no improvement

fcd.mph.med <-  lmer(cds.minph ~ mat_ed_num3 +
                       (1|Corpus), data = f.cdsrd)
anova(fcd.bas, fcd.mph.med)
                # no improvement

fcd.mph.myr <-  lmer(cds.minph ~ mother_dob +
                       (1|Corpus), data = f.cdsrd)
anova(fcd.bas, fcd.mph.myr)
                # no improvement

fcd.mph.nsb <-  lmer(cds.minph ~ number_older_sibs +
                       (1|Corpus), data = f.cdsrd)
anova(fcd.bas, fcd.mph.nsb)
                # no improvement; anova throws convergence warning


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
fcd.mph.agecgd <- lmer(cds.minph ~
                             agem.c:chi_gender +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.agecgd)
                    # no improvement

fcd.mph.agemed <- lmer(cds.minph ~
                             agem.c:mat_ed_num3 +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.agemed)
                    # no improvement

fcd.mph.agemyr <- lmer(cds.minph ~
                             agem.c:mother_dob +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.agemyr)
                    # only marginal improvement (p = 0.09376)

fcd.mph.agensb <- lmer(cds.minph ~
                             agem.c:number_older_sibs +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.agensb)
                    # no improvement

fcd.mph.cgdmed <- lmer(cds.minph ~
                             chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.cgdmed)
                    # no improvement

fcd.mph.cgdmyr <- lmer(cds.minph ~
                             chi_gender:mother_dob +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.cgdmyr)
                    # no improvement

fcd.mph.cgdnsb <- lmer(cds.minph ~
                             chi_gender:number_older_sibs +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.cgdnsb)
                    # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

fcd.mph.mednsb <- lmer(cds.minph ~
                             mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.mednsb)
                    # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
fcd.mph.agecgdmed <- lmer(cds.minph ~
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.agecgdmed)
                    # no improvement

fcd.mph.agecgdmyr <- lmer(cds.minph ~
                           agem.c:chi_gender:mother_dob +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.agecgdmyr)
                    # no improvement

fcd.mph.agecgdnsb <- lmer(cds.minph ~
                           agem.c:chi_gender:number_older_sibs +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.agecgdnsb)
                    # no improvement

fcd.mph.agemednsb <- lmer(cds.minph ~
                           agem.c:mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.agemednsb)
                    # no improvement

fcd.mph.cgdmednsb <- lmer(cds.minph ~
                           chi_gender:mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = f.cdsrd)
anova(fcd.bas, fcd.mph.cgdmednsb)
                    # no improvement


# Best model: ####
fcd.mph.best <- lmer(cds.minph ~
                 (1|Corpus),
                 data = f.cdsrd)


# MALES ####
# 0. Random effects only (base model) ####
mcd.bas <-  lmer(cds.minph ~ (1|Corpus), data = m.cdsrd)


# 1. Single-predictor effects ####
# Significant contributors: adult gender
# Individual models ####
mcd.mph.age <-  lmer(cds.minph ~ agem.c +
                      (1|Corpus), data = m.cdsrd)
anova(mcd.bas, mcd.mph.age)
                # no improvement

mcd.mph.cgd <-  lmer(cds.minph ~ chi_gender +
                       (1|Corpus), data = m.cdsrd)
anova(mcd.bas, mcd.mph.cgd)
                # no improvement

mcd.mph.med <-  lmer(cds.minph ~ mat_ed_num3 +
                       (1|Corpus), data = m.cdsrd)
anova(mcd.bas, mcd.mph.med)
                # no improvement; anova throws convergence warning

mcd.mph.myr <-  lmer(cds.minph ~ mother_dob +
                       (1|Corpus), data = m.cdsrd)
anova(mcd.bas, mcd.mph.myr)
                # no improvement

mcd.mph.nsb <-  lmer(cds.minph ~ number_older_sibs +
                       (1|Corpus), data = m.cdsrd)
anova(mcd.bas, mcd.mph.nsb)
                # no improvement


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
mcd.mph.agecgd <- lmer(cds.minph ~
                             agem.c:chi_gender +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.agecgd)
                    # no improvement

mcd.mph.agemed <- lmer(cds.minph ~
                             agem.c:mat_ed_num3 +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.agemed)
                    # no improvement

mcd.mph.agemyr <- lmer(cds.minph ~
                             agem.c:mother_dob +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.agemyr)
                    # no improvement

mcd.mph.agensb <- lmer(cds.minph ~
                             agem.c:number_older_sibs +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.agensb)
                    # no improvement; anova throws convergence warning

mcd.mph.cgdmed <- lmer(cds.minph ~
                             chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.cgdmed)
                    # no improvement

mcd.mph.cgdmyr <- lmer(cds.minph ~
                             chi_gender:mother_dob +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.cgdmyr)
                    # no improvement

mcd.mph.cgdnsb <- lmer(cds.minph ~
                             chi_gender:number_older_sibs +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.cgdnsb)
                    # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

mcd.mph.mednsb <- lmer(cds.minph ~
                             mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.mednsb)
                    # no improvement

# No model with mother_dob:number_older_sibs because they are correlated



# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
mcd.mph.agecgdmed <- lmer(cds.minph ~
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.agecgdmed)
                    # no improvement

mcd.mph.agecgdmyr <- lmer(cds.minph ~
                           agem.c:chi_gender:mother_dob +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.agecgdmyr)
                    # no improvement

mcd.mph.agecgdnsb <- lmer(cds.minph ~
                           agem.c:chi_gender:number_older_sibs +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.agecgdnsb)
                    # no improvement

mcd.mph.agemednsb <- lmer(cds.minph ~
                           agem.c:mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.agemednsb)
                    # no improvement

mcd.mph.cgdmednsb <- lmer(cds.minph ~
                           chi_gender:mat_ed_num3:number_older_sibs +
                           (1|Corpus),
                           data = m.cdsrd)
anova(mcd.bas, mcd.mph.cgdmednsb)
                    # no improvement



# Best model: ####
mcd.mph.best <- lmer(cds.minph ~
                 (1|Corpus),
                 data = m.cdsrd)