# Model of proportion CDS:

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
f.cdspp <- filter(propCDS.agd, adu_gender_m == "FEMALE")
m.cdspp <- filter(propCDS.agd, adu_gender_m == "MALE")

# FEMALES ####
# 0. Random effects only (base model) ####
fcd.prp.bas <-  lmer(prp.cds.lg ~ (1|Corpus), data = f.cdspp)


# 1. Single-predictor effects ####
# Significant contributors: child age
# Individual models ####
fcd.prp.age <-  lmer(prp.cds.lg ~ agem.c +
                      (1|Corpus), data = f.cdspp)
                # effect of child age
anova(fcd.prp.bas, fcd.prp.age)
                # improved over previous model

fcd.prp.cgd <-  lmer(prp.cds.lg ~ chi_gender +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.bas, fcd.prp.cgd)
                # no improvement

fcd.prp.med <-  lmer(prp.cds.lg ~ mat_ed_num3 +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.bas, fcd.prp.med)
                # no improvement

fcd.prp.myr <-  lmer(prp.cds.lg ~ mother_dob +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.bas, fcd.prp.myr)
                # no improvement

fcd.prp.nsb <-  lmer(prp.cds.lg ~ number_older_sibs +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.bas, fcd.prp.nsb)
                # no improvement


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
fcd.prp.age.agecgd <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:chi_gender +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.agecgd)
                # no improvement

fcd.prp.age.agemed <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:mat_ed_num3 +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.agemed)
                # no improvement

fcd.prp.age.agemyr <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:mother_dob +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.agemyr)
                # no improvement; anova throws convergence warning

fcd.prp.age.agensb <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:number_older_sibs +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.agensb)
                # no improvement

fcd.prp.age.cgdmed <-  lmer(prp.cds.lg ~ agem.c +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.cgdmed)
                # no improvement

fcd.prp.age.cgdmyr <-  lmer(prp.cds.lg ~ agem.c +
                           chi_gender:mother_dob +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.cgdmyr)
                # no improvement

fcd.prp.age.cgdnsb <-  lmer(prp.cds.lg ~ agem.c +
                           chi_gender:number_older_sibs +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.cgdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

fcd.prp.age.mednsb <-  lmer(prp.cds.lg ~ agem.c +
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
fcd.prp.age.agecgdmed <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.agecgdmed)
                # no improvement

fcd.prp.age.agecgdmyr <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.agecgdmyr)
                # no improvement

fcd.prp.age.agecgdnsb <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.agecgdnsb)
                # no improvement

fcd.prp.age.agemednsb <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.agemednsb)
                # no improvement

fcd.prp.age.cgdmednsb <-  lmer(prp.cds.lg ~ agem.c +
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = f.cdspp)
anova(fcd.prp.age, fcd.prp.age.cgdmednsb)
                # no improvement; anova throws convergence warning
                             

# Best model: ####
fcd.prp.best <- lmer(prp.cds.lg ~ agem.c +
                 (1|Corpus),
                 data = f.cdspp)



# MALES ####
# 0. Random effects only (base model) ####
mcd.prp.bas <-  lmer(prp.cds.lg ~ (1|Corpus), data = m.cdspp)


# 1. Single-predictor effects ####
# Significant contributors: child age
# Individual models ####
mcd.prp.age <-  lmer(prp.cds.lg ~ agem.c +
                      (1|Corpus), data = m.cdspp)
                # effect of child age
anova(mcd.prp.bas, mcd.prp.age)
                # improved over previous model; anova throws convergence warning

mcd.prp.cgd <-  lmer(prp.cds.lg ~ chi_gender +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.bas, mcd.prp.cgd)
                # no improvement

mcd.prp.med <-  lmer(prp.cds.lg ~ mat_ed_num3 +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.bas, mcd.prp.med)
                # no improvement; anova throws convergence warning

mcd.prp.myr <-  lmer(prp.cds.lg ~ mother_dob +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.bas, mcd.prp.myr)
                # no improvement

mcd.prp.nsb <-  lmer(prp.cds.lg ~ number_older_sibs +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.bas, mcd.prp.nsb)
                # no improvement


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
mcd.prp.age.agecgd <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:chi_gender +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.agecgd)
                # no improvement; anova throws convergence warning

mcd.prp.age.agemed <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:mat_ed_num3 +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.agemed)
                # no improvement; anova throws convergence warning

mcd.prp.age.agemyr <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:mother_dob +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.agemyr)
                # no improvement; anova throws convergence warning

mcd.prp.age.agensb <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:number_older_sibs +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.agensb)
                # no improvement; anova throws convergence warning

mcd.prp.age.cgdmed <-  lmer(prp.cds.lg ~ agem.c +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.cgdmed)
                # no improvement; anova throws convergence warning

mcd.prp.age.cgdmyr <-  lmer(prp.cds.lg ~ agem.c +
                           chi_gender:mother_dob +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.cgdmyr)
                # no improvement; anova throws convergence warning

mcd.prp.age.cgdnsb <-  lmer(prp.cds.lg ~ agem.c +
                           chi_gender:number_older_sibs +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.cgdnsb)
                # no improvement; anova throws convergence warning

# No model with mat_ed_num3:mother_dob because they are correlated

mcd.prp.age.mednsb <-  lmer(prp.cds.lg ~ agem.c +
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.mednsb)
                # no improvement; anova throws convergence warning

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
mcd.prp.age.agecgdmed <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.agecgdmed)
                # no improvement; anova throws convergence warning

mcd.prp.age.agecgdmyr <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.agecgdmyr)
                # no improvement; anova throws convergence warning

mcd.prp.age.agecgdnsb <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.agecgdnsb)
                # no improvement; anova throws convergence warning

mcd.prp.age.agemednsb <-  lmer(prp.cds.lg ~ agem.c +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.agemednsb)
                # no improvement; anova throws convergence warning

mcd.prp.age.cgdmednsb <-  lmer(prp.cds.lg ~ agem.c +
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = m.cdspp)
anova(mcd.prp.age, mcd.prp.age.cgdmednsb)
                # no improvement; anova throws convergence warning


# Best model: ####
mcd.prp.best <- lmer(prp.cds.lg ~ agem.c +
                 (1|Corpus),
                 data = m.cdspp)
