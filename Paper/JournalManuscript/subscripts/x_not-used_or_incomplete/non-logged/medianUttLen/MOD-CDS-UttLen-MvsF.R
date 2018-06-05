# Model of CDS utterance length:

# Available predictors:
# agem.c
# chi_gender
# mat_ed_num3
# mother_dob
# number_older_sibs
# adu_gender_m (for f.cdspp.agd)

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
# - I will also avoid putting the known correlated effects (mother_dob--mat_ed_num3
#   and mother_dob--number_older_sibs) in the same model
# - With 1 data point per child we can't include a random effect of child

# Separate models by speaker gender ####
f.cdsln <- filter(CDSutts.agd, adu_gender_m == "FEMALE")
m.cdsln <- filter(CDSutts.agd, adu_gender_m == "MALE")

# FEMALES ####
# 0. Random effects only (base model) ####
fcd.dur.bas <-  lmer(mutt_len ~ 1 + (1|Corpus), data = f.cdsln, REML = F)


# 1. Single-predictor effects ####
# Nothing to add
# Individual models ####
fcd.dur.age <-  lmer(mutt_len ~ agem.c +
                      (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.age)
                # no improvement

fcd.dur.cgd <-  lmer(mutt_len ~ chi_gender +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.cgd)
                # no improvement

fcd.dur.med <-  lmer(mutt_len ~ mat_ed_num3 +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.med)
                # no improvement

fcd.dur.myr <-  lmer(mutt_len ~ mother_dob +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.myr)
                # no improvement

fcd.dur.nsb <-  lmer(mutt_len ~ number_older_sibs +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.nsb)
                # no improvement


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
fcd.dur.agecgd <-  lmer(mutt_len ~
                           agem.c:chi_gender +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.agecgd)
                # no improvement

fcd.dur.agemed <-  lmer(mutt_len ~
                           agem.c:mat_ed_num3 +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.agemed)
                # no improvement

fcd.dur.agemyr <-  lmer(mutt_len ~
                           agem.c:mother_dob +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.agemyr)
                # no improvement

fcd.dur.agensb <-  lmer(mutt_len ~
                           agem.c:number_older_sibs +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.agensb)
                # no improvement

fcd.dur.cgdmed <-  lmer(mutt_len ~
                           chi_gender:mat_ed_num3 +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.cgdmed)
                # no improvement

fcd.dur.cgdmyr <-  lmer(mutt_len ~
                           chi_gender:mother_dob +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.cgdmyr)
                # no improvement

fcd.dur.cgdnsb <-  lmer(mutt_len ~
                           chi_gender:number_older_sibs +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.cgdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

fcd.dur.mednsb <-  lmer(mutt_len ~
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
fcd.dur.agecgdmed <-  lmer(mutt_len ~
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.agecgdmed)
                # no improvement

fcd.dur.agecgdmyr <-  lmer(mutt_len ~
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.agecgdmyr)
                # no improvement

fcd.dur.agecgdnsb <-  lmer(mutt_len ~
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.agecgdnsb)
                # no improvement

fcd.dur.agemednsb <-  lmer(mutt_len ~
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.agemednsb)
                # no improvement

fcd.dur.cgdmednsb <-  lmer(mutt_len ~
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = f.cdsln, REML = F)
anova(fcd.dur.bas, fcd.dur.cgdmednsb)
                # no improvement
                             

# Best model: ####
fcd.dur.best <- lmer(mutt_len ~ (1|Corpus),
                 data = f.cdsln, REML = T)


# MALES ####
# 0. Random effects only (base model) ####
mcd.dur.bas <-  lmer(mutt_len ~ 1 + (1|Corpus), data = m.cdsln, REML = F)


# 1. Single-predictor effects ####
# Nothing to add
# Individual models ####
mcd.dur.age <-  lmer(mutt_len ~ agem.c +
                      (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.age)
                # no improvement

mcd.dur.cgd <-  lmer(mutt_len ~ chi_gender +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.cgd)
                # no improvement

mcd.dur.med <-  lmer(mutt_len ~ mat_ed_num3 +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.med)
                # no improvement

mcd.dur.myr <-  lmer(mutt_len ~ mother_dob +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.myr)
                # no improvement

mcd.dur.nsb <-  lmer(mutt_len ~ number_older_sibs +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.nsb)
                # only marginal improvement (p = 0.0884)


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
mcd.dur.agecgd <-  lmer(mutt_len ~
                           agem.c:chi_gender +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.agecgd)
                # no improvement

mcd.dur.agemed <-  lmer(mutt_len ~
                           agem.c:mat_ed_num3 +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.agemed)
                # no improvement

mcd.dur.agemyr <-  lmer(mutt_len ~
                           agem.c:mother_dob +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.agemyr)
                # no improvement

mcd.dur.agensb <-  lmer(mutt_len ~
                           agem.c:number_older_sibs +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.agensb)
                # no improvement

mcd.dur.cgdmed <-  lmer(mutt_len ~
                           chi_gender:mat_ed_num3 +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.cgdmed)
                # no improvement

mcd.dur.cgdmyr <-  lmer(mutt_len ~
                           chi_gender:mother_dob +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.cgdmyr)
                # no improvement

mcd.dur.cgdnsb <-  lmer(mutt_len ~
                           chi_gender:number_older_sibs +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.cgdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

mcd.dur.mednsb <-  lmer(mutt_len ~
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
mcd.dur.agecgdmed <-  lmer(mutt_len ~
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.agecgdmed)
                # no improvement

mcd.dur.agecgdmyr <-  lmer(mutt_len ~
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.agecgdmyr)
                # no improvement

mcd.dur.agecgdnsb <-  lmer(mutt_len ~
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.agecgdnsb)
                # no improvement

mcd.dur.agemednsb <-  lmer(mutt_len ~
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.agemednsb)
                # no improvement

mcd.dur.cgdmednsb <-  lmer(mutt_len ~
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = m.cdsln, REML = F)
anova(mcd.dur.bas, mcd.dur.cgdmednsb)
                # no improvement
                             

# Best model: ####
mcd.dur.best <- lmer(mutt_len ~ (1|Corpus),
                 data = m.cdsln, REML = T)
