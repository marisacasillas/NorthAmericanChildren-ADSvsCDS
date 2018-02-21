# Model of ADS minutes per hour:

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
f.adsrd <- filter(adsratedata.agd.sub, adu_gender_m == "FEMALE")
m.adsrd <- filter(adsratedata.agd.sub, adu_gender_m == "MALE")

# FEMALES ####
# 0. Random effects only (base model) ####
fad.bas <-  lmer(ads.minph.lg ~ (1|Corpus), data = f.adsrd)

# 1. Single-predictor effects ####
# Significant contributors: child age and maternal age
# Individual models ####
fad.mph.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus), data = f.adsrd)
                # effect of child age
anova(fad.bas, fad.mph.age)
                # improved over previous model

fad.mph.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus), data = f.adsrd)
anova(fad.bas, fad.mph.cgd)
                # no improvement

fad.mph.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus), data = f.adsrd)
anova(fad.bas, fad.mph.med)
                # no improvement

fad.mph.nsb <-  lmer(ads.minph.lg ~ n_sibs +
                       (1|Corpus), data = f.adsrd)
anova(fad.bas, fad.mph.nsb)
                # no improvement

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
fad.mph.age.agecgd <- lmer(ads.minph.lg ~ agem.c +
                             agem.c:chi_gender +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.agecgd)
                    # only marginal improvement (p = 0.06463)

fad.mph.age.agemed <- lmer(ads.minph.lg ~ agem.c +
                             agem.c:mat_ed_num3 +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.agemed)
                    # no improvement

fad.mph.age.agensb <- lmer(ads.minph.lg ~ agem.c +
                             agem.c:n_sibs +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.agensb)
                    # no improvement

fad.mph.age.cgdmed <- lmer(ads.minph.lg ~ agem.c +
                             chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.cgdmed)
                    # no improvement

fad.mph.age.cgdnsb <- lmer(ads.minph.lg ~ agem.c +
                             chi_gender:n_sibs +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.cgdnsb)
                    # no improvement

fad.mph.age.mednsb <- lmer(ads.minph.lg ~ agem.c +
                             mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.mednsb)
                    # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
fad.mph.age.agecgdmed <- lmer(ads.minph.lg ~ agem.c +
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.agecgdmed)
                    # no improvement

fad.mph.age.agecgdnsb <- lmer(ads.minph.lg ~ agem.c +
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.agecgdnsb)
                    # no improvement

fad.mph.age.agemednsb <- lmer(ads.minph.lg ~ agem.c +
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.agemednsb)
                    # no improvement

fad.mph.age.cgdmednsb <- lmer(ads.minph.lg ~ agem.c +
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = f.adsrd)
anova(fad.mph.age, fad.mph.age.cgdmednsb)
                    # no improvement

# Best model: ####
fad.mph.best <- fad.mph.age


# MALES ####
# 0. Random effects only (base model) ####
mad.bas <-  lmer(ads.minph.lg ~ (1|Corpus), data = m.adsrd)

# 1. Single-predictor effects ####
# Significant contributors: child age and maternal age
# Individual models ####
mad.mph.age <-  lmer(ads.minph.lg ~ agem.c +
                      (1|Corpus), data = m.adsrd)
anova(mad.bas, mad.mph.age)
                # only marginal improvement (p = 0.07073)

mad.mph.cgd <-  lmer(ads.minph.lg ~ chi_gender +
                       (1|Corpus), data = m.adsrd)
anova(mad.bas, mad.mph.cgd)
                # no improvement

mad.mph.med <-  lmer(ads.minph.lg ~ mat_ed_num3 +
                       (1|Corpus), data = m.adsrd)
anova(mad.bas, mad.mph.med)
                # no improvement

mad.mph.nsb <-  lmer(ads.minph.lg ~ n_sibs +
                       (1|Corpus), data = m.adsrd)
anova(mad.bas, mad.mph.nsb)
                # no improvement

# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
mad.mph.agecgd <- lmer(ads.minph.lg ~
                             agem.c:chi_gender +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.agecgd)
                    # no improvement

mad.mph.agemed <- lmer(ads.minph.lg ~ 
                             agem.c:mat_ed_num3 +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.agemed)
                    # no improvement

mad.mph.agensb <- lmer(ads.minph.lg ~ 
                             agem.c:n_sibs +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.agensb)
                # no improvement

mad.mph.cgdmed <- lmer(ads.minph.lg ~ 
                             chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.cgdmed)
                    # no improvement

mad.mph.cgdnsb <- lmer(ads.minph.lg ~ 
                             chi_gender:n_sibs +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.cgdnsb)
                    # no improvement

mad.mph.mednsb <- lmer(ads.minph.lg ~ 
                             mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.mednsb)
                    # no improvement

# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
mad.mph.agecgdmed <- lmer(ads.minph.lg ~
                           agem.c:chi_gender:mat_ed_num3 +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.agecgdmed)
                    # no improvement

mad.mph.agecgdnsb <- lmer(ads.minph.lg ~
                           agem.c:chi_gender:n_sibs +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.agecgdnsb)
                    # no improvement

mad.mph.agemednsb <- lmer(ads.minph.lg ~
                           agem.c:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.agemednsb)
                    # no improvement

mad.mph.cgdmednsb <- lmer(ads.minph.lg ~
                           chi_gender:mat_ed_num3:n_sibs +
                           (1|Corpus),
                           data = m.adsrd)
anova(mad.bas, mad.mph.cgdmednsb)
                    # no improvement

# Best model: ####
mad.mph.best <- mad.bas