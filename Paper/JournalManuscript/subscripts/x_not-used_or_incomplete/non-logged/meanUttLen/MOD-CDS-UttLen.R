# Model of CDS utterance length:

# Available predictors:
# agem.c
# chi_gender
# mat_ed_num3
# mother_dob
# number_older_sibs
# adu_gender_m (for CDSutts.agd)

# Notes:
# - In what follows I'll only report model outcomes for models that
#   significantly improve upon base/previous models
# - I will also avoid putting the known correlated effects (mother_dob--mat_ed_num3
#   and mother_dob--number_older_sibs) in the same model
# - With 1 data point per child we can't include a random effect of child


#### MODEL 1: CDS utt length overall (1 datapoint per child) ##########
# 0. Random effects only (base model) ####
cds.dur.bas <-  lmer(mutt_len ~ 1 + (1|Corpus), data = CDSutts, REML = F)


# 1. Single-predictor effects ####
# Nothing to add
# Individual models ####
cds.dur.age <-  lmer(mutt_len ~ agem.c +
                      (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.age)
                # no improvement

cds.dur.cgd <-  lmer(mutt_len ~ chi_gender +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.cgd)
                # no improvement

cds.dur.med <-  lmer(mutt_len ~ mat_ed_num3 +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.med)
                # no improvement

cds.dur.myr <-  lmer(mutt_len ~ mother_dob +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.myr)
                # no improvement

cds.dur.nsb <-  lmer(mutt_len ~ number_older_sibs +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.nsb)
                # no improvement


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.dur.agecgd <-  lmer(mutt_len ~
                           agem.c:chi_gender +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.agecgd)
                # no improvement

cds.dur.agemed <-  lmer(mutt_len ~
                           agem.c:mat_ed_num3 +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.agemed)
                # no improvement

cds.dur.agemyr <-  lmer(mutt_len ~
                           agem.c:mother_dob +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.agemyr)
                # no improvement

cds.dur.agensb <-  lmer(mutt_len ~
                           agem.c:number_older_sibs +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.agensb)
                # no improvement

cds.dur.cgdmed <-  lmer(mutt_len ~
                           chi_gender:mat_ed_num3 +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.cgdmed)
                # no improvement

cds.dur.cgdmyr <-  lmer(mutt_len ~
                           chi_gender:mother_dob +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.cgdmyr)
                # no improvement

cds.dur.cgdnsb <-  lmer(mutt_len ~
                           chi_gender:number_older_sibs +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.cgdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.dur.mednsb <-  lmer(mutt_len ~
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.dur.agecgdmed <-  lmer(mutt_len ~
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.agecgdmed)
                # no improvement

cds.dur.agecgdmyr <-  lmer(mutt_len ~
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.agecgdmyr)
                # no improvement

cds.dur.agecgdnsb <-  lmer(mutt_len ~
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.agecgdnsb)
                # no improvement

cds.dur.agemednsb <-  lmer(mutt_len ~
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.agemednsb)
                # no improvement

cds.dur.cgdmednsb <-  lmer(mutt_len ~
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = CDSutts, REML = F)
anova(cds.dur.bas, cds.dur.cgdmednsb)
                # no improvement
                             

# Best model: ####
cds.dur.best <- lmer(mutt_len ~ (1|Corpus),
                 data = CDSutts, REML = T)


#### MODEL 2: CDS utt len by spkr gender (max 2 datapoints per child) ##########
# 0. Random effects only (base model) ####
cds.dur.agd.bas <-  lmer(mutt_len ~ 1 + (1|Corpus) + (1|ID),
                         data = CDSutts.agd, REML = F)

# 1. Single-predictor effects ####
# Significant contributors: child age and mother age
# Individual models ####
cds.dur.agd.age <-  lmer(mutt_len ~ agem.c +
                      (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.bas, cds.dur.agd.age)
                # no improvement

cds.dur.agd.cgd <-  lmer(mutt_len ~ chi_gender +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.bas, cds.dur.agd.cgd)
                # no improvement

cds.dur.agd.agd <-  lmer(mutt_len ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
                # effect of speaker gender
anova(cds.dur.agd.bas, cds.dur.agd.agd)
                # improved over previous model

cds.dur.agd.med <-  lmer(mutt_len ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.bas, cds.dur.agd.med)
                # no improvement

cds.dur.agd.myr <-  lmer(mutt_len ~ mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.bas, cds.dur.agd.myr)
                # no improvement

cds.dur.agd.nsb <-  lmer(mutt_len ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.bas, cds.dur.agd.nsb)
                # no improvement


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.dur.agd.agd.agecgd <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agecgd)
                # no improvement

cds.dur.agd.agd.ageagd <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.ageagd)
                # no improvement

cds.dur.agd.agd.agemed <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agemed)
                # no improvement

cds.dur.agd.agd.agemyr <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agemyr)
                # no improvement

cds.dur.agd.agd.agensb <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agensb)
                # no improvement

cds.dur.agd.agd.cgdagd <-  lmer(mutt_len ~ adu_gender_m +
                           chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.cgdagd)
                # no improvement

cds.dur.agd.agd.cgdmed <-  lmer(mutt_len ~ adu_gender_m +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.cgdmed)
                # no improvement

cds.dur.agd.agd.cgdmyr <-  lmer(mutt_len ~ adu_gender_m +
                           chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.cgdmyr)
                # no improvement

cds.dur.agd.agd.cgdnsb <-  lmer(mutt_len ~ adu_gender_m +
                           chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.cgdnsb)
                # no improvement

cds.dur.agd.agd.agdmed <-  lmer(mutt_len ~ adu_gender_m +
                           adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agdmed)
                # no improvement

cds.dur.agd.agd.agdmyr <-  lmer(mutt_len ~ adu_gender_m +
                           adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agdmyr)
                # no improvement

cds.dur.agd.agd.agdnsb <-  lmer(mutt_len ~ adu_gender_m +
                           adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.dur.agd.agd.mednsb <-  lmer(mutt_len ~ adu_gender_m +
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.dur.agd.agd.agecgdagd <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agecgdagd)
                # no improvement

cds.dur.agd.agd.agecgdmed <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agecgdmed)
                # no improvement

cds.dur.agd.agd.agecgdmyr <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agecgdmyr)
                # no improvement

cds.dur.agd.agd.agecgdnsb <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agecgdnsb)
                # no improvement

cds.dur.agd.agd.ageagdmed <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.ageagdmed)
                # no improvement

cds.dur.agd.agd.ageagdmyr <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.ageagdmyr)
                # no improvement

cds.dur.agd.agd.ageagdnsb <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.ageagdnsb)
                # no improvement

cds.dur.agd.agd.agemednsb <-  lmer(mutt_len ~ adu_gender_m +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agemednsb)
                # no improvement

cds.dur.agd.agd.cgdagdmed <-  lmer(mutt_len ~ adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.cgdagdmed)
                # no improvement
                             
cds.dur.agd.agd.cgdagdmyr <-  lmer(mutt_len ~ adu_gender_m +
                           chi_gender:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.cgdagdmyr)
                # no improvement
                             
cds.dur.agd.agd.cgdagdnsb <-  lmer(mutt_len ~ adu_gender_m +
                           chi_gender:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.cgdagdnsb)
                # no improvement
                             
cds.dur.agd.agd.cgdmednsb <-  lmer(mutt_len ~ adu_gender_m +
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.cgdmednsb)
                # no improvement
                             
cds.dur.agd.agd.agdmednsb <-  lmer(mutt_len ~ adu_gender_m +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd, REML = F)
anova(cds.dur.agd.agd, cds.dur.agd.agd.agdmednsb)
                # no improvement


# Best model: ####
cds.dur.agd.best <- lmer(mutt_len ~ adu_gender_m +
                 (1|Corpus) + (1|ID),
                 data = CDSutts.agd, REML = T)
