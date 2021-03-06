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
cds.dur.bas <-  lmer(mutt_len ~ (1|Corpus), data = CDSutts)


# 1. Single-predictor effects ####
# Nothing to add
# Individual models ####
cds.dur.age <-  lmer(mutt_len ~ agem.c +
                      (1|Corpus), data = CDSutts)
anova(cds.dur.bas, cds.dur.age)
                # no improvement

cds.dur.cgd <-  lmer(mutt_len ~ chi_gender +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.bas, cds.dur.cgd)
                # no improvement

cds.dur.med <-  lmer(mutt_len ~ mat_ed_num3 +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.bas, cds.dur.med)
                # no improvement

cds.dur.myr <-  lmer(mutt_len ~ mother_dob +
                       (1|Corpus), data = CDSutts)
                # effect of mother age
anova(cds.dur.bas, cds.dur.myr)
                # improvement over previous model

cds.dur.nsb <-  lmer(mutt_len ~ number_older_sibs +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.bas, cds.dur.nsb)
                # no improvement


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.dur.myr.agecgd <-  lmer(mutt_len ~ mother_dob +
                           agem.c:chi_gender +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.agecgd)
                # no improvement

cds.dur.myr.agemed <-  lmer(mutt_len ~ mother_dob +
                           agem.c:mat_ed_num3 +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.agemed)
                # no improvement

cds.dur.myr.agemyr <-  lmer(mutt_len ~ mother_dob +
                           agem.c:mother_dob +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.agemyr)
                # no improvement

cds.dur.myr.agensb <-  lmer(mutt_len ~ mother_dob +
                           agem.c:number_older_sibs +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.agensb)
                # no improvement

cds.dur.myr.cgdmed <-  lmer(mutt_len ~ mother_dob +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.cgdmed)
                # no improvement

cds.dur.myr.cgdmyr <-  lmer(mutt_len ~ mother_dob +
                           chi_gender:mother_dob +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.cgdmyr)
                # no improvement

cds.dur.myr.cgdnsb <-  lmer(mutt_len ~ mother_dob +
                           chi_gender:number_older_sibs +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.cgdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.dur.myr.mednsb <-  lmer(mutt_len ~ mother_dob +
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.dur.myr.agecgdmed <-  lmer(mutt_len ~ mother_dob +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.agecgdmed)
                # no improvement

cds.dur.myr.agecgdmyr <-  lmer(mutt_len ~ mother_dob +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.agecgdmyr)
                # no improvement

cds.dur.myr.agecgdnsb <-  lmer(mutt_len ~ mother_dob +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.agecgdnsb)
                # no improvement

cds.dur.myr.agemednsb <-  lmer(mutt_len ~ mother_dob +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.agemednsb)
                # no improvement

cds.dur.myr.cgdmednsb <-  lmer(mutt_len ~ mother_dob +
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus), data = CDSutts)
anova(cds.dur.myr, cds.dur.myr.cgdmednsb)
                # no improvement
                             

# Best model: ####
cds.dur.best <- lmer(mutt_len ~ mother_dob +
                 (1|Corpus),
                 data = CDSutts)


#### MODEL 2: CDS utt len by spkr gender (max 2 datapoints per child) ##########
# 0. Random effects only (base model) ####
cds.dur.agd.bas <-  lmer(mutt_len ~ (1|Corpus) + (1|ID), data = CDSutts.agd)


# 1. Single-predictor effects ####
# Significant contributors: child age and mother age
# Individual models ####
cds.dur.agd.age <-  lmer(mutt_len ~ agem.c +
                      (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.bas, cds.dur.agd.age)
                # no improvement

cds.dur.agd.cgd <-  lmer(mutt_len ~ chi_gender +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
                # effect of child gender
anova(cds.dur.agd.bas, cds.dur.agd.cgd)
                # improved over previous model

cds.dur.agd.agd <-  lmer(mutt_len ~ adu_gender_m +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.bas, cds.dur.agd.agd)
                # no improvement

cds.dur.agd.med <-  lmer(mutt_len ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.bas, cds.dur.agd.med)
                # no improvement

cds.dur.agd.myr <-  lmer(mutt_len ~ mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.bas, cds.dur.agd.myr)
                # no improvement

cds.dur.agd.nsb <-  lmer(mutt_len ~ number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.bas, cds.dur.agd.nsb)
                # no improvement


# 2. Try out 2-way interactions ####
# Nothing to add
# Individual models ####
cds.dur.agd.cgd.agecgd <-  lmer(mutt_len ~ chi_gender +
                           agem.c:chi_gender +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agecgd)
                # no improvement

cds.dur.agd.cgd.ageagd <-  lmer(mutt_len ~ chi_gender +
                           agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.ageagd)
                # no improvement

cds.dur.agd.cgd.agemed <-  lmer(mutt_len ~ chi_gender +
                           agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agemed)
                # no improvement

cds.dur.agd.cgd.agemyr <-  lmer(mutt_len ~ chi_gender +
                           agem.c:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agemyr)
                # no improvement

cds.dur.agd.cgd.agensb <-  lmer(mutt_len ~ chi_gender +
                           agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agensb)
                # no improvement

cds.dur.agd.cgd.cgdagd <-  lmer(mutt_len ~ chi_gender +
                           chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.cgdagd)
                # no improvement

cds.dur.agd.cgd.cgdmed <-  lmer(mutt_len ~ chi_gender +
                           chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.cgdmed)
                # no improvement

cds.dur.agd.cgd.cgdmyr <-  lmer(mutt_len ~ chi_gender +
                           chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.cgdmyr)
                # no improvement

cds.dur.agd.cgd.cgdnsb <-  lmer(mutt_len ~ chi_gender +
                           chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.cgdnsb)
                # no improvement

cds.dur.agd.cgd.agdmed <-  lmer(mutt_len ~ chi_gender +
                           adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agdmed)
                # no improvement

cds.dur.agd.cgd.agdmyr <-  lmer(mutt_len ~ chi_gender +
                           adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agdmyr)
                # no improvement

cds.dur.agd.cgd.agdnsb <-  lmer(mutt_len ~ chi_gender +
                           adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agdnsb)
                # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.dur.agd.cgd.mednsb <-  lmer(mutt_len ~ chi_gender +
                           mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.mednsb)
                # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Nothing to add
# Individual models ####
cds.dur.agd.cgd.agecgdagd <-  lmer(mutt_len ~ chi_gender +
                           agem.c:chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agecgdagd)
                # no improvement

cds.dur.agd.cgd.agecgdmed <-  lmer(mutt_len ~ chi_gender +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agecgdmed)
                # no improvement

cds.dur.agd.cgd.agecgdmyr <-  lmer(mutt_len ~ chi_gender +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agecgdmyr)
                # no improvement

cds.dur.agd.cgd.agecgdnsb <-  lmer(mutt_len ~ chi_gender +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agecgdnsb)
                # no improvement

cds.dur.agd.cgd.ageagdmed <-  lmer(mutt_len ~ chi_gender +
                           agem.c:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.ageagdmed)
                # no improvement

cds.dur.agd.cgd.ageagdmyr <-  lmer(mutt_len ~ chi_gender +
                           agem.c:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.ageagdmyr)
                # no improvement

cds.dur.agd.cgd.ageagdnsb <-  lmer(mutt_len ~ chi_gender +
                           agem.c:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.ageagdnsb)
                # no improvement

cds.dur.agd.cgd.agemednsb <-  lmer(mutt_len ~ chi_gender +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agemednsb)
                # no improvement; anova throws convergence warning

cds.dur.agd.cgd.cgdagdmed <-  lmer(mutt_len ~ chi_gender +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.cgdagdmed)
                # no improvement
                             
cds.dur.agd.cgd.cgdagdmyr <-  lmer(mutt_len ~ chi_gender +
                           chi_gender:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.cgdagdmyr)
                # no improvement
                             
cds.dur.agd.cgd.cgdagdnsb <-  lmer(mutt_len ~ chi_gender +
                           chi_gender:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.cgdagdnsb)
                # no improvement
                             
cds.dur.agd.cgd.cgdmednsb <-  lmer(mutt_len ~ chi_gender +
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.cgdmednsb)
                # no improvement
                             
cds.dur.agd.cgd.agdmednsb <-  lmer(mutt_len ~ chi_gender +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID), data = CDSutts.agd)
anova(cds.dur.agd.cgd, cds.dur.agd.cgd.agdmednsb)
                # no improvement


# Best model: ####
cds.dur.agd.best <- lmer(mutt_len ~ chi_gender +
                 (1|Corpus) + (1|ID),
                 data = CDSutts.agd)
