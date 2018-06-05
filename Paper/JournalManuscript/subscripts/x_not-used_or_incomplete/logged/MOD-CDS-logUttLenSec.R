# Model of CDS utterance length in seconds:

# Available predictors:
# agem.c
# chi_gender
# adu_gender_m
# mat_ed_num3
# mother_dob
# number_older_sibs

# Notes:
# In what follows I'll only report model outcomes for models that
# significatly improve upon base/previous models
# I will also avoid putting the known correlated effects (mother_dob--mat_ed_num3
# and mother_dob--number_older_sibs) in the same model

# 1. Single-predictor effects ####
# Significant contributors: child age (though shaky) and adult gender
# Individual models ####
cds.len.lg.age <-  lmer(utt_dur.sec.lg ~ agem.c +
                      (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                # no effect of child age

cds.len.lg.cgd <-  lmer(utt_dur.sec.lg ~ chi_gender +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                # no effect of child gender

cds.len.lg.agd <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                # effect of adult gender

cds.len.lg.med <-  lmer(utt_dur.sec.lg ~ mat_ed_num3 +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                # no effect of maternal education

cds.len.lg.myr <-  lmer(utt_dur.sec.lg ~ mother_dob +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                # no effect of maternal age

cds.len.lg.nsb <-  lmer(utt_dur.sec.lg ~ number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                # no effect of num siblings


# 2. Try out two-way interactions ####
# Nothing to add
# Individual models ####
cds.len.lg.agd.ageagd <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             agem.c:adu_gender_m +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.ageagd)
                    # no improvement

cds.len.lg.agd.agemed <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             agem.c:mat_ed_num3 +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agemed)
                    # no improvement

cds.len.lg.agd.agemyr <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             agem.c:mother_dob +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agemyr)
                    # no improvement

cds.len.lg.agd.agensb <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             agem.c:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agensb)
                    # no improvement

cds.len.lg.agd.cgdagd <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.cgdagd)
                    # no improvement

cds.len.lg.agd.cgdmed <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.cgdmed)
                    # no improvement

cds.len.lg.agd.cgdmyr <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             chi_gender:mother_dob +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.cgdmyr)
                    # no improvement

cds.len.lg.agd.cgdnsb <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.cgdnsb)
                    # no improvement

cds.len.lg.agd.agdmed <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agdmed)
                    # no improvement

cds.len.lg.agd.agdmyr <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agdmyr)
                    # no improvement

cds.len.lg.agd.agdnsb <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agdnsb)
                    # no improvement

# No model with mat_ed_num3:mother_dob because they are correlated

cds.len.lg.agd.mednsb <-  lmer(utt_dur.sec.lg ~ adu_gender_m +
                             mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.mednsb)
                    # no improvement

# No model with mother_dob:number_older_sibs because they are correlated


# 3. Try out three-way interactions ####
# Three 3-wy interactions contribute to model fit, chiage-chigen-adugen and
# chiage-chigen-matedu
# Individual models ####
cds.len.lg.agd.agecgdagd <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                    # effects of adult gender and a 3wy effect of
                    # age for adult female speech to male infants
anova(cds.len.lg.agd, cds.len.lg.agd.agecgdagd)
                    # improved over previous model

cds.len.lg.agd.agecgdmed <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                    # effects of adult gender and a 3wy effect of
                    # age for speech to male infants with maternal education
anova(cds.len.lg.agd, cds.len.lg.agd.agecgdmed)
                    # improved over previous model

cds.len.lg.agd.agecgdmyr <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:chi_gender:mother_dob +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agecgdmyr)
                    # marginally improved (p=0.0508)

cds.len.lg.agd.agecgdnsb <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:chi_gender:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agecgdnsb)
                    # no improvement

cds.len.lg.agd.ageagdmed <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.ageagdmed)
                    # no improvement

cds.len.lg.agd.ageagdmyr <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.ageagdmyr)
                    # no improvement

cds.len.lg.agd.ageagdnsb <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.ageagdnsb)
                    # no improvement

cds.len.lg.agd.agemednsb <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agemednsb)
                    # no improvement

cds.len.lg.agd.cgdagdmed <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           chi_gender:adu_gender_m:mat_ed_num3 +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.cgdagdmed)
                    # no improvement

cds.len.lg.agd.cgdagdmyr <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           chi_gender:adu_gender_m:mother_dob +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.cgdagdmyr)
                    # no improvement

cds.len.lg.agd.cgdagdnsb <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           chi_gender:adu_gender_m:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.cgdagdnsb)
                    # no improvement

cds.len.lg.agd.cgdmednsb <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           chi_gender:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.cgdmednsb)
                    # no improvement

cds.len.lg.agd.agdmednsb <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                           adu_gender_m:mat_ed_num3:number_older_sibs +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
anova(cds.len.lg.agd, cds.len.lg.agd.agdmednsb)
                    # no improvement


# 4. 3-way interaction combinations (w/ maternal effects as base models) ####
# Nothing to add
# Individual models ####
cds.len.lg.agd.agecgdmed.agecgdagd  <- lmer(
                           utt_dur.sec.lg ~ adu_gender_m +
                           agem.c:chi_gender:mat_ed_num3 +
                           agem.c:chi_gender:adu_gender_m +
                       (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)
                    # effects of adult gender and a 3wy effect of
                    # age for speech to male infants with maternal education
anova(cds.len.lg.agd.agecgdmed, cds.len.lg.agd.agecgdmed.agecgdagd)
                    # no improvement over model with just the
                    # chiage-chigen-matedu interaction
anova(cds.len.lg.agd.agecgdagd, cds.len.lg.agd.agecgdmed.agecgdagd)
                    # no improvement over model with just the
                    # chiage-chigen-adugen interaction

# Direct model comparison of the single 3-wy effect models:
anova(cds.len.lg.agd.agecgdmed, cds.len.lg.agd.agecgdagd)
      # no difference


########## A slighty tricky situation: ########################################
# SIMPLE SOLUTIONS:
# - AIC and BIC are smallest for the single 3-wy interaction of
#   chiage-chigen-matedy but this ignores that:
#       + there is another 3-wy effects that significantly improve model fit
#       + the inclusion of both 3-wy effects renders the interaction of
#         chiage-chigen-adugen insignificant
#
# COMPROMISE
# - As with the non-logged model, we can base our decision on the principle
#   that each 3-wy interaction contributes significantly to model fit and
#   therefore use the model with the combination of chiage-chigen-matedu and
#   chiage-chigen-adugen, which (sensibly) has a larger AIC and BIC than the
#   other two models with only one 3-wy interaction.

# Best model: ####
cds.len.lg.best <- lmer(utt_dur.sec.lg ~ adu_gender_m +
                     agem.c:chi_gender:mat_ed_num3 +
                     agem.c:chi_gender:adu_gender_m +
                     (1|Corpus) + (1|ID) + (1|uniq_block), data = CDSutts)

