# Versions of reported models with maximal random effects structure:

# ADS minPH ####
ads.mph.best.max <- lmer(ads.minph ~ agem.c +
                     (1 + chi_gender * agem.c * n_sibs|Corpus),
                     data = adsratedata, REML = T,
                     control = lmerControl(optCtrl = list(maxfun = 500000)))

ads.agd.mph.best.max <- lmer(ads.minph ~ agem.c * adu_gender_m +
                         (1 + agem.c * n_sibs + chi_gender * adu_gender_m +
                            adu_gender_m:agem.c + adu_gender_m:n_sibs +
                            agem.c:chi_gender|Corpus) +
                         (1|ID),
                         data = adsratedata.agd,
                         REML = T,
                         control = lmerControl(optCtrl = list(maxfun = 500000)))

ads.agd.s.mph.best.max <- lmer(ads.minph ~ agem.c * adu_gender_m +
                           (1 + chi_gender * agem.c + adu_gender_m|Corpus) +
                           (1|ID),
                           data = adsratedata.agd.sub,
                           REML = T,
                           control = lmerControl(optCtrl =
                                                   list(maxfun = 500000),
                                                 optimizer = "bobyqa"))

# CDS minPH ####
cds.mph.best.max <- lmer(cds.minph ~ mat_ed_num3 +
                     (1 + chi_gender * agem.c * n_sibs +
                        mat_ed_num3 * agem.c + mat_ed_num3:n_sibs|Corpus),
                     data = cdsratedata, REML = T,
                     control = lmerControl(optCtrl = list(maxfun = 500000),
                                                 optimizer = "bobyqa"))

cds.agd.mph.best.max <- lmer(cds.minph ~ adu_gender_m +
                       (1 + chi_gender * agem.c + chi_gender * adu_gender_m +
                        chi_gender * n_sibs + chi_gender * mat_ed_num3 +
                        adu_gender_m:n_sibs + adu_gender_m:agem.c +
                        mat_ed_num3:n_sibs|Corpus) +
                       (1|ID),
                       data = cdsratedata.agd, REML = T,
                       control = lmerControl(optCtrl = list(maxfun = 500000),
                                                 optimizer = "bobyqa"))

cds.agd.s.mph.best.max <- lmer(cds.minph ~ adu_gender_m +
                       (1 + chi_gender * agem.c * adu_gender_m +
                          chi_gender * n_sibs * mat_ed_num3 +
                          agem.c:n_sibs + adu_gender_m:mat_ed_num3|Corpus) +
                       (1|ID),
                       data = cdsratedata.agd.sub, REML = T,
                       control = lmerControl(optCtrl = list(maxfun = 500000),
                                                 optimizer = "bobyqa"))


# Prop CDS ####
cds.prp.best.max <- lmer(prp.cds ~ agem.c +
                      (1|Corpus), data = propCDS, REML = T)

cds.prp.agd.best.max <- lmer(prp.cds ~ agem.c * adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd, REML = T)

cds.prp.agd.s.best.max <- lmer(prp.cds ~ agem.c + adu_gender_m +
                       (1|Corpus) + (1|ID), data = propCDS.agd.sub, REML = T)
