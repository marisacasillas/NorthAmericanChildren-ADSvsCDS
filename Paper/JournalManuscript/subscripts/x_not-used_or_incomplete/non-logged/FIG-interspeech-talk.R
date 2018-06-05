# figures elika used for interspeech 2017
# rely on running all other scripts first; sorry i'm so sloppy
# i made the lena gender plot properly and then was too pressed for time


ggplot(xdsall %>% filter(!label_m%in% c("NMJ","JUNK")), aes(label_m, fill = lena_id))+geom_bar(position = "fill")+theme_bw(base_size=18)+facet_wrap(~adu_gender_m)+scale_fill_manual(values = c("pink","blue"))

ggplot(propCDS,
       aes(x=AgeMonths, y=prp.cds, fill=factor(mat_ed_num3))) +
  geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("CDS/XDS (sec)") + xlab("Age (months)") +
  scale_y_continuous(expand = c(0.1, 0.1), limits = c(-0.001,1.20)) + #eb edit
  misc.prefs + col.medu +
  guides(color = guide_legend("Maternal education"),
         fill = guide_legend("Maternal education"))+facet_wrap(~adu_gender_m)


ggplot(propCDS,
       aes(x=AgeMonths, y=prp.cds, fill=chi_gender)) +
  geom_smooth(aes(color=chi_gender), method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("CDS/XDS (sec)") + xlab("Age (months)") +
  scale_y_continuous(expand = c(0.1, 0.1), limits = c(-0.001,1.100)) + #eb edit
  misc.prefs + 
  col.cgen +
  guides(color = guide_legend("child gender"),
         fill = guide_legend("child gender"))+
  facet_wrap(~adu_gender_m)

#this version gives equal 'weight' to male and female speakers
ggplot(propCDS,
       aes(x=AgeMonths, y=prp.cds)) +
  geom_smooth(method="lm") +
  stat_summary(aes(group=ChiID), fun.y=mean, geom = "point", position = "jitter", shape = 21, size = 6)+
  #geom_jitter(size=6, shape=21) +
  ylab("CDS/XDS (sec)") + xlab("Age (months)") +
  scale_y_continuous(expand = c(0.1, 0.1), limits = c(-0.001,1.100)) + #eb edit
  misc.prefs

#this version gives equal weight to each **utterance**; used this one in talk
ggplot(propCDS_collag,
       aes(x=AgeMonths, y=prp.cds)) +
  geom_smooth(method="lm") +
  geom_point(size=6, shape=21) +
  ylab("CDS/XDS (sec)") + xlab("Age (months)") +
  scale_y_continuous(expand = c(0.1, 0.1), limits = c(-0.001,1.000)) + #eb edit
  misc.prefs

#ads minph by adult gender
ggplot(adsratedata,
       aes(x=AgeMonths, y=ads.minph, fill=adu_gender_m)) +
  geom_smooth(aes(color=adu_gender_m), method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("ADS min/hr") + xlab("Age (months)") +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
  misc.prefs + col.agen +
  guides(color = guide_legend("Speaker gender"),
         fill = guide_legend("Speaker gender"))
#cds minph by adult gender
ggplot(cdsratedata,
       aes(x=AgeMonths, y=cds.minph, fill=adu_gender_m)) +
  geom_smooth(aes(color=adu_gender_m), method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("CDS min/hr") + xlab("Age (months)") +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
  misc.prefs + col.agen +
  guides(color = guide_legend("Speaker gender"),
         fill = guide_legend("Speaker gender"))
#ads minph collapsed across speakers
ggplot(adsratedata_collag,
       aes(x=AgeMonths, y=ads.minph)) +
  geom_smooth(method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("ADS min/hr") + xlab("Age (months)") +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
  misc.prefs

#cds minph collapsed across speakers
ggplot(cdsratedata_collag,
       aes(x=AgeMonths, y=cds.minph)) +
  geom_smooth(method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("CDS min/hr") + xlab("Age (months)") +
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
  misc.prefs

ggplot(cdsratedata,
       aes(x=AgeMonths, y=cds.minph, fill=factor(mat_ed_num3))) +
  geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("CDS min/hr") + xlab("Age (months)") +
  #scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
  misc.prefs + col.medu +
  guides(color = guide_legend("Maternal education"),
         fill = guide_legend("Maternal education"))+facet_wrap(~adu_gender_m)

ggplot(adsratedata,
       aes(x=AgeMonths, y=ads.minph, fill=factor(mat_ed_num3))) +
  geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("ADS min/hr") + xlab("Age (months)") +
  #scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
  misc.prefs + col.medu +
  guides(color = guide_legend("Maternal education"),
         fill = guide_legend("Maternal education"))+facet_wrap(~adu_gender_m)

ggplot(cdsratedata_collag,
       aes(x=AgeMonths, y=cds.minph, fill=factor(mat_ed_num3))) +
  geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("CDS min/hr") + xlab("Age (months)") +
  #scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
  misc.prefs + col.medu +
  guides(color = guide_legend("Maternal education"),
         fill = guide_legend("Maternal education"))+facet_wrap(~adu_gender_m)

ggplot(adsratedata_collag,
       aes(x=AgeMonths, y=ads.minph, fill=factor(mat_ed_num3))) +
  geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
  geom_jitter(size=6, shape=21) +
  ylab("ADS min/hr") + xlab("Age (months)") +
  #scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
  misc.prefs + col.medu +
  guides(color = guide_legend("Maternal education"),
         fill = guide_legend("Maternal education"))+facet_wrap(~adu_gender_m)

ggplot(propCDS, aes(factor(mat_ed_num3), cds.sec, color = ChiID, shape=cut(AgeMonths,2)))+
  facet_wrap(~adu_gender_m)+
  stat_summary(fun.data=mean_cl_boot, geom = "pointrange", color = "red", position = position_dodge(width = .3), size = 2)+
  scale_shape_discrete(solid=F)+
  misc.prefs + 
  #col.medu +
  geom_point(position = position_dodge(width=.3), aes(group = cut(AgeMonths,2)), size = 6)+
  guides(color = "none")

ggplot(propCDS, aes(factor(mat_ed_num3), ads.sec+cds.sec, color = ChiID, shape=cut(AgeMonths,2)))+
  facet_wrap(~adu_gender_m)+
  stat_summary(fun.data=mean_cl_boot, geom = "pointrange", color = "black", position = position_dodge(width = .3), size = 2)+
  scale_shape_discrete(solid=F)+
  misc.prefs + 
  #col.medu +
  geom_point(position = position_dodge(width=.3), aes(group = cut(AgeMonths,2)), size = 6)+
  guides(color = "none")

#used in talk for age x SES
ggplot(propCDS_collag, aes(factor(mat_ed_num3), (ads.sec+cds.sec)/60, color = ChiID, 
                           shape=cut(AgeMonths,2)))+
  #facet_wrap(~adu_gender_m)+
  stat_summary(fun.data=mean_cl_boot, 
               geom = "pointrange", 
               color = "black", 
               position = position_dodge(width = .3), 
               size = 2,
               aes(group=cut(AgeMonths,2)))+
  scale_shape_discrete(solid=F)+
  misc.prefs + 
  geom_point(position = position_dodge(width=.3), aes(group = cut(AgeMonths,2)), size = 6)+
  guides(color = "none",
         shape = guide_legend(title = "Age Group"))+
  ylab("Total ADS+CDS Input (min.)")+
  xlab("Maternal Education")

wilcox.test(subset(propCDS_collag, AgeGroup=="(2.98,11.5]" & mat_ed_num3==1)$totallang,
            subset(propCDS_collag, AgeGroup=="(2.98,11.5]" & mat_ed_num3!=1)$totallang,
            conf.int=T)
