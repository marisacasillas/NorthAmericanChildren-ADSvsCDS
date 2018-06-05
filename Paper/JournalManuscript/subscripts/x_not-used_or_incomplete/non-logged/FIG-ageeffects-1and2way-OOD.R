figname1 <- "simpleeffects-chiage_corpus.png"
figname2 <- "simpleeffects-chiage_chigen.png"
figname3 <- "simpleeffects-chiage_matedu.png"
figname4 <- "simpleeffects-chiage_adugen.png"

misc.prefs <- list(
  xlim(0,24),
  basic.theme + theme(legend.position = "none"))                 

col.corp <- list(
  scale_fill_manual(labels=corpuslabels, values=corpuscolors),
  scale_color_manual(labels=corpuslabels, values=corpuscolors))                 
col.cgen <- list(
  scale_fill_manual(labels=c("Girl", "Boy"), values=c("pink", "lightblue")),
  scale_color_manual(labels=c("Girl", "Boy"), values=c("pink", "lightblue")))                 
#col.medu <- list(
#  scale_fill_manual(labels=c("HS-", "BA+"), values=c("gray80", "gray20")),
#  scale_color_manual(labels=c("HS-", "BA+"), values=c("gray80", "gray20")))
col.medu <- list(
  scale_fill_manual(labels=c("SHS/SC", "BA", "AD"),
                    values=c("gray90", "gray50", "gray20")),
  scale_color_manual(labels=c("SHS/SC", "BA", "AD"),
                    values=c("gray90", "gray50", "gray20")))
col.agen <- list(
  scale_fill_manual(labels=c("Woman", "Man"), values=c("maroon", "mediumblue")),
  scale_color_manual(labels=c("Woman", "Nab"), values=c("maroon", "mediumblue")))                 

### child age by corpus
c.age.corp.1 <-  ggplot(c.age.vol.all,
                       aes(x=AgeMonths, y=n, fill=Corpus)) +
                 geom_smooth(aes(color=Corpus), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (CDS+ADS)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.corp
c.age.corp.2 <-  ggplot(c.age.vol.ads,
                       aes(x=AgeMonths, y=n, fill=Corpus)) +
                 geom_smooth(aes(color=Corpus), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (ADS only)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.corp
c.age.corp.3 <-  ggplot(c.age.vol.cds,
                       aes(x=AgeMonths, y=n, fill=Corpus)) +
                 geom_smooth(aes(color=Corpus), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (CDS only)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.corp
c.age.corp.4 <-  ggplot(c.age.pro.cds,
                       aes(x=AgeMonths, y=n, fill=Corpus)) +
                 geom_smooth(aes(color=Corpus), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("% Utts (CDS)") + xlab("Age (months)") +
                 ylim(0,100) + misc.prefs + col.corp


### child age by child gender
c.age.c.gen.1 <-  ggplot(c.age.vol.all,
                       aes(x=AgeMonths, y=n, fill=chi_gender)) +
                 geom_smooth(aes(color=chi_gender), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (CDS+ADS)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.cgen
c.age.c.gen.2 <-  ggplot(c.age.vol.ads,
                       aes(x=AgeMonths, y=n, fill=chi_gender)) +
                 geom_smooth(aes(color=chi_gender), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (ADS only)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.cgen
c.age.c.gen.3 <-  ggplot(c.age.vol.cds,
                       aes(x=AgeMonths, y=n, fill=chi_gender)) +
                 geom_smooth(aes(color=chi_gender), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (CDS only)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.cgen
c.age.c.gen.4 <-  ggplot(c.age.pro.cds,
                       aes(x=AgeMonths, y=n, fill=chi_gender)) +
                 geom_smooth(aes(color=chi_gender), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("% Utts (CDS)") + xlab("Age (months)") +
                 ylim(0,100) + misc.prefs + col.cgen


### child age by maternal education
c.age.m.edu.1 <-  ggplot(c.age.vol.all,
                       aes(x=AgeMonths, y=n, fill=mat_ed_num3)) +
                 geom_smooth(aes(color=mat_ed_num3), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (CDS+ADS)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.medu
c.age.m.edu.2 <-  ggplot(c.age.vol.ads,
                       aes(x=AgeMonths, y=n, fill=mat_ed_num3)) +
                 geom_smooth(aes(color=mat_ed_num3), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (ADS only)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.medu
c.age.m.edu.3 <-  ggplot(c.age.vol.cds,
                       aes(x=AgeMonths, y=n, fill=mat_ed_num3)) +
                 geom_smooth(aes(color=mat_ed_num3), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (CDS only)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.medu
c.age.m.edu.4 <-  ggplot(c.age.pro.cds,
                       aes(x=AgeMonths, y=n, fill=mat_ed_num3)) +
                 geom_smooth(aes(color=mat_ed_num3), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("% Utts (CDS)") + xlab("Age (months)") +
                 ylim(0,100) + misc.prefs + col.medu


### child age by adult gender
c.age.a.gen.1 <-  ggplot(a.gen.vol.all,
                       aes(x=AgeMonths, y=n, fill=adu_gender_m)) +
                 geom_smooth(aes(color=adu_gender_m), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (CDS+ADS)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.agen
c.age.a.gen.2 <-  ggplot(a.gen.vol.ads,
                       aes(x=AgeMonths, y=n, fill=adu_gender_m)) +
                 geom_smooth(aes(color=adu_gender_m), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (ADS only)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.agen
c.age.a.gen.3 <-  ggplot(a.gen.vol.cds,
                       aes(x=AgeMonths, y=n, fill=adu_gender_m)) +
                 geom_smooth(aes(color=adu_gender_m), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("# Utts (CDS only)") + xlab("Age (months)") +
                 ylim(0,500) + misc.prefs + col.agen
c.age.a.gen.4 <-  ggplot(a.gen.pro.cds,
                       aes(x=AgeMonths, y=n, fill=adu_gender_m)) +
                 geom_smooth(aes(color=adu_gender_m), method="lm") +
                 geom_jitter(size=6, shape=21) +
                 ylab("% Utts (CDS)") + xlab("Age (months)") +
                 ylim(0,100) + misc.prefs + col.agen


if (BW == "Y") {
  c.age.corp <- c.age.corp + scale_fill_grey(start = 1, end = 0)
  c.age.c.gen <- c.age.c.gen + scale_fill_grey(start = 1, end = 0)
  c.age.m.edu <- c.age.m.edu + scale_fill_grey(start = 1, end = 0)
  c.age.a.gen <- c.age.a.gen + scale_fill_grey(start = 1, end = 0)
  figname1 <- "simpleeffects-chiage_corpus-BW.png"
  figname3 <- "simpleeffects-chiage_chigen-BW.png"
  figname2 <- "simpleeffects-chiage_matedu-BW.png"
  figname4 <- "simpleeffects-chiage_adugen-BW.png"
}

ggsave(plot = multiplot(c.age.corp.1, c.age.corp.2,
                        c.age.corp.3, c.age.corp.4, cols=4),
       filename = figname1,
       path = results.plot.path,
       width = 80,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

ggsave(plot = multiplot(c.age.c.gen.1, c.age.c.gen.2,
                        c.age.c.gen.3, c.age.c.gen.4, cols=4),
       filename = figname2,
       path = results.plot.path,
       width = 80,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

ggsave(plot = multiplot(c.age.m.edu.1, c.age.m.edu.2,
                        c.age.m.edu.3, c.age.m.edu.4, cols=4),
       filename = figname3,
       path = results.plot.path,
       width = 80,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

ggsave(plot = multiplot(c.age.a.gen.1, c.age.a.gen.2,
                        c.age.a.gen.3, c.age.a.gen.4, cols=4),
       filename = figname4,
       path = results.plot.path,
       width = 80,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)
