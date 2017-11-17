misc.prefs <- list(basic.theme)
source(paste0(scripts, "_TEMP-editedGGPIRATE.R"))

col.agen <- list(
  scale_fill_manual(labels=agdlabels, values=agdcolors),
  scale_color_manual(labels=agdlabels, values=agdcolors))                 

col.agen.bu <- list(
  scale_fill_manual(labels=agdlabels, values=agdcolors.bu.gsc),
  scale_color_manual(labels=agdlabels, values=agdcolors.bu.gsc))                 

col.medu <- list(
  scale_fill_manual(labels=medlabels, values=medcolors),
  scale_color_manual(labels=medlabels, values=medcolors))

col.cgen <- list(
  scale_fill_manual(labels=cgdlabels, values=cgdcolors),
  scale_color_manual(labels=cgdlabels, values=cgdcolors))


## Figure requests:

# CDS minph (y-axis) by Maternal Education (x-axis) (maternal age?) bargraph or violinplot
# with CIs with individual kid dots overlain (jittered as needed)
# Requested 18 Oct 2017
cdsratedata$mat_ed_num3.l <- factor(cdsratedata$mat_ed_num3,
                                       labels = c("no-BA", "BA", "AD"))

cdsmph.mat_ed <- ggplot(cdsratedata,
      aes(x = mat_ed_num3.l, y = cds.minph)) +
      geom_pirate(bars = FALSE) +
      ylab("CDS (min/hr)") + xlab("Maternal education level")	+
	scale_y_continuous(limits=c(0,30),
		breaks=seq(0,30,5)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=30),
			axis.title.x =
			element_text(size=30),
			axis.title.y =
			element_text(size=30),
		legend.title =
			element_text(size=30))
ggsave(plot = cdsmph.mat_ed,
       filename = "FIN-CDSmph_by_MatEdu.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)

cdsratedata$mother_yr3 <- findInterval(cdsratedata$mother_dob, c(0, 25, 35)) 
cdsratedata$mother_yr3 <- factor(cdsratedata$mother_yr3, 
                                labels = c("17-24", "25-34", "35-42")) 

cdsmph.mat_yr <- ggplot(cdsratedata,
      aes(x = mother_dob, y = cds.minph)) +
      geom_point(size=6, alpha=0.6) +
      geom_smooth(method = "lm", size=3, color="cyan3", fill="cyan3") +
      ylab("CDS (min/hr)") + xlab("Maternal age (yr)")	+
	scale_y_continuous(limits=c(0,30),
		breaks=seq(0,30,5)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=20, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=20),
			axis.title.x =
			element_text(size=20),
			axis.title.y =
			element_text(size=20),
		legend.title =
			element_text(size=22))
ggsave(plot = cdsmph.mat_yr,
       filename = "FIN-CDSmph_by_MatAge.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# two panels: ADS minph (y axis) by Mat Age (x-axis)  scatterplot stat_smooth overlaid, next
# to ADS minph (y axis) by Child Age (x-axis) overall for both, not gender split
# Requested 18 Oct 2017
adsratedata$mother_yr3 <- findInterval(adsratedata$mother_dob, c(0, 25, 35)) 
adsratedata$mother_yr3 <- factor(adsratedata$mother_yr3, 
                                labels = c("17-24", "25-34", "35-42")) 

# panel 1
adsmph.mat_yr <- ggplot(adsratedata,
      aes(x = mother_dob, y = ads.minph)) +
      geom_point(size=6, alpha=0.6) +
      geom_smooth(method = "lm", size=3, color="cyan3", fill="cyan3") +
      ylab("ADS (min/hr)") + xlab("Maternal age (yr)")	+
	scale_y_continuous(limits=c(0,30),
		breaks=seq(0,30,5)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=30),
			axis.title.x =
			element_text(size=30),
			axis.title.y =
			element_text(size=30),
		legend.title =
			element_text(size=30))
ggsave(plot = adsmph.mat_yr,
       filename = "FIN-ADSmph_by_MatAge.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)
# panel 2
adsmph.chi_yr <- ggplot(adsratedata,
      aes(x = AgeMonths, y = ads.minph)) +
      geom_point(size=6, alpha=0.6) +
      geom_smooth(method = "lm", size=3, color="cyan3", fill="cyan3") +
      ylab("ADS (min/hr)") + xlab("Child age (mo)")	+
	scale_y_continuous(limits=c(-10,30),
		breaks=seq(0,30,5)) +
  coord_cartesian(ylim=c(0,30)) +
  basic.theme +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=30),
			axis.title.x =
			element_text(size=30),
			axis.title.y =
			element_text(size=30),
		legend.title =
			element_text(size=30))
ggsave(plot = adsmph.chi_yr,
       filename = "FIN-ADSmph_by_ChiAge.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)
# Conjoined
ads.multiplot <- ggarrange(
    adsmph.mat_yr, adsmph.chi_yr, 
    labels = c("A", "B"),
    ncol = 2)
ggsave(plot = ads.multiplot,
       filename = "FIN-ADSmph_by_MatAge_AND_by_ChiAge.png",
       path = results.plot.path,
       width = 60,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)


# a three-panel figure with CDS minph, ADS minph and CDS prop (y axes) by child age (x axis)
# with color showing ADU gender (same as the old 4 panel without cds utt length)
# Requested 18 Oct 2017
# panel 1
cdsmph.chi_yr.adg <- ggplot(cdsratedata.agd,
      aes(x = AgeMonths, y = cds.minph, color=adu_gender_m, fill = adu_gender_m)) +
      geom_point(aes(color=adu_gender_m),  size=6, alpha=0.6) +
      geom_smooth(aes(color=adu_gender_m, fill = adu_gender_m), method = "lm") +
      ylab("CDS (min/hr)") + xlab("Child age (mo)")	+
	scale_y_continuous(limits=c(-10,30),
		breaks=seq(0,30,5)) +
  coord_cartesian(ylim=c(0,30)) +
  basic.theme + col.agen.bu +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=30),
			axis.title.x =
			element_text(size=30),
			axis.title.y =
			element_text(size=30),
		legend.title =
			element_text(size=30),
		legend.text = element_text(size=30)) +
  guides(color = guide_legend("Speaker gender"),
        fill = guide_legend("Speaker gender")) +
  # comment out this last line if you want the legend
  theme(legend.position="none")
ggsave(plot = cdsmph.chi_yr.adg,
       filename = "FIN-CDSmph_by_ChiAge_and_AduGender.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)
# panel 2
adsmph.chi_yr.adg <- ggplot(adsratedata.agd,
      aes(x = AgeMonths, y = ads.minph, color=adu_gender_m, fill = adu_gender_m)) +
      geom_point(aes(color=adu_gender_m),  size=6, alpha=0.6) +
      geom_smooth(aes(color=adu_gender_m, fill = adu_gender_m), method = "lm") +
      ylab("ADS (min/hr)") + xlab("Child age (mo)")	+
	scale_y_continuous(limits=c(-10,30),
		breaks=seq(0,30,5)) +
  coord_cartesian(ylim=c(0,30)) +
  basic.theme + col.agen.bu +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=30),
			axis.title.x =
			element_text(size=30),
			axis.title.y =
			element_text(size=30),
		legend.title =
			element_text(size=30),
		legend.text = element_text(size=30)) +
  guides(color = guide_legend("Speaker gender"),
        fill = guide_legend("Speaker gender")) +
  # comment out this last line if you want the legend
  theme(legend.position="none")
ggsave(plot = adsmph.chi_yr.adg,
       filename = "FIN-ADSmph_by_ChiAge_and_AduGender.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)
# panel 3
cdsprp.chi_yr.adg <- ggplot(propCDS.agd,
      aes(x = AgeMonths, y = prp.cds, color=adu_gender_m, fill = adu_gender_m)) +
      geom_point(aes(color=adu_gender_m),  size=6, alpha=0.6) +
      geom_smooth(aes(color=adu_gender_m, fill = adu_gender_m), method = "lm") +
      ylab("Proportion CDS") + xlab("Child age (mo)")	+
	scale_y_continuous(limits=c(0,2),
		breaks=seq(0,1,0.2)) +
  coord_cartesian(ylim=c(0,1)) +
  basic.theme + col.agen.bu +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=30),
			axis.title.x =
			element_text(size=30),
			axis.title.y =
			element_text(size=30),
		legend.title =
			element_text(size=30),
		legend.text = element_text(size=30)) +
  guides(color = guide_legend("Speaker gender"),
        fill = guide_legend("Speaker gender")) +
  # comment out this last line if you want the legend
  theme(legend.position="none")
ggsave(plot = cdsprp.chi_yr.adg,
       filename = "FIN-CDSprp_by_ChiAge_and_AduGender.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)
# Conjoined
xds_cdsprp.multiplot <- grid_arrange_shared_legend(cdsmph.chi_yr.adg, adsmph.chi_yr.adg,
                           cdsprp.chi_yr.adg,
                           ncol = 3, nrow = 1)
ggsave(plot = xds_cdsprp.multiplot,
       filename = "FIN-XDSquantity_and_CDSprop_by_ChiAge.png",
       path = results.plot.path,
       width = 60,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)


# maternal age by maternal edu (maybe for supplementals?)
# Requested 18 Oct 2017
IDS_demo_update$mat_ed_num3.l <- factor(IDS_demo_update$mat_ed_num3,
                                       labels = c("no-BA", "BA", "AD"))

matedu.matyr <- ggplot(IDS_demo_update,
      aes(x = mat_ed_num3.l, y = mother_dob)) +
      geom_pirate(bars = FALSE) +
      ylab("Maternal age (yr)") + xlab("Maternal education level")	+
	scale_y_continuous(limits=c(15,45),
		breaks=seq(15,45,5)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=20, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=20),
			axis.title.x =
			element_text(size=20),
			axis.title.y =
			element_text(size=20),
		legend.title =
			element_text(size=20),
		legend.text = element_text(size=20))
ggsave(plot = matedu.matyr,
       filename = "SUP-MatAge_by_MatEdu.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)


# overall CDS proportion histogram?
# Requested 18 Oct 2017
cdsmph.hist <- ggplot(cdsratedata, aes(x=cds.minph)) +
      geom_histogram(binwidth = 2, fill = "gray90", color = "black") +
      ylab("# Children") + xlab("CDS (min/hr)")	+
	scale_x_continuous(limits=c(0,30),
		breaks=seq(0,30,5)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=20, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=20),
			axis.title.x =
			element_text(size=20),
			axis.title.y =
			element_text(size=20),
		legend.title =
			element_text(size=22))
ggsave(plot = cdsmph.hist,
       filename = "SUP-CDSmph_overall_histogram.png",
       path = results.plot.path,
       width = 30,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# CDS (y-axis) prop by Mat Age (xaxis) overall for both not gender split
# Requested 18 Oct 2017
cdsprp.mother_yr <- ggplot(propCDS,
      aes(x = mother_dob, y = prp.cds)) +
      geom_point(size=6, alpha=0.6) +
      geom_smooth(method = "lm", size=3, color="cyan3", fill="cyan3") +
      ylab("Proportion CDS") + xlab("Maternal age (yr)")	+
	scale_y_continuous(limits=c(0,1),
		breaks=seq(0,1,0.2)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=30, angle=0, hjust=0.5),
			axis.text.y =
			element_text(size=30),
			axis.title.x =
			element_text(size=30),
			axis.title.y =
			element_text(size=30),
		legend.title =
			element_text(size=30),
		legend.text = element_text(size=30))
ggsave(plot = cdsprp.mother_yr,
       filename = "FIN-CDSprp_by_MatAge.png",
       path = results.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)




# # Age + adult gender effect plots ####
# figname.ageagd <- "age-and-adugender-effects.png"
# 
# # ADS minutes per hour
# adsrate.plot <-  ggplot(adsratedata,
#                        aes(x=AgeMonths, y=ads.minph, fill=adu_gender_m)) +
#                  geom_smooth(aes(color=adu_gender_m), method="lm") +
#                  geom_jitter(size=6, shape=21) +
#                  ylab("ADS min/hr") + xlab("Age (months)") +
#                  scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
#                  misc.prefs + col.agen +
#                  guides(color = guide_legend("Speaker gender"),
#                         fill = guide_legend("Speaker gender"))
# 
# # CDS minutes per hour
# cdsrate.plot <-  ggplot(cdsratedata,
#                        aes(x=AgeMonths, y=cds.minph, fill=adu_gender_m)) +
#                  geom_smooth(aes(color=adu_gender_m), method="lm") +
#                  geom_jitter(size=6, shape=21) +
#                  ylab("CDS min/hr") + xlab("Age (months)") +
#                  scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
#                  misc.prefs + col.agen +
#                  guides(color = guide_legend("Speaker gender"),
#                         fill = guide_legend("Speaker gender"))
# 
# # Proportion CDS
# propCDS.plot <-  ggplot(propCDS,
#                  aes(x=AgeMonths, y=prp.cds, fill=adu_gender_m)) +
#                  geom_smooth(aes(color=adu_gender_m), method="lm") +
#                  geom_jitter(size=6, shape=21) +
#                  ylab("CDS/XDS (sec)") + xlab("Age (months)") +
#                  scale_y_continuous(expand = c(0.1, 0.1), limits = c(-0.001,1.100)) + #eb edit
#                  misc.prefs + col.agen +
#                  guides(color = guide_legend("Speaker gender"),
#                         fill = guide_legend("Speaker gender"))
# 
# # CDS utterance length (made into by-participant averages)
# CDSutts.byID <-   CDSutts %>%
#                   group_by(ID, adu_gender_m) %>%
#                   summarise(utt_dur.sec.med = median(utt_dur.sec)) %>%
#                   left_join(IDS_demo_update, by = "ID")
# 
# CDSuttlen.plot <-  ggplot(CDSutts.byID,
#                    aes(x=AgeMonths, y=utt_dur.sec.med, fill=adu_gender_m)) +
#                    geom_smooth(aes(color=adu_gender_m), method="lm") +
#                    geom_jitter(size=6, shape=21) +
#                    ylab("CDS utt len (sec)") + xlab("Age (months)") +
#                    scale_y_continuous(expand = c(0, 0), limits = c(0.5,2.5)) +
#                    misc.prefs + col.agen +
#                    guides(color = guide_legend("Speaker gender"),
#                           fill = guide_legend("Speaker gender"))
# 
# if (BW == "Y") {
#   adsrate.plot <- adsrate.plot +
#                   scale_fill_grey(start = 1, end = 0) +
#                   scale_color_grey(start = 1, end = 0)
#   cdsrate.plot <- cdsrate.plot +
#                   scale_fill_grey(start = 1, end = 0) +
#                   scale_color_grey(start = 1, end = 0)
#   propCDS.plot <- propCDS.plot +
#                   scale_fill_grey(start = 1, end = 0) +
#                   scale_color_grey(start = 1, end = 0)
#   CDSuttlen.plot <- CDSuttlen.plot +
#                   scale_fill_grey(start = 1, end = 0) +
#                   scale_color_grey(start = 1, end = 0)
#   figname.ageagd <- "age-and-adugender-effects-BW.png"
# }
# 
# ageagd.multiplot <- grid_arrange_shared_legend(adsrate.plot, cdsrate.plot,
#                            propCDS.plot, CDSuttlen.plot,
#                            ncol = 2, nrow = 2)
# 
# ggsave(plot = ageagd.multiplot,
#        filename = figname.ageagd,
#        path = results.plot.path,
#        width = 60,
#        height = 40,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# 
# # Age + maternal education effect plots ####
# # same data, different grouping factor
# figname.agemed <- "age-and-matedu-effects.png"
# 
# # ADS minutes per hour
# adsrate.med.plot <-  ggplot(adsratedata,
#                        aes(x=AgeMonths, y=ads.minph, fill=factor(mat_ed_num3))) +
#                  geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
#                  geom_jitter(size=6, shape=21) +
#                  ylab("ADS min/hr") + xlab("Age (months)") +
#                  scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
#                  misc.prefs + col.medu +
#                  guides(color = guide_legend("Maternal education"),
#                         fill = guide_legend("Maternal education"))#+facet_wrap(~adu_gender_m)
# 
# # CDS minutes per hour
# cdsrate.med.plot <-  ggplot(cdsratedata,
#                        aes(x=AgeMonths, y=cds.minph, fill=factor(mat_ed_num3))) +
#                  geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
#                  geom_jitter(size=6, shape=21) +
#                  ylab("CDS min/hr") + xlab("Age (months)") +
#                  scale_y_continuous(expand = c(0.01, 0.01), limits = c(-1,10)) +
#                  misc.prefs + col.medu +
#                  guides(color = guide_legend("Maternal education"),
#                         fill = guide_legend("Maternal education"))#+facet_wrap(~adu_gender_m)
# 
# # Proportion CDS
# propCDS.med.plot <-  ggplot(propCDS,
#                  aes(x=AgeMonths, y=prp.cds, fill=factor(mat_ed_num3))) +
#                  geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
#                  geom_jitter(size=6, shape=21) +
#                  ylab("CDS/XDS (sec)") + xlab("Age (months)") +
#                  scale_y_continuous(expand = c(0.1, 0.1), limits = c(-0.001,1.10)) + #eb edit
#                  misc.prefs + col.medu +
#                  guides(color = guide_legend("Maternal education"),
#                         fill = guide_legend("Maternal education"))#+facet_wrap(~adu_gender_m)
# 
# # CDS utterance length (made into by-participant averages)
# CDSuttlen.med.plot <-  ggplot(CDSutts.byID,
#                    aes(x=AgeMonths, y=utt_dur.sec.med, fill=factor(mat_ed_num3))) +
#                    geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
#                    geom_jitter(size=6, shape=21) +
#                    ylab("CDS utt len (sec)") + xlab("Age (months)") +
#                    scale_y_continuous(expand = c(0, 0), limits = c(0.5,2.5)) +
#                    misc.prefs + col.medu +
#                    guides(color = guide_legend("Maternal education"),
#                           fill = guide_legend("Maternal education"))
# 
# if (BW == "Y") {
#   adsrate.med.plot <- adsrate.med.plot +
#                   scale_fill_grey(start = 0.8, end = 0.2) +
#                   scale_color_grey(start = 0.8, end = 0.2)
#   cdsrate.med.plot <- cdsrate.med.plot +
#                   scale_fill_grey(start = 0.8, end = 0.2) +
#                   scale_color_grey(start = 0.8, end = 0.2)
#   propCDS.med.plot <- propCDS.med.plot +
#                   scale_fill_grey(start = 0.8, end = 0.2) +
#                   scale_color_grey(start = 0.8, end = 0.2)
#   CDSuttlen.med.plot <- CDSuttlen.med.plot +
#                   scale_fill_grey(start = 0.8, end = 0.2) +
#                   scale_color_grey(start = 0.8, end = 0.2)
#   figname.agemed <- "age-and-matedu-effects-BW.png"
# }
# 
# agemed.multiplot <- grid_arrange_shared_legend(adsrate.med.plot, cdsrate.med.plot,
#                            propCDS.med.plot, CDSuttlen.med.plot,
#                            ncol = 2, nrow = 2)
# 
# ggsave(plot = agemed.multiplot,
#        filename = figname.agemed,
#        path = results.plot.path,
#        width = 60,
#        height = 40,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# 
# # 3-wy effect with maternal education plot ####
# chigens <- list(
#   'female'="Girl",
#   'male'="Boy"
# )
# 
# chigen_labeller <- function(variable,value){
#   return(chigens[value])
# }
# 
# figname.agecgdmed <- "age-chigender-matedu-effect.png"
# 
# CDSuttlen.med.3wy1.plot <-  ggplot(CDSutts.byID,
#                    aes(x=AgeMonths, y=utt_dur.sec.med, fill=factor(mat_ed_num3)),
#                    group=chi_gender) +
#                    geom_smooth(aes(color=factor(mat_ed_num3)), method="lm") +
#                    geom_jitter(size=6, shape=21) +
#                    facet_grid(. ~ chi_gender, labeller=chigen_labeller) +
#                    ylab("CDS utt len (sec)") + xlab("Age (months)") +
#                    scale_y_continuous(expand = c(0, 0), limits = c(0.5,2.5)) +
#                    misc.prefs + col.medu +
#                    guides(color = guide_legend("Mat. education"),
#                           fill = guide_legend("Mat. education"))
# 
# if (BW == "Y") {
#   CDSuttlen.med.3wy1.plot <- CDSuttlen.med.3wy1.plot +
#                   scale_fill_grey(start = 0.8, end = 0.2) +
#                   scale_color_grey(start = 0.8, end = 0.2)
#   figname.agecgdmed <- "age-chigender-matedu-effect-BW.png"
# }
# 
# ggsave(plot = CDSuttlen.med.3wy1.plot,
#        filename = figname.agecgdmed,
#        path = results.plot.path,
#        width = 40,
#        height = 20,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# 
# # 3-wy effect with adult gender education plot ####
# figname.agecgdagd <- "age-chigender-adugender-effect.png"
# 
# CDSuttlen.agd.3wy1.plot <-  ggplot(CDSutts.byID,
#                    aes(x=AgeMonths, y=utt_dur.sec.med, fill=adu_gender_m),
#                    group=chi_gender) +
#                    geom_smooth(aes(color=adu_gender_m), method="lm") +
#                    geom_jitter(size=6, shape=21) +
#                    facet_grid(. ~ chi_gender, labeller=chigen_labeller) +
#                    ylab("CDS utt len (sec)") + xlab("Age (months)") +
#                    scale_y_continuous(expand = c(0, 0), limits = c(0.5,2.5)) +
#                    misc.prefs + col.agen +
#                    guides(color = guide_legend("Speaker gender"),
#                           fill = guide_legend("Speaker gender"))
# 
# if (BW == "Y") {
#   CDSuttlen.agd.3wy1.plot <- CDSuttlen.agd.3wy1.plot +
#                   scale_fill_grey(start = 0.8, end = 0.2) +
#                   scale_color_grey(start = 0.8, end = 0.2)
#   figname.agecgdagd <- "age-chigender-adugender-effect-BW.png"
# }
# 
# 
# ggsave(plot = CDSuttlen.agd.3wy1.plot,
#        filename = figname.agecgdagd,
#        path = results.plot.path,
#        width = 40,
#        height = 20,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# 
# # lena error gender plot
# figname.lenahumange <- "lena-human-adugen-count.png"
# 
# Lena_human_gender.plot <- ggplot(xdsall, aes(lena_id, fill = adu_gender_m))+geom_bar()+theme_bw(base_size=18)
# 
# if (BW == "Y") {
#   Lena_human_gender.plot <- Lena_human_gender.plot +
#     scale_fill_grey(start = 0.8, end = 0.2) +
#     scale_color_grey(start = 0.8, end = 0.2)
#   figname.agecgdagd <- "age-chigender-adugender-effect-BW.png"
# }
# 
# ggsave(plot = Lena_human_gender.plot,
#        filename = figname.lenahumange,
#        path = results.plot.path,
#        width = 40,
#        height = 20,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# 
# #ggplot(xdsall %>% filter(!adu_gender_m%in% c("NMJ","NA")), aes(adu_gender_m, fill = lena_id))+geom_bar(position = "fill")+theme_bw(base_size=18)+facet_wrap(~label_m)
# #ggplot(xdsall %>% filter(!label_m%in% c("NMJ","JUNK")), aes(label_m, fill = lena_id))+geom_bar(position = "fill")+theme_bw(base_size=18)+facet_wrap(~adu_gender_m)+scale_fill_manual(values = c("#f8766d","#53B400"))
# ggplot(xdsall %>% filter(!label_m%in% c("NMJ","JUNK")), aes(label_m, fill = lena_id))+geom_bar(position = "fill")+theme_bw(base_size=18)+facet_wrap(~adu_gender_m)+scale_fill_manual(values = c("pink","blue"))

