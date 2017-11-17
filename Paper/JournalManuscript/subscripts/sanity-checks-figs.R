################################################################################
# BLOCK DUR SUMMARIES PRIOR TO EXCLUSIONS ####
blk.dur.plot <- ggplot(blockdata, aes(x=(blk_dur))) +
  geom_histogram(binwidth = 0.5) +
  xlab("block duration (min)") +
  geom_vline(xintercept = (mean(blockdata$blk_dur) +
                             OL.thresh*(sd(blockdata$blk_dur))), color="red")
ggsave(plot = blk.dur.plot,
       filename = "blockdurs-withoutliers.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

################################################################################
# TAG PREVALENCE WITH AGE ####
nrec.age <- IDS_demo_update %>%
            group_by(AgeMonths) %>%
            summarise(n_age = n())

all.tags <- xdscopy %>%
            group_by(lena_id) %>%
            summarise(tot_dur = sum(utt_dur)/60000,
                      n = n())

top.10.tags <- all.tags %>%
            arrange(-tot_dur) %>%
            slice(1:10) %>%
            dplyr::select(lena_id)

xdscopy.s <- xdscopy %>%
            semi_join(top.10.tags)

all.tags.age <- xdscopy %>%
            group_by(AgeMonths, lena_id) %>%
            summarise(tot_dur = sum(utt_dur)/60000,
                      n = n()) %>%
            left_join(nrec.age) %>%
            mutate(tot_dur_n = tot_dur/n_age,
                   n_n = n/n_age) %>%
            arrange(tot_dur, AgeMonths)

all.tags.ordered.dur <- all.tags %>%
            arrange(tot_dur)

all.tags.age.plot.dur <- ggplot(
    all.tags.age, aes(x = AgeMonths, y = tot_dur_n, color = lena_id)) +
    ylab("Avg. total duration (min)") + xlab("Child age (mo)")	+
    geom_point(size = 3) +
    geom_smooth(size = 3, method = "lm", se = F) +
	scale_x_continuous(limits=c(3,21),
		breaks=seq(3,21,3)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=26, angle=0, hjust=0.5))
ggsave(plot = all.tags.age.plot.dur,
       filename = "tags-with-age_ALL_dur.png",
       path = support.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)

all.tags.age.plot.n <- ggplot(
    all.tags.age, aes(x = AgeMonths, y = n_n, color = lena_id)) +
    ylab("Avg. # of clips") + xlab("Child age (mo)")	+
    geom_point(size = 3) +
    geom_smooth(size = 3, method = "lm", se = F) +
	scale_x_continuous(limits=c(3,21),
		breaks=seq(3,21,3)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=26, angle=0, hjust=0.5))
ggsave(plot = all.tags.age.plot.n,
       filename = "tags-with-age_ALL_n.png",
       path = support.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)

top.tags.age <- xdscopy.s %>%
            group_by(AgeMonths, lena_id) %>%
            summarise(tot_dur = sum(utt_dur)/60000,
                      n = n()) %>%
            left_join(nrec.age) %>%
            mutate(tot_dur_n = tot_dur/n_age,
                   n_n = n/n_age)


all.tags.age.plot.dur <- ggplot(
    top.tags.age, aes(x = AgeMonths, y = tot_dur_n, color = lena_id)) +
    ylab("Avg. total duration (min)") + xlab("Child age (mo)")	+
    geom_point(size = 3) +
    geom_smooth(size = 3, method = "lm", se = F) +
	scale_x_continuous(limits=c(3,21),
		breaks=seq(3,21,3)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=26, angle=0, hjust=0.5))
ggsave(plot = all.tags.age.plot.dur,
       filename = "tags-with-age_TOP10_dur.png",
       path = support.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)

all.tags.age.plot.n <- ggplot(
    top.tags.age, aes(x = AgeMonths, y = n_n, color = lena_id)) +
    ylab("Avg. # of clips") + xlab("Child age (mo)")	+
    geom_point(size = 3) +
    geom_smooth(size = 3, method = "lm", se = F) +
	scale_x_continuous(limits=c(3,21),
		breaks=seq(3,21,3)) +
	basic.theme +
	theme(axis.text.x =
			element_text(size=26, angle=0, hjust=0.5))
ggsave(plot = all.tags.age.plot.n,
       filename = "tags-with-age_TOP10_n.png",
       path = support.plot.path,
       width = 30,
       height = 20,
       units = "cm",dpi = 72,
       bg = "transparent"
)


################################################################################
# ADS AND CDS MinPH DISTRIBUTIONS ####
ads.minph.plot <- ggplot(adsratedata, aes(x=ads.minph)) +
  geom_histogram(binwidth = 0.5)
ggsave(plot = ads.minph.plot,
       filename = "ads-minutesperhour.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

cds.minph.plot <- ggplot(cdsratedata, aes(x=cds.minph)) +
  geom_histogram(binwidth = 0.5)
ggsave(plot = cds.minph.plot,
       filename = "cds-minutesperhour.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)


################################################################################
# Proportion CDS ####
prp.cds.plot <- ggplot(propCDS, aes(x=prp.cds)) +
  geom_histogram(binwidth = 0.01)
ggsave(plot = prp.cds.plot,
       filename = "cds-proportion.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)


################################################################################
# # CDS utterance length ####
# utt_dur.sec.plot <- ggplot(CDSutts, aes(x=mutt_len)) +
#   geom_histogram(binwidth = 0.1)
# ggsave(plot = utt_dur.sec.plot,
#        filename = "cds-uttlensec.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )


################################################################################
# MODEL RESIDUALS ####

# ADS minutes per hour ----
# Model 1: one datapoint per child ####
ads.mph.best.resid <- residuals(ads.mph.best)
ads.mph.resid.plot <- qqplot.data(ads.mph.best.resid)
ads.mph.best.int.resids <- data.frame(resid = ads.mph.best.resid)
ads.mph.resid.dist.plot <- ggplot(ads.mph.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = ads.mph.resid.plot,
       filename = "ads-minutesperhour-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = ads.mph.resid.dist.plot,
       filename = "ads-minutesperhour-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 2: two datapoints per child with exclusions ####
if (models == "logged/") {
  ads.agd.mph.match.resid <- residuals(ads.agd.s.mph.match)
  ads.agd.mph.match.resid.plot <- qqplot.data(ads.agd.mph.match.resid)
  ads.agd.mph.match.int.resids <- data.frame(resid = ads.agd.mph.match.resid)
  ads.agd.mph.match.resid.dist.plot <- ggplot(ads.agd.mph.match.int.resids, aes(x=resid)) +
                                geom_histogram(aes(y=..density..),
                                               fill="white", color="black",
                                               binwidth = 0.5) +
                               geom_density(color="firebrick3", lwd=2)
  ggsave(plot = ads.agd.mph.match.resid.plot,
         filename = "ads-minutesperhour-agd-modelresids-MATCHED.png",
         path = support.plot.path,
         width = 20,
         height = 10,
         units = "cm",dpi = 72,
         bg = "transparent"
  )
  ggsave(plot = ads.agd.mph.match.resid.dist.plot,
         filename = "ads-minutesperhour-agd-modelresiddist-MATCHED.png",
         path = support.plot.path,
         width = 20,
         height = 10,
         units = "cm",dpi = 72,
         bg = "transparent"
)
}

ads.agd.mph.best.resid <- residuals(ads.agd.s.mph.best)
ads.agd.mph.resid.plot <- qqplot.data(ads.agd.mph.best.resid)
ads.agd.mph.best.int.resids <- data.frame(resid = ads.agd.mph.best.resid)
ads.agd.mph.resid.dist.plot <- ggplot(ads.agd.mph.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = ads.agd.mph.resid.plot,
       filename = "ads-minutesperhour-agd-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = ads.agd.mph.resid.dist.plot,
       filename = "ads-minutesperhour-agd-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 3: Female ADS MinPH ####
fad.mph.best.resid <- residuals(fad.mph.best)
fad.mph.resid.plot <- qqplot.data(fad.mph.best.resid)
fad.mph.best.int.resids <- data.frame(resid = fad.mph.best.resid)
fad.mph.resid.dist.plot <- ggplot(fad.mph.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = fad.mph.resid.plot,
       filename = "ads-minutesperhour-FEM-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = fad.mph.resid.dist.plot,
       filename = "ads-minutesperhour-FEM-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 4: Male ADS MinPH ####
mad.mph.best.resid <- residuals(mad.mph.best)
mad.mph.resid.plot <- qqplot.data(mad.mph.best.resid)
mad.mph.best.int.resids <- data.frame(resid = mad.mph.best.resid)
mad.mph.resid.dist.plot <- ggplot(mad.mph.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = mad.mph.resid.plot,
       filename = "ads-minutesperhour-MAL-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = mad.mph.resid.dist.plot,
       filename = "ads-minutesperhour-MAL-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# CDS minutes per hour ----
# Model 1: one datapoint per child ####
cds.mph.best.resid <- residuals(cds.mph.best)
cds.mph.resid.plot <- qqplot.data(cds.mph.best.resid)
cds.mph.best.int.resids <- data.frame(resid = cds.mph.best.resid)
cds.mph.resid.dist.plot <- ggplot(cds.mph.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = cds.mph.resid.plot,
       filename = "cds-minutesperhour-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = cds.mph.resid.dist.plot,
       filename = "cds-minutesperhour-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 2: two datapoints per child with exclusions ####
cds.agd.mph.best.resid <- residuals(cds.agd.s.mph.best)
cds.agd.mph.resid.plot <- qqplot.data(cds.agd.mph.best.resid)
cds.agd.mph.best.int.resids <- data.frame(resid = cds.agd.mph.best.resid)
cds.agd.mph.resid.dist.plot <- ggplot(cds.agd.mph.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = cds.agd.mph.resid.plot,
       filename = "cds-minutesperhour-agd-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = cds.agd.mph.resid.dist.plot,
       filename = "cds-minutesperhour-agd-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 3: Female CDS MinPH ####
fcd.mph.best.resid <- residuals(fcd.mph.best)
fcd.mph.resid.plot <- qqplot.data(fcd.mph.best.resid)
fcd.mph.best.int.resids <- data.frame(resid = fcd.mph.best.resid)
fcd.mph.resid.dist.plot <- ggplot(fcd.mph.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = fad.mph.resid.plot,
       filename = "cds-minutesperhour-FEM-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = fad.mph.resid.dist.plot,
       filename = "cds-minutesperhour-FEM-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 4: Male CDS MinPH ####
mcd.mph.best.resid <- residuals(mcd.mph.best)
mcd.mph.resid.plot <- qqplot.data(mcd.mph.best.resid)
mcd.mph.best.int.resids <- data.frame(resid = mcd.mph.best.resid)
mcd.mph.resid.dist.plot <- ggplot(mcd.mph.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = mcd.mph.resid.plot,
       filename = "cds-minutesperhour-MAL-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = mcd.mph.resid.dist.plot,
       filename = "cds-minutesperhour-MAL-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)


# CDS proportion ----
# Model 1: one datapoint per child ####
cds.prp.best.resid <- residuals(cds.prp.best)
cds.prp.resid.plot <- qqplot.data(cds.prp.best.resid)
cds.prp.best.int.resids <- data.frame(resid = cds.prp.best.resid)
cds.prp.resid.dist.plot <- ggplot(cds.prp.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.05) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = cds.prp.resid.plot,
       filename = "cds-proportion-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = cds.prp.resid.dist.plot,
       filename = "cds-proportion-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 2: two datapoints per child with exclusions ####
cds.prp.agd.best.resid <- residuals(cds.prp.agd.best)
cds.prp.agd.resid.plot <- qqplot.data(cds.prp.agd.best.resid)
cds.prp.agd.best.int.resids <- data.frame(resid = cds.prp.agd.best.resid)
cds.prp.agd.resid.dist.plot <- ggplot(cds.prp.agd.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = cds.prp.agd.resid.plot,
       filename = "cds-proportion-agd-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = cds.prp.agd.resid.dist.plot,
       filename = "cds-proportion-agd-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 3: Female CDS proportion ####
fcd.prp.best.resid <- residuals(fcd.prp.best)
fcd.prp.resid.plot <- qqplot.data(fcd.prp.best.resid)
fcd.prp.best.int.resids <- data.frame(resid = fcd.prp.best.resid)
fcd.prp.resid.dist.plot <- ggplot(fcd.prp.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = fcd.prp.resid.plot,
       filename = "cds-proportion-FEM-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = fcd.prp.resid.dist.plot,
       filename = "cds-proportion-FEM-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model 4: Male CDS proportion ####
mcd.prp.best.resid <- residuals(mcd.prp.best)
mcd.prp.resid.plot <- qqplot.data(mcd.prp.best.resid)
mcd.prp.best.int.resids <- data.frame(resid = mcd.prp.best.resid)
mcd.prp.resid.dist.plot <- ggplot(mcd.prp.best.int.resids, aes(x=resid)) +
                              geom_histogram(aes(y=..density..),
                                             fill="white", color="black",
                                             binwidth = 0.5) +
                              geom_density(color="firebrick3", lwd=2)
ggsave(plot = mcd.prp.resid.plot,
       filename = "cds-proportion-MAL-modelresids.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = mcd.prp.resid.dist.plot,
       filename = "cds-proportion-MAL-modelresiddist.png",
       path = support.plot.path,
       width = 20,
       height = 10,
       units = "cm",dpi = 72,
       bg = "transparent"
)


# # CDS utterance length ----
# # Model 1: one datapoint per child ####
# cds.dur.best.best.resid <- residuals(cds.dur.best)
# cds.dur.best.resid.plot <- qqplot.data(cds.dur.best.best.resid)
# cds.dur.best.best.int.resids <- data.frame(resid = cds.dur.best.best.resid)
# cds.dur.best.resid.dist.plot <- ggplot(cds.dur.best.best.int.resids,
#                                           aes(x=resid)) +
#                                           geom_histogram(aes(y=..density..),
#                                                          fill="white",
#                                                          color="black",
#                                                          binwidth = 0.05) +
#                                           geom_density(color="firebrick3", lwd=2)
# ggsave(plot = cds.dur.best.resid.plot,
#        filename = "cds-uttdursec-modelresids.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# ggsave(plot = cds.dur.best.resid.dist.plot,
#        filename = "cds-uttdursec-modelresiddist.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# 
# # Model 2: two datapoints per child with exclusions ####
# cds.dur.agd.best.best.resid <- residuals(cds.dur.agd.best)
# cds.dur.agd.best.resid.plot <- qqplot.data(cds.dur.agd.best.best.resid)
# cds.dur.agd.best.best.int.resids <- data.frame(resid = cds.dur.agd.best.best.resid)
# cds.dur.agd.best.resid.dist.plot <- ggplot(cds.dur.agd.best.best.int.resids,
#                                           aes(x=resid)) +
#                                           geom_histogram(aes(y=..density..),
#                                                          fill="white",
#                                                          color="black",
#                                                          binwidth = 0.05) +
#                                           geom_density(color="firebrick3", lwd=2)
# ggsave(plot = cds.dur.agd.best.resid.plot,
#        filename = "cds-uttdursec-agd-modelresids.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# ggsave(plot = cds.dur.agd.best.resid.dist.plot,
#        filename = "cds-uttdursec-agd-modelresiddist.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# 
# # Model 3: Female CDS utterance length ####
# fcd.dur.best.best.resid <- residuals(fcd.dur.best)
# fcd.dur.best.resid.plot <- qqplot.data(fcd.dur.best.best.resid)
# fcd.dur.best.best.int.resids <- data.frame(resid = fcd.dur.best.best.resid)
# fcd.dur.best.resid.dist.plot <- ggplot(fcd.dur.best.best.int.resids,
#                                           aes(x=resid)) +
#                                           geom_histogram(aes(y=..density..),
#                                                          fill="white",
#                                                          color="black",
#                                                          binwidth = 0.05) +
#                                           geom_density(color="firebrick3", lwd=2)
# ggsave(plot = fcd.dur.best.resid.plot,
#        filename = "cds-uttdursec-FEM-modelresids.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# ggsave(plot = fcd.dur.best.resid.dist.plot,
#        filename = "cds-uttdursec-FEM-modelresiddist.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# 
# # Model 4: Male CDS utterance length ####
# mcd.dur.best.best.resid <- residuals(mcd.dur.best)
# mcd.dur.best.resid.plot <- qqplot.data(mcd.dur.best.best.resid)
# mcd.dur.best.best.int.resids <- data.frame(resid = mcd.dur.best.best.resid)
# mcd.dur.best.resid.dist.plot <- ggplot(mcd.dur.best.best.int.resids,
#                                           aes(x=resid)) +
#                                           geom_histogram(aes(y=..density..),
#                                                          fill="white",
#                                                          color="black",
#                                                          binwidth = 0.05) +
#                                           geom_density(color="firebrick3", lwd=2)
# ggsave(plot = mcd.dur.best.resid.plot,
#        filename = "cds-uttdursec-MAL-modelresids.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )
# ggsave(plot = mcd.dur.best.resid.dist.plot,
#        filename = "cds-uttdursec-MAL-modelresiddist.png",
#        path = support.plot.path,
#        width = 20,
#        height = 10,
#        units = "cm",dpi = 72,
#        bg = "transparent"
# )


################################################################################
# PREVALENCE OF NO-XDS BLOCKS ####
no.ads.plot <- ggplot(no.ads.blks, aes(x=ads.utt.N)) +
                      geom_histogram(binwidth = 1) +
                      facet_grid(. ~ adu_gender_m) +
                      xlim(-1,80) + ylim(-1,1500) +
                      xlab("# ADS utterances in block")

no.cds.plot <- ggplot(no.cds.blks, aes(x=cds.utt.N)) +
                      geom_histogram(binwidth = 1) +
                      facet_grid(. ~ adu_gender_m) +
                      xlim(-1,80) + ylim(-1,1500) +
                      xlab("# CDS utterances in block")

ggsave(plot = multiplot(no.ads.plot, no.cds.plot, cols=1),
       filename = "uttsperblock-FvM.png",
       path = support.plot.path,
       width = 20,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

no.ads.plot.all <- ggplot(no.ads.blks.all, aes(x=ads.utt.N)) +
                      geom_histogram(binwidth = 1) +
                      xlim(-1,80) + ylim(-1,1500) +
                      xlab("# ADS utterances in block")

no.cds.plot.all <- ggplot(no.cds.blks.all, aes(x=cds.utt.N)) +
                      geom_histogram(binwidth = 1) +
                      xlim(-1,80) + ylim(-1,1500) +
                      xlab("# CDS utterances in block")

ggsave(plot = multiplot(no.ads.plot.all, no.cds.plot.all, cols=1),
       filename = "uttsperblock-overall.png",
       path = support.plot.path,
       width = 10,
       height = 15,
       units = "cm",dpi = 72,
       bg = "transparent"
)

