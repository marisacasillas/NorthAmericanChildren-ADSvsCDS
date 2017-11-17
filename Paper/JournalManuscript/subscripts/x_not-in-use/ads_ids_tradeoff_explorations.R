#scratchpad for ads/cds tradeoff explorations for EB & MC
#depends on _XDS-DARCLE.R being sourced first
source('~/Dropbox/Interspeech2017/ADS-IDS/ArticleManuscript/_XDS-DARCLE.R')


#it's possible i've done something silly due to not understand previous aggregation steps, lmk!

#first lets look at just the # of segments per block
nseg_perblock <- blkall %>% 
  left_join(dplyr::select(chiall, ID, AgeMonths, Corpus)) %>% 
  filter(instance==0) %>% 
  group_by(block,  AgeMonths, Corpus) %>% 
  tally()

# n segments in a block definitely goes down with age (this is more extreme if you filter long blocks eg. >200 or 300 segs)
ggplot(nseg_perblock, aes(AgeMonths, n, color=Corpus))+geom_jitter(width=.2)+geom_smooth(aes(group=1))+
  theme_bw(base_size=18)
#born out statistically:
cor.test(nseg_perblock$AgeMonths, nseg_perblock$n, conf.int=T, method = "spearman")# highly sig but not numerically huge correlation, -.28

#okay, and how to different talkers vary with age?
lena_id_per_block <- blkall %>% 
  left_join(dplyr::select(chiall, ID, AgeMonths)) %>% 
  group_by(lena_id, label, clan_file,block, instance, AgeMonths) %>% 
  filter(instance==0) %>% 
  tally()
 

# number of different kinds of participants in blocks goes down a bit with age
ggplot(lena_id_per_block, aes(AgeMonths, n))+geom_point()+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", aes(group=clan_file), color = "red")+
  stat_smooth()+
  facet_wrap(~lena_id, scales="free_y", nrow=2)+
  theme_bw(base_size=18)

# most relevantly, let's look at the big-n ones
ggplot(lena_id_per_block %>% filter(lena_id %in% c("CHN","FAN","MAN","OLN","SIL","CXN")), aes(AgeMonths, n))+geom_point()+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange", aes(group=clan_file), color = "red")+
  stat_smooth()+
  facet_wrap(~lena_id, scales="free_y", nrow=2)+
  theme_bw(base_size=18)

#make it wide for stats-ease
lena_id_per_block_wide <- lena_id_per_block %>% 
  filter(lena_id %in% c("CHN","FAN","MAN","OLN","SIL","CXN")) %>% 
  spread(lena_id, n)
#correlation between # of diarization tags for  that speaker, and age
cor.test(lena_id_per_block_wide$AgeMonths, lena_id_per_block_wide$CHN, conf.int=T, method = "spearman")#nada
cor.test(lena_id_per_block_wide$AgeMonths, lena_id_per_block_wide$FAN, conf.int=T, method = "spearman")# ding ding ding
cor.test(lena_id_per_block_wide$AgeMonths, lena_id_per_block_wide$MAN, conf.int=T, method = "spearman")# ding ding ding
cor.test(lena_id_per_block_wide$AgeMonths, lena_id_per_block_wide$OLN, conf.int=T, method = "spearman")# ding ding ding
cor.test(lena_id_per_block_wide$AgeMonths, lena_id_per_block_wide$SIL, conf.int=T, method = "spearman")# ding ding ding okay that's weird.
cor.test(lena_id_per_block_wide$AgeMonths, lena_id_per_block_wide$CXN, conf.int=T, method = "spearman")# ding ding ding
#all these spearker counts go down with age except CHN, about -.1 to -.2 correlations, 
#most of them quite highly sig so could withstand correction for mult testif need be

#let's look at just fanman in a histogram
#huh, so there are way fewer FAN/MAN *segments* with age
ggplot(xdsall, aes(AgeMonths, fill=lena_id))+geom_histogram(binwidth=2)

# and as a function of XDS we see the pattern strongest in ADS
ggplot(xdsall, aes(AgeMonths, fill = lena_id))+geom_histogram(binwidth=2)+theme_bw(base_size=18)+facet_wrap(~label_m)

#but there are actually way *more* CDS segments in older infants, relative to ADS
# this is another way of saying ADS goes down with age but CDS doesn't, which prompted this exploration
ggplot(xdsall, aes(label_m, fill = lena_id))+geom_bar()+theme_bw(base_size=18)+
  facet_wrap(~cut(AgeMonths,4), scales = "free_y")


#switching gears to look at this as a function of duration
#duration drops more sharplyfor ads, but there is a more modest drop across first 2 age bins
#(arbitrary age bins by cutting age into 4, which probably map on almost exactly to warlaumont vs. other corpora)
ggplot(propCDS_collag)+
  geom_jitter(aes(cut(AgeMonths,4),cds.sec), shape = 5, color ='black', width = .2)+
  geom_jitter(aes(cut(AgeMonths,4),ads.sec), shape = 20, color = "red", width=.2)+
  theme_bw(base_size=18)+
  ylab("xds.sec (black=cds, red=ads)")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange",aes(cut(AgeMonths,4),cds.sec),color="black")+
  stat_summary(fun.data=mean_cl_boot, geom="pointrange",aes(cut(AgeMonths,4),ads.sec), color = "red")

#same idea, unbinned, with smoother
ggplot(propCDS_collag)+
  geom_jitter(aes(AgeMonths,cds.sec), shape = 5, color ='black', width = .2)+
  geom_jitter(aes(AgeMonths,ads.sec), shape = 20, color = "red", width=.2)+
  theme_bw(base_size=18)+
  ylab("xds.sec (black=cds, red=ads)")+
  stat_smooth(aes(AgeMonths, ads.sec), fill = "red", linetype=1)+
  stat_smooth(aes(AgeMonths, cds.sec),fill = "black", linetype=2)

#this should be the mean totallang (FAN or MAN s of input iirc) per kid per agebin (should double check)
ggplot(propCDS_collag, aes(cut(AgeMonths,4),totallang, fill=cut(AgeMonths,4), group=ChiID))+stat_summary(fun.y=mean, na.rm=T,geom ="bar")+theme_bw(base_size=18)+
  facet_wrap(~cut(AgeMonths,4), scales = "free_y")

#okay so now let's directly compare #FAN or MAN segs, sum of the duration, mean duration, sum of whole block dur and mean of block dur
subj_FANMAN <- xdsall %>% 
  group_by(ChiID, AgeMonths, Corpus) %>% 
  summarise(
        n_FANMAN = n(),
         sum_FANMAN = sum(utt_dur, na.rm=T),
         mean_FANMAN = mean(utt_dur, na.rm=T),
         sum_blk_dur = sum(blk_dur, na.rm=T),
         mean_blk_dur = mean(blk_dur, na.rm=T)) 

#gather to lnog for faceting
subj_FANMAN_long <- subj_FANMAN %>%   gather(attribute, measurement, n_FANMAN:mean_blk_dur)

ggplot(subj_FANMAN_long, aes(AgeMonths, measurement, color = attribute, shape = Corpus, group=ChiID))+
  geom_jitter(width=.2, size=3)+
  stat_smooth(aes(group=attribute))+
  facet_wrap(~attribute, scales="free_y", nrow=5)+
  theme_bw(base_size=18)+
  scale_shape(solid=F)

#oh yeah, all of these things are correlated negatively with age
#all rho between -.34 and -.55, all highly sig
cor.test(subj_FANMAN$AgeMonths, subj_FANMAN$mean_blk_dur, method="spearman", conf.int=T)
cor.test(subj_FANMAN$AgeMonths, subj_FANMAN$mean_FANMAN, method="spearman", conf.int=T)
cor.test(subj_FANMAN$AgeMonths, subj_FANMAN$n_FANMAN, method="spearman", conf.int=T)
cor.test(subj_FANMAN$AgeMonths, subj_FANMAN$sum_blk_dur, method="spearman", conf.int=T)
cor.test(subj_FANMAN$AgeMonths, subj_FANMAN$sum_FANMAN, method="spearman", conf.int=T)

#okay, new vague hypothesis: lots of things go down with age: amount of speech, # of segments from certain speakers, block length (in #segms & duration)
# so then, ADS going down is part of this...and the fact that CDS does *not* go down is part of why propCDS goes up
# so babies are hearing less talk overall as they get older, hence ADS dropping, but more of it is CDS, hence CDS staying 'stable' 
# if that's the case, then it's interesting, but i'm wodndering if there's any reason
# the algorithm would be making shorter blocks as babies get older vs. blocks actually being shorter?  I wrote to jill gilkerson, tbd.