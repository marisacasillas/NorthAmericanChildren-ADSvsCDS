################################################################################
# CODER RELIABILITY
uniq_coders <- blkall %>%
                dplyr::select(coder) %>%
                distinct()
# CDS/ADS label
relcoders_label <- blkall %>%
  filter(lena_id == "MAN" | lena_id == "FAN") %>%
  dplyr::select(coder, label, clan_file, timestamp) %>%
  spread(coder, label) %>%
  dplyr::select(-clan_file, -timestamp) %>% 
  as.matrix() %>% 
  t(.)
#relcoders_label <- relcoders_label %>% mutate(allCDS = as.factor("CDS"))
#summary(relcoders_label)
kripp.alpha(relcoders_label)

# Speaker gender label
relcoders_gender <- blkall %>%
  filter(lena_id == "MAN" | lena_id == "FAN") %>%
  dplyr::select(coder, adu_gender, clan_file, timestamp) %>%
  spread(coder, adu_gender) %>%
  dplyr::select(-clan_file, -timestamp)%>% 
  as.matrix() %>% 
  t(.)
kripp.alpha(relcoders_gender)


################################################################################
# PREDICTOR CORRELATIONS
# Maternal age and maternal education: Significantly correlated
cor.test(IDS_demo_update$mother_dob, IDS_demo_update$mat_ed_num3,
         method = "spearman")
# Maternal age and number of older siblings: Significantly correlated
cor.test(IDS_demo_update$mother_dob, IDS_demo_update$number_older_sibs,
         method = "spearman")
# Maternal education and number of older siblings: No correlation
cor.test(IDS_demo_update$mat_ed_num3, IDS_demo_update$number_older_sibs,
         method = "spearman")


################################################################################
# BLOCK DUR SUMMARIES PRIOR TO EXCLUSIONS
# Table of block duration information prior to block exclusion
blkdurs
# ...and by corpus
blkdurs.cp


################################################################################
# TESTING FOR AN AGE EFFECT ON JUNK CLIPS
MANFAN.tot <- xdsall %>%
              group_by(AgeMonths) %>%
              summarise(all_n = n())

# counting both "junk" and ambiguous "non-majority" cases as junk
junkdata <- xdsall %>%
              # 1798 utterances with XDS not equal to CDS or ADS
              # (1730 agreed-upon as junk and 68 without a majority score)
						  filter(label_m != "CDS" & label_m != "ADS") %>%
              mutate(junk = 1) %>%
              group_by(AgeMonths) %>%
              summarise(junk_n = n()) %>%
              left_join(MANFAN.tot) %>%
              mutate(jnk.prp = junk_n/all_n)

# No correlation of child age and junk/ambiguous codes
cor.test(junkdata$jnk.prp, junkdata$AgeMonths,
         method = "spearman")

# counting just "junk" as junk
junkdata.excl <- xdsall %>%
              # 1798 utterances with XDS not equal to CDS or ADS
              # (1730 agreed-upon as junk and 68 without a majority score)
						  filter(label_m == "JUNK") %>%
              mutate(junk = 1) %>%
              group_by(AgeMonths) %>%
              summarise(junk_n = n()) %>%
              left_join(MANFAN.tot) %>%
              mutate(jnk.prp = junk_n/all_n)

# No correlation of child age and junk codes alone
cor.test(junkdata.excl$jnk.prp, junkdata.excl$AgeMonths,
         method = "spearman")

################################################################################
# LOOKING AT LENA ERRORS
xdsall.copy <- xdsall %>%
              mutate(fem.right = ifelse(lena_id == "FAN" & adu_gender_m == "FEMALE", 1, 0),
                     mal.right = ifelse(lena_id == "MAN" & adu_gender_m == "MALE", 1, 0),
                     gen.right = ifelse(fem.right == 1 | mal.right == 1, 1, 0))

# overall divergence from lena's gender labels
gen.diverg.overall <- 1-mean(xdsall.copy$gen.right)

fanman.tot <- xdsall.copy %>%
              group_by(lena_id) %>%
						  summarise(tot_tags = n())

lena.err.xds <- xdsall.copy %>%
						  group_by(lena_id, adu_gender_m) %>%
						  summarise(n = n()) %>%
              left_join(fanman.tot) %>%
              mutate(perc = n/tot_tags)

fanman.tot.all <- xdsall.copy %>%
              group_by(lena_id, label_m) %>%
						  summarise(tot_tags = n())

lena.err.all <- xdsall.copy %>%
						  group_by(lena_id, label_m, adu_gender_m) %>%
						  summarise(n = n()) %>%
              left_join(fanman.tot.all) %>%
              mutate(perc = n/tot_tags)

# lena thought the speaker was a female when he was actually was a male
# 9.5% of the time in CDS but only 3.8% of the time for ADS
#  2     FAN     ADS         MALE   103     2727 0.0377704437
#  5     FAN     CDS         MALE   489     5118 0.0955451348
# conversely, lena thought the speaker was actually a male when she was actually a female
# 14     MAN     ADS       FEMALE   516     1527 0.3379174853
# 17     MAN     CDS       FEMALE   330     1514 0.2179656539


################################################################################
# PREVALENCE OF NO-XDS BLOCKS
uniq.blks <- tibble(uniq_block = blockdata$uniq_block)
blockdata2 <- blockdata %>% mutate_if(sapply(., is.character), as.factor)

# Split on gender of speaker
no.ads.blks <-  modeldata.excl %>%
                filter(label_m == "ADS") %>%
                mutate(uniq_block = as.character(uniq_block)) %>%
                group_by(uniq_block, adu_gender_m) %>%
                summarise(ads.utt.N = n()) %>%
                right_join(block.agd.combos, by =
                             c("uniq_block", "adu_gender_m")) %>%
                replace_na(list(ads.utt.N = 0)) %>%
                mutate(uniq_block = as.factor(uniq_block)) %>%
                right_join(blockdata2, by = "uniq_block")

no.cds.blks <-  modeldata.excl %>%
                filter(label_m == "CDS") %>%
                mutate(uniq_block = as.character(uniq_block)) %>%
                group_by(uniq_block, adu_gender_m) %>%
                summarise(cds.utt.N = n()) %>%
                right_join(block.agd.combos, by =
                             c("uniq_block", "adu_gender_m")) %>%
                replace_na(list(cds.utt.N = 0)) %>%
                mutate(uniq_block = as.factor(uniq_block)) %>%
                right_join(blockdata2, by = "uniq_block")


# Any speaker
no.ads.blks.all <-  modeldata.excl %>%
                filter(label_m == "ADS") %>%
                mutate(uniq_block = as.character(uniq_block)) %>%
                group_by(uniq_block) %>%
                summarise(ads.utt.N = n()) %>%
                right_join(uniq.blks, by = "uniq_block") %>%
                replace_na(list(ads.utt.N = 0)) %>%
                mutate(uniq_block = as.factor(uniq_block)) %>%
                right_join(blockdata2, by = "uniq_block")

no.cds.blks.all <-  modeldata.excl %>%
                filter(label_m == "CDS") %>%
                mutate(uniq_block = as.character(uniq_block)) %>%
                group_by(uniq_block) %>%
                summarise(cds.utt.N = n()) %>%
                right_join(uniq.blks, by = "uniq_block") %>%
                replace_na(list(cds.utt.N = 0)) %>%
                mutate(uniq_block = as.factor(uniq_block)) %>%
                right_join(blockdata2, by = "uniq_block")

# Summary tables for any speaker
no.ads.ID <-  no.ads.blks.all %>%
              mutate(noads = ifelse(ads.utt.N == 0,1,0)) %>%
              group_by(ID, Corpus) %>%
              summarise(no.ads.blk = sum(noads))

no.cds.ID <-  no.cds.blks.all %>%
              mutate(nocds = ifelse(cds.utt.N == 0,1,0)) %>%
              group_by(ID) %>%
              summarise(no.cds.blk = sum(nocds))

# Summary tables for blocks with speech, pre and post block exclusion
pre.excl.xds <- modeldata %>%
                group_by(Corpus, ID, uniq_block) %>%
                summarise(utt.N = n()) %>%
                group_by(ID) %>%
                summarise(pre.blk.N = n())

post.excl.xds <- modeldata.excl %>%
                group_by(Corpus, ID, uniq_block) %>%
                summarise(utt.N = n()) %>%
                group_by(ID) %>%
                summarise(post.blk.N = n())

# Overarching summary of no-speech blocks
no.xds.ID <-  no.ads.ID %>%
              right_join(no.cds.ID, by = "ID") %>%
              right_join(pre.excl.xds, by = "ID") %>%
              right_join(post.excl.xds, by = "ID") %>%
              arrange(pre.blk.N, post.blk.N, -no.cds.blk, no.ads.blk)

