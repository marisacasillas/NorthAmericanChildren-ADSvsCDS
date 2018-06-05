allrows_allblocks_tripleinstances <- read_csv("input/IDS-ADS-DATATOANALYZE_all_blocks_pass1__11-22-16.csv")
summary(allrows_allblocks_tripleinstances)

#this initial file has everything in triplicate, once for each coder
#so if we only retain instance 0, we'll get just one copy

allrows_allblocks <- allrows_allblocks_tripleinstances %>% 
  filter(instance==0) %>% 
  dplyr::select(clan_file, block, timestamp, clip, tier) %>% 
  separate(timestamp,remove = F, sep ="_",into = c("onset","offset")) %>% 
  mutate(manfan = ifelse(tier=="MAN" | tier=="FAN", T, F),
         length_clip = as.numeric(offset)-as.numeric(onset)) %>% 
  group_by(tier)

allrows_allblocks %>% 
  tally()

allrows_allblocks %>% 
  group_by(manfan) %>% 
  tally() %>% 
  spread(manfan,n) %>% 
  rename(nonMF = `FALSE`,
         MF = `TRUE`) %>% 
  mutate(propMF = MF/(MF+nonMF),
         totalrows_allspkrs= MF+nonMF)

  
12684/(12684+30884)# 30% of clips

allrows_allblocks %>% 
  group_by(manfan) %>% 
  summarise(lengthtotal = sum(length_clip)) %>% 
  spread(manfan,lengthtotal) %>% 
  rename(nonMF = `FALSE`,
         MF = `TRUE`) %>% 
  mutate(propMF = MF/(MF+nonMF),
         totaltime_hrs_allspkrs = (MF+nonMF)/1000/60/60)

19522676/(19522676+30934254) #40% of the time
19522676/60/60/1000# 5 hours
(19522676+30934254)/60/60/1000# total is only 14 hours NOT 42 (42 came from 14*3)

allrows_allblocks %>% 
  filter(clan_file %in% c("B-01_06_pass1", "B-02_08_pass1") &
           block!=703) %>% #outlier block for 01
  group_by(clan_file,manfan) %>% 
  summarise(lengthtotal = sum(length_clip)) %>% 
  spread(manfan,lengthtotal) %>% 
  rename(nonMF = `FALSE`,
         MF = `TRUE`) %>% 
  mutate(propMF = MF/(MF+nonMF),
         totaltime_hrs_allspkrs = (MF+nonMF)/1000/60/60)

allrows_allblocks %>% 
  filter(clan_file %in% c("B-01_06_pass1", "B-02_08_pass1") &
           block!=703) %>% #outlier block for 01
  group_by(clan_file) %>% 
  summarise(lengthtotal = sum(length_clip)) %>% 
  mutate(totaltime_hrs_allspkrs = (lengthtotal)/1000/60/60)

#how about how long the recordings were to start with
xds_sample_reclength <- readxl::read_xlsx("input/Xds_sample_reclength.xlsx")
xds_sample_reclength %>% 
  summarise(total_reclength_hrs = sum(reclength)/60) #808.055 hrs
808.055/61

#assuming you've read in _XDS-DARCLE.R stuff
#let's just get 2 Ss
blockdata %>% filter(ID %in% c("B-01", "B-02")) %>% View()
blockdata %>% 
  filter(ID %in% c("B-01", "B-02") & toolong==0) %>% 
  group_by(ID) %>% 
  summarise(sum(blk_dur))
modeldata.excl %>% filter(ID %in% c("B-01", "B-02")) %>% View()
vetteddata %>% filter(ID %in% c("B-01", "B-02")) %>% View()

blkdurs %>% filter(ID %in% c("B-01", "B-02")) %>% group_by(ID) %>% summarise(sum(blk_dur))

adsratedata %>% filter(ID %in% c("B-01", "B-02"))
# A tibble: 2 x 28
#ID  ads.min      ID.h  ID.N aclew_id   Row    Corpus  ChiID chi_gender adu_univ Multiling AgeMonths mat_ed fat_ed number_older_sibs mother_dob father_dob household_size premature mat_ed_num fat_ed_num mat_ed_num3 fat_ed_num3    agem.c age.bin age.qrt ads.minph ads.minph.lg
#<fctr>    <dbl>     <dbl> <int>   <fctr> <int>    <fctr> <fctr>     <fctr>   <fctr>    <fctr>     <int> <fctr> <fctr>             <dbl>      <dbl>      <dbl>          <int>    <fctr>      <dbl>      <dbl>       <dbl>       <dbl>     <dbl>   <dbl>   <int>     <dbl>        <dbl>
#  1   B-01 3.286333 0.9966667    19     9803     1 Bergelson      1     female      yes        no         6     AD     AD                 1         32         35              4         N          5          5           3           3 -4.836066       0       2  3.297324     1.457993
#  2   B-02 3.218667 1.1160000    20     0673     3 Bergelson      2       male      yes        no         8     AD     AD                 1         32         34              4         N          5          5           3           3 -2.836066       0       2  2.884110     1.356894
cdsratedata %>% filter(ID %in% c("B-01", "B-02"))
# A tibble: 2 x 28
#ID  cds.min      ID.h  ID.N aclew_id   Row    Corpus  ChiID chi_gender adu_univ Multiling AgeMonths mat_ed fat_ed number_older_sibs mother_dob father_dob household_size premature mat_ed_num fat_ed_num mat_ed_num3 fat_ed_num3    agem.c age.bin age.qrt cds.minph cds.minph.lg
#<fctr>    <dbl>     <dbl> <int>   <fctr> <int>    <fctr> <fctr>     <fctr>   <fctr>    <fctr>     <int> <fctr> <fctr>             <dbl>      <dbl>      <dbl>          <int>    <fctr>      <dbl>      <dbl>       <dbl>       <dbl>     <dbl>   <dbl>   <int>     <dbl>        <dbl>
#  1   B-01 5.142500 0.9966667    19     9803     1 Bergelson      1     female      yes        no         6     AD     AD                 1         32         35              4         N          5          5           3           3 -4.836066       0       2  5.159699     1.818028
#  2   B-02 4.998333 1.1160000    20     0673     3 Bergelson      2       male      yes        no         8     AD     AD                 1         32         34              4         N          5          5           3           3 -2.836066       0       2  4.478793     1.700885

adsratedata %>% filter(ID %in% c("B-01", "B-02")) 
cdsratedata %>% filter(ID %in% c("B-01", "B-02")) 