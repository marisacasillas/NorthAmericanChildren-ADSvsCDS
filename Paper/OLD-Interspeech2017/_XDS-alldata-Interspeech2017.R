rm(list=ls())
source("_helper-XDS-alldata-Interspeech2017.R")

plot.path <- "plots/"

# Read in and set up label data
xdsall <- read_csv("IDS-ADS-DATATOANALYZE_all_blocks_pass1__11-22-16.csv", 
				col_types = cols(
					dont_share = col_factor(levels = c("False", "True")), 
					gender = col_factor(levels = c("FEMALE","MALE")), 
					label = col_factor(levels = c("CDS", "ADS","JUNK")),
					reliability = col_factor(levels = c("False", "True")),
					training = col_factor(levels = c("False", "True"))))
 
xdsall.t <- xdsall %>%
			rename(lena_id = tier,
				adu_gender  = gender) %>%
			mutate(ID = as.factor(
						str_match(clan_file, "[A-Z]-[A-Z0-9]+")),
					coder = as.factor(coder),
					lab_name = as.factor(lab_name)) %>%
			# Remove unhelpful tiers
			dplyr::select(-training, -reliability,
				-audiofile, -dont_share)

# Read in and set up participant data
tchidata <- read_csv("proposed_sample_summary-22Jun16 with substitutes-FINALbeforeIDSlabelannotation-restricted20171118.csv",
				col_types = cols(
					Gender = col_factor(levels = c("female","male")), 
					Corpus = col_factor(levels = c(
						"Bergelson", "McDivitt",
						"VanDam2", "Warlaumont")),
					`CaregiverBA/BS` = col_factor(levels = c("no", "yes"))))

tchidata.t <-	tchidata %>%
				rename(chi_gender = Gender,
					ID = `Sample ID`,
					ChiID = `Child ID`,
					adu_univ = `CaregiverBA/BS`) %>%
				mutate(ID = as.factor(ID),
					ChiID = as.factor(ChiID))

# Print a figure of the sample
pass1sample <- ggplot(tchidata.t,
		aes(x=AgeMonths, fill=Corpus)) +
		geom_histogram(binwidth=1,
			colour="black") +
		ylab("# individuals") + xlab("Age (months)") +
		scale_fill_manual(
			labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
			values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
		basic.theme + theme(
			panel.grid.major = element_blank(),
			panel.grid.minor = element_blank(),
	 		legend.position = "right")
png(paste(plot.path, "pass1sample.png", sep=""),
    width=1000, height=300,units="px", bg = "transparent")
print(pass1sample)
dev.off()

# Adding Greyscale Version Fig 1 ------------------------------------------
#EB: reploted for grayscale
greypass1sample <- ggplot(tchidata.t,
                      aes(x=AgeMonths, fill=Corpus)) +
  geom_histogram(binwidth=1,
                 colour="black") +
  ylab("# individuals") + xlab("Age (months)") +
  scale_fill_manual(
    labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
    values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
  basic.theme + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right")+
  scale_fill_grey(start = 1, end = 0)

ggsave(plot = greypass1sample,
  filename = "greypass1sample.png",
  #"png",
  path = plot.path,
  width = 35.28,
  height = 10.58,
  units = "cm",dpi = 72,
  bg = "transparent"
)


# Merge the child data into the IDS/ADS coding
xdsall.t <- xdsall.t %>%
			left_join(tchidata.t, by = "ID") %>%
			unite(uttID, ID, block, timestamp,
				sep = '.', remove = FALSE) %>%
			as.data.table()

# Add new "adu_gender" and "label" columns
# based on agreement between coders
uniq.uttIDs <-	xdsall.t %>%
				dplyr::select(-c(date, coder, lab_name)) %>%
				dplyr::select(uttID) %>%
				distinct() %>%
				as.data.table()

# Add new summary cols
uniq.uttIDs[, adu_gender_m := ""]
uniq.uttIDs[, label_m := ""]

# Create a new summary with one column per utterance
# I'm sure there's a way to make this faster...
summ.lab.gend <-		xdsall.t %>%
					dplyr::select(-c(date, coder, lab_name)) %>%
					filter(lena_id == "MAN"|lena_id == "FAN") %>%
					group_by(uttID) %>%
					summarise(label_m = majority(label),
						adu_gender_m = majority(adu_gender))

xdsall.t2 <-		xdsall.t %>%
				dplyr::select(-c(date, coder, lab_name)) %>%
				filter(lena_id == "MAN"|lena_id == "FAN") %>%
				filter(instance == 0) %>%
				dplyr::select(-c(instance)) %>%
				right_join(summ.lab.gend, by = "uttID") %>% 
				as.data.table()

# Add a unique block ID
xdsall.t2[, uniq_block := paste(ID, block, sep='.')]

# Note: Now that we're dealing with summaries over the annotators'
# codes, we'll be using the suffix "_m" to indicate a majority value

##### Tier 1 questions #####

#----------------------------------------------------------------------
#-- Among all LENA-labeled MAN/FAN utterances,
#-- how often were they JUNK, ADS, and CDS by 2+ coders?
N.LENA.MANFAN <-	xdsall.t2 %>%
					summarise(total = n())

all.XDSlabels <-	xdsall.t2 %>%
					group_by(label_m) %>%
					summarise(N = n(),
						mean = (n() / as.numeric(N.LENA.MANFAN)))

# Summary table:
# label_m     N        mean
#   <chr> <int>       <dbl>
#     ADS  4254 0.335383160
#     CDS  6632 0.522863450
#    JUNK  1730 0.136392305
#     NMJ    68 0.005361085#

#-- Discussion --------------------------------------------------------
#   MC-comment:	Pretty high error rate from LENA: ~14% false positives
#				(i.e., junk detected as "utterances").
#
#				CDS is more frequent than ADS; about 1.5 to 1.
#
#				Fewer than 1% "no-majority" cases for XDS judgments on
#				LENA-identified MAN/FAN segments---great!
#----------------------------------------------------------------------


#----------------------------------------------------------------------
#-- ... and among non-JUNK MAN and FAN:
#-- how often was LENA correct about the gender?
LENA.MANFAN.gend <-	xdsall.t2 %>%
					group_by(lena_id) %>%
					summarise(total.lenaid = n())

all.adu.gender.labels <-		xdsall.t2 %>%
							group_by(lena_id, adu_gender_m) %>%
							summarise(N = n()) %>%
							inner_join(LENA.MANFAN.gend, by = "lena_id") %>%
							mutate(mean = N / total.lenaid) %>%
							arrange(lena_id, adu_gender_m)

# Summary table:
# lena_id adu_gender_m     N total.lenaid        mean
#   <chr>        <chr> <int>        <int>       <dbl>
#     FAN       FEMALE  7612         9292 0.819199311
#     FAN         MALE   704         9292 0.075764098
#     FAN           NA   946         9292 0.101808007
#     FAN          NMJ    30         9292 0.003228584
#     MAN       FEMALE   889         3392 0.262087264
#     MAN         MALE  2271         3392 0.669516509
#     MAN           NA   218         3392 0.064268868
#     MAN          NMJ    14         3392 0.004127358

#-- how often was LENA correct about the gender?
all.adu.gender.labels.cds <-		xdsall.t2 %>%
							group_by(lena_id, adu_gender_m, label_m) %>%
							summarise(N = n()) %>%
							inner_join(LENA.MANFAN.gend, by = "lena_id") %>%
							mutate(mean = N / total.lenaid) %>%
							arrange(adu_gender_m, label_m, lena_id)


# How about if we just consider cases where LENA detected something that
# wasn't junk and had a majority human annotation for gender? 
N.notjunk.majcode <-		xdsall.t2 %>%
						filter(label_m != "JUNK") %>%
						filter(adu_gender_m == "FEMALE"|
							adu_gender_m == "MALE") %>%
						group_by(lena_id) %>%
						summarise(total.lenaid = n())

notjunk.majcode.gen.lab <-	xdsall.t2 %>%
							filter(label_m != "JUNK") %>%
							filter(adu_gender_m == "FEMALE"|
								adu_gender_m == "MALE") %>%
							group_by(lena_id, adu_gender_m) %>%
							summarise(N = n()) %>%
							inner_join(N.notjunk.majcode, by="lena_id") %>%
							mutate(mean = N / total.lenaid) %>%
							arrange(lena_id, adu_gender_m)

# Summary table:
# lena_id adu_gender_m     N total.lenaid       mean
#   <chr>        <chr> <int>        <int>      <dbl>
#     FAN       FEMALE  7282         7880 0.92411168
#     FAN         MALE   598         7880 0.07588832
#     MAN       FEMALE   851         3043 0.27965823
#     MAN         MALE  2192         3043 0.72034177

# Speech ID'd by humans as female is much more prevalent.
# How much more?
N.adu_gender <-	xdsall.t2 %>%
				filter(adu_gender_m == "FEMALE"|
					adu_gender_m == "MALE") %>%
				group_by(adu_gender_m) %>%
				summarise(total.adugend = n())
# adu_gender_m total.adugend
#         MALE          2813
#       FEMALE          8186
#
#-- Discussion --------------------------------------------------------
#   MC-comment:	When LENA says something is "FAN", the utterance turns
#				out to be majority coded as female speech about 82% of
#				of the time. When LENA says something is a "MAN", that
#				bears out in majority coding about 67% of the time.
#				LENA's "MAN" codes turn out to be females about 26% of
#				time time while their "FAN" codes turn out to be males
#				only 8% of the time---the rest being non-majority
#				codes (ambiguous) or junk utterances (non-speech).
#
#				If we focus on "unambiguous" cases from a human
#				perspective (utterances judged by a 2+ annotators as
#				coming from a male or from a female), "FAN" turns out
#				to be female speech 92% of the time, but "MAN" still
#				only turns out to be male speech 72% of the time. We
#				might then conclude that LENA's misidentifications
#				are fairly systematic, even in these "easier" cases.
#
#				Likely related to this asymmetry in identification,
#				female speech is much more common than male speech:
#				almost 3 to 1!
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# Check for age and (adu/chi) gender differences
# First for overall differences in CDS / CDS + ADS
# Forgetting LENA's errors for now:
# We'll only count non-junk codes with a majority agreement on both
# speaker gender and XDS
N.nonjunk.adu_gender <-	xdsall.t2 %>%
						filter(label_m == "CDS" |label_m == "ADS") %>%
						filter(adu_gender_m != "NMJ") %>%
						group_by(adu_gender_m) %>%
						summarise(total.njadugend = n())

adu.gender.effect <-	xdsall.t2 %>%
						filter(label_m == "CDS" |label_m == "ADS") %>%
						filter(adu_gender_m != "NMJ") %>%
						group_by(adu_gender_m, label_m) %>%
						summarise(N = n()) %>%
						inner_join(N.nonjunk.adu_gender,
							by = "adu_gender_m") %>%
						mutate(mean = N / total.njadugend) %>%
						arrange(adu_gender_m, label_m) %>%
						# We're just interested proport. CDS right now
						filter(label_m == "CDS")

# Summary table:
# adu_gender_m label_m     N total.njadugend      mean
#        <chr>   <chr> <int>           <int>     <dbl>
#       FEMALE     CDS  4954            8085 0.6127396
#         MALE     CDS  1670            2776 0.6015850

# Look at 2-wy interaction of:
# child gender and adult gender
gender.gender.totals <-	xdsall.t2 %>%
							filter(label_m == "CDS" |label_m == "ADS") %>%
							filter(adu_gender_m != "NMJ") %>%
							group_by(chi_gender, adu_gender_m) %>%
							summarise(gg.total = n()) %>%
							arrange(chi_gender, adu_gender_m)

gg.interactions <-	xdsall.t2 %>%
					filter(label_m == "CDS" |label_m == "ADS") %>%
					filter(adu_gender_m != "NMJ") %>%
					group_by(chi_gender, adu_gender_m, label_m) %>%
					summarise(N = n()) %>%
					arrange(chi_gender, adu_gender_m, label_m) %>%
					left_join(gender.gender.totals, by = c(
						"chi_gender", "adu_gender_m")) %>%
					mutate(mean = N / gg.total) %>%
					filter(label_m == "CDS")

# Summary table:
# chi_gender adu_gender_m label_m     N gg.total      mean
#     <fctr>        <chr>   <chr> <int>    <int>     <dbl>
#     female       FEMALE     CDS  2114     3281 0.6443158
#     female         MALE     CDS   635     1120 0.5669643
#       male       FEMALE     CDS  2840     4804 0.5911740
#       male         MALE     CDS  1035     1656 0.6250000

# Now look at 3-wy interaction of:
# child age, child gender, and adult gender
age.gender.gender.totals <-	xdsall.t2 %>%
							filter(label_m == "CDS" |label_m == "ADS") %>%
							filter(adu_gender_m != "NMJ") %>%
							group_by(AgeMonths, chi_gender,
								adu_gender_m) %>%
							summarise(agg.total = n()) %>%
							arrange(AgeMonths, chi_gender, adu_gender_m)

agg.interactions <-	xdsall.t2 %>%
					filter(label_m == "CDS" |label_m == "ADS") %>%
					filter(adu_gender_m != "NMJ") %>%
					group_by(AgeMonths, chi_gender,
						adu_gender_m, label_m) %>%
					summarise(N = n()) %>%
					arrange(AgeMonths, chi_gender,
						adu_gender_m, label_m) %>%
					left_join(age.gender.gender.totals, by = c(
						"AgeMonths", "chi_gender",
						"adu_gender_m")) %>%
					mutate(mean = N / agg.total) %>%
					filter(label_m == "CDS") %>%
					as.data.table()
					
# Note: one row is dropped because there are 0 instances of CDS from
# female speakers to female children at age 15 months (there are only 5
# instances of woman-to-girl speech at that age and they are all ADS)
# Later: fix the summary so it doesn't drop empty categories!!
# Elika: there's a way to force it not to drop them, i'll look in my old code later

# Summary table too big to show here, so plotted instead:
# see chiage-chigend-adugend.png
agg.interactions[, chig2 := factor(chi_gender, labels=c("Girl", "Boy"))]
agg.interactions[, adug2 := factor(adu_gender_m, labels=c("Woman", "Man"))]
age.gender.effects <- ggplot(agg.interactions,
	aes(x=AgeMonths, y=mean)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(adug2), fill = factor(adug2)), show.legend=F) +
	geom_point(aes(color=factor(adug2), size=N)) +
	scale_color_manual(
		values=c("maroon", "mediumblue")) +
	facet_grid(. ~ chig2) +
	ylim(0,1) + xlim(0,20) +
	scale_size(range = c(3, 13)) +
	labs(x = "Age (months)", y = "Proportion CDS\n",
		color = "Adult speaker", size="N tokens") +
	guides(colour = guide_legend(order =1, override.aes = list(size=8)), 
		size = guide_legend(order = 2),
		fill = "none") +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
 		legend.position = "right")
png(paste(plot.path, "chiage-chigend-adugend.png", sep=""),
    width=1400, height=400,units="px", bg = "transparent")
print(age.gender.effects)
dev.off()

# Version with no legend (for photoshop)
age.gender.effects.noleg <- ggplot(agg.interactions,
	aes(x=AgeMonths, y=mean)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(adug2), fill = factor(adug2)), show.legend=F) +
	geom_point(aes(color=factor(adug2), size=N)) +
	scale_color_manual(
		values=c("maroon", "mediumblue")) +
	facet_grid(. ~ chig2) +
	ylim(0,1) + xlim(0,20) +
	scale_size(range = c(3, 13)) +
	labs(x = "Age (months)", y = "Proportion CDS\n",
		color = "Adult speaker", size="N tokens") +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
 		legend.position = "none")
png(paste(plot.path, "chiage-chigend-adugend-no-legend.png", sep=""),
    width=1400, height=400,units="px", bg = "transparent")
print(age.gender.effects.noleg)
dev.off()


# adding greyscale version of figure 2 ------------------------------------
#EB
grey_age.gender.effects.leg <-
  ggplot(agg.interactions,
         aes(x=AgeMonths, y=mean)) +
  geom_smooth(method="lm", se=T,
              aes(color=factor(adug2), fill = factor(adug2),
                  linetype = factor(adug2)), show.legend=F) +
  geom_point(aes(color=factor(adug2), size=N,
                 shape=factor(adug2))) +
  scale_color_manual(
    #values=c("maroon", "mediumblue")) +
   values=c("grey20", "grey70")) +
  scale_fill_manual(values=c("grey20", "grey70")) +
  guides(colour = guide_legend(order =1, override.aes = list(size=8)), 
         size = guide_legend(order = 2),
         shape = guide_legend(order = 1),
         fill = "none") +
  
  facet_grid(. ~ chig2) +
  ylim(0,1) + xlim(0,20) +
  scale_size(range = c(3, 13)) +
  labs(x = "Age (months)", y = "Proportion CDS\n",
       color = "Adult speaker", size="N tokens", fill = "Adult speaker", shape = "Adult speaker") +
  basic.theme + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right")

grey_age.gender.effects.no.leg <-
  ggplot(agg.interactions,
         aes(x=AgeMonths, y=mean)) +
  geom_smooth(method="lm", se=T,
              aes(color=factor(adug2), fill = factor(adug2),
                  linetype = factor(adug2)), show.legend=F) +
  geom_point(aes(color=factor(adug2), size=N,
                 shape=factor(adug2))) +
  scale_color_manual(
    #values=c("maroon", "mediumblue")) +
    values=c("grey20", "grey70")) +
  scale_fill_manual(values=c("grey20", "grey70")) +
 # guides(colour = guide_legend(order =1, override.aes = list(size=8)), 
#         size = guide_legend(order = 2),
#         shape = guide_legend(order = 1),
#         fill = "none") +
  
  facet_grid(. ~ chig2) +
  ylim(0,1) + xlim(0,20) +
  scale_size(range = c(3, 13)) +
  labs(x = "Age (months)", y = "Proportion CDS\n",
       color = "Adult speaker", size="N tokens", fill = "Adult speaker", shape = "Adult speaker") +
  basic.theme + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none")

ggsave(plot = grey_age.gender.effects.leg,
       filename = "greychiage-chigend-adugend.png",
       #"png",
       path = plot.path,
       width = 49.39,
       height = 14.11,
       units = "cm",dpi = 72,
       bg = "transparent"
)
ggsave(plot = grey_age.gender.effects.no.leg,
       filename = "greychiage-chigend-adugend-no-legend.png",
       #"png",
       path = plot.path,
       width = 49.39,
       height = 14.11,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# To help interpret weird gender effect,
# look at it without the child gender facets
age.adugender.totals <-	xdsall.t2 %>%
							filter(label_m == "CDS" |label_m == "ADS") %>%
							filter(adu_gender_m != "NMJ") %>%
							group_by(AgeMonths, adu_gender_m) %>%
							summarise(aag.total = n()) %>%
							arrange(AgeMonths, adu_gender_m)

aag.interactions <-	xdsall.t2 %>%
					filter(label_m == "CDS" |label_m == "ADS") %>%
					filter(adu_gender_m != "NMJ") %>%
					group_by(AgeMonths, adu_gender_m, label_m) %>%
					summarise(N = n()) %>%
					arrange(AgeMonths, adu_gender_m, label_m) %>%
					left_join(age.adugender.totals, by = c(
						"AgeMonths", "adu_gender_m")) %>%
					mutate(mean = N / aag.total) %>%
					filter(label_m == "CDS") %>%
					arrange(adu_gender_m, AgeMonths, label_m) %>%
					as.data.table()
					
# Summary table too big to show here, so plotted instead:
# see chiage-adugend.png
aag.interactions[, adug2 := factor(adu_gender_m, labels=c("Woman", "Man"))]
age.agender.effects <- ggplot(aag.interactions,
	aes(x=AgeMonths, y=mean)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(adug2), fill = factor(adug2)), show.legend=F) +
	geom_point(aes(color=factor(adug2), size=N)) +
	scale_color_manual(
		values=c("maroon", "mediumblue")) +
	ylim(0,1) + xlim(0,20) +
	scale_size(range = c(3, 13)) +
	labs(x = "Age (months)", y = "Proportion CDS\n",
		color = "Adult speaker", size="N tokens") +
	guides(colour = guide_legend(order =1, override.aes = list(size=8)), 
		size = guide_legend(order = 2),
		fill = "none") +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
 		legend.position = "right")
png(paste(plot.path, "chiage-adugend.png", sep=""),
    width=1000, height=400,units="px", bg = "transparent")
print(age.agender.effects)
dev.off()



# Model:
humanOK.subset <-	xdsall.t2 %>%
					filter(label_m == "CDS" |label_m == "ADS") %>%
					filter(adu_gender_m != "NMJ") %>%
					mutate(CDS_yes = as.integer(
						ifelse(label_m == "CDS", 1, 0)),
						adu_gender_m = as.factor(adu_gender_m),
						AgeMonthsC = round(AgeMonths-mean(AgeMonths), 2)) %>%
					as.data.table()

# Model:
# with different random effects structures...
# agg.model.chi <-	glmer(CDS_yes ~ AgeMonthsC * chi_gender * adu_gender_m +
						# (1|ID), data=humanOK.subset,
						# family="binomial", control = glmerControl(optimizer="bobyqa"))

agg.model.corpchi <-	glmer(CDS_yes ~ AgeMonthsC * chi_gender * adu_gender_m +
						(1|Corpus/ID), data=humanOK.subset,
						family="binomial", control = glmerControl(optimizer="bobyqa"))

# agg.model.corpchiblk <-	glmer(CDS_yes ~ AgeMonthsC * chi_gender * adu_gender_m +
						# (1|Corpus/ID/uniq_block), data=humanOK.subset,
						# family="binomial", control = glmerControl(optimizer="bobyqa"))

# binnedplot(fitted(agg.model.chi),resid(agg.model.chi))


# Output:
     # AIC      BIC   logLik deviance df.resid 
 # 12320.5  12393.4  -6150.3  12300.5    10851 

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -5.6521 -0.8157  0.3714  0.7418  2.2365 

# Random effects:
 # Groups    Name        Variance  Std.Dev. 
 # ID:Corpus (Intercept) 1.222e+00 1.106e+00
 # Corpus    (Intercept) 4.018e-16 2.004e-08
# Number of obs: 10861, groups:  ID:Corpus, 61; Corpus, 4

# Fixed effects:
                                            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                                 0.783709   0.232219   3.375 0.000738 ***
# AgeMonthsC                                  0.148868   0.049205   3.025 0.002483 ** 
# chi_gendermale                             -0.033276   0.312542  -0.106 0.915209    
# adu_gender_mMALE                           -0.316897   0.087768  -3.611 0.000305 ***
# AgeMonthsC:chi_gendermale                   0.055403   0.065689   0.843 0.399000    
# AgeMonthsC:adu_gender_mMALE                 0.006671   0.019006   0.351 0.725593    
# chi_gendermale:adu_gender_mMALE             0.082715   0.117041   0.707 0.479741    
# AgeMonthsC:chi_gendermale:adu_gender_mMALE -0.137666   0.025407  -5.418 6.01e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) AgMntC ch_gnd a__MAL AgMC:_ AMC:__ c_:__M
# AgeMonthsC  -0.381                                          
# chi_gendrml -0.741  0.284                                   
# ad_gnd_MALE -0.098 -0.001  0.073                            
# AgMnthsC:c_  0.287 -0.747 -0.328  0.001                     
# AgMC:__MALE  0.002 -0.107 -0.001 -0.025  0.080              
# ch_g:__MALE  0.073  0.001 -0.101 -0.750 -0.025  0.019       
# AMC:_:__MAL  0.000  0.081 -0.016  0.019 -0.123 -0.748  0.118

# What would we have seen of this if we used LENA's codes for adu_gender?
# Model:
# agg.model.lena.chi <-	glmer(CDS_yes ~ AgeMonthsC * chi_gender * lena_id +
						# (1|ID), data=humanOK.subset,
						# family="binomial", control = glmerControl(optimizer="bobyqa"))

agg.model.lena.corpchi <-	glmer(CDS_yes ~ AgeMonthsC * chi_gender * lena_id +
						(1|Corpus/ID), data=humanOK.subset,
						family="binomial", control = glmerControl(optimizer="bobyqa"))

# agg.model.lena.corpchiblk <-	glmer(CDS_yes ~ AgeMonthsC * chi_gender * lena_id +
						# (1|Corpus/ID/uniq_block), data=humanOK.subset,
						# family="binomial", control = glmerControl(optimizer="bobyqa"))



# Output:
     # AIC      BIC   logLik deviance df.resid 
 # 12151.5  12224.4  -6065.7  12131.5    10851 

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -6.5586 -0.7540  0.3709  0.7150  2.6069 

# Random effects:
 # Groups    Name        Variance  Std.Dev. 
 # ID:Corpus (Intercept) 1.315e+00 1.147e+00
 # Corpus    (Intercept) 2.338e-09 4.836e-05
# Number of obs: 10861, groups:  ID:Corpus, 61; Corpus, 4

# Fixed effects:
                                      # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           0.926101   0.239974   3.859 0.000114 ***
# AgeMonthsC                            0.150935   0.050979   2.961 0.003069 ** 
# chi_gendermale                        0.026618   0.322815   0.082 0.934284    
# lena_idMAN                           -0.750098   0.080340  -9.337  < 2e-16 ***
# AgeMonthsC:chi_gendermale             0.027729   0.068009   0.408 0.683477    
# AgeMonthsC:lena_idMAN                 0.003581   0.017850   0.201 0.840981    
# chi_gendermale:lena_idMAN            -0.146066   0.108452  -1.347 0.178036    
# AgeMonthsC:chi_gendermale:lena_idMAN -0.049847   0.024144  -2.065 0.038966 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) AgMntC ch_gnd ln_MAN AgMC:_ AMC:_M c_:_MA
# AgeMonthsC  -0.380                                          
# chi_gendrml -0.740  0.283                                   
# lena_idMAN  -0.100 -0.003  0.074                            
# AgMnthsC:c_  0.286 -0.748 -0.328  0.002                     
# AgMntC:_MAN -0.001 -0.111  0.000 -0.026  0.083              
# ch_gnd:_MAN  0.073  0.002 -0.102 -0.741 -0.025  0.019       
# AgMC:_:_MAN  0.001  0.082 -0.019  0.019 -0.122 -0.739  0.129

#-- Discussion --------------------------------------------------------
#   MC-comment:	Overall, women and men use nearly identical proportions
#				of CDS (~0.6), despite the fact that women's speech is
#				much more prevalent.
#
#				Men and women both show increases in proportion CDS with
#				age but women increase their CDS with age more for boys
#				than for girls, whereas men increase their CDS comparably
#				for both.
#
##############  OUT OF DATE!!! SEE THE ABOVE FOR THE NEWEST MODEL SUMM'S
#				Model findings:
#				a - CDS increases with age **
#				b - Men use significantly less CDS than women ***
#				c - Women's increase in CDS with age for boys is
#					significantly larger than it is for men's ***
#
#				If we use LENA's FAN & MAN labels instead of our human-
#				based gender determinations, we get the same qualitat.
#				results.
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# What is the effect of SES on proportion CDS?
age.education.totals <-	xdsall.t2 %>%
						filter(label_m == "CDS" |label_m == "ADS") %>%
						filter(adu_gender_m != "NMJ") %>%
						group_by(AgeMonths, adu_univ) %>%
						summarise(ae.total = n()) %>%
						arrange(AgeMonths, adu_univ)

ae.interactions <-	xdsall.t2 %>%
					filter(label_m == "CDS" |label_m == "ADS") %>%
					filter(adu_gender_m != "NMJ") %>%
					group_by(AgeMonths, adu_univ, label_m) %>%
					summarise(N = n()) %>%
					arrange(AgeMonths, adu_univ, label_m) %>%
					inner_join(age.education.totals, by = c(
						"AgeMonths", "adu_univ")) %>%
					mutate(mean = N / ae.total) %>%
					filter(label_m == "CDS")

# Summary table too big to show here, so plotted instead:
# see chiage-aduedu.png
age.edu.effects <- ggplot(ae.interactions,
	aes(x=AgeMonths, y=mean)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(adu_univ), fill = factor(adu_univ)), show.legend=F) +
	geom_point(aes(color=factor(adu_univ), size=N)) +
	scale_fill_manual(
		labels=c("No", "Yes"),
		values=c("gray80", "gray60")) +
	scale_color_manual(
		labels=c("No", "Yes"),
		values=c("gray50", "gray30")) +
	ylim(0,1) + xlim(0,20) +
	scale_size(range = c(5, 15)) +
	labs(x = "Age (months)", y = "Proportion CDS\n",
		color = "University\neducation", size="N tokens") +
	guides(colour = guide_legend(order =1, override.aes = list(size=10)), 
            size = guide_legend(order = 2),
			fill = F) +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank())
png(paste(plot.path, "chiage-aduedu.png", sep=""),
    width=1000, height=400,units="px", bg = "transparent")
print(age.edu.effects)
dev.off()

# Model:
# ae.model.fullrand <-	glmer(CDS_yes ~ AgeMonthsC * adu_univ +
						# (1|Corpus/ID/uniq_block), data=humanOK.subset,
						# family="binomial", control = glmerControl(optimizer="bobyqa"))

ae.model.corpchi <-	glmer(CDS_yes ~ AgeMonthsC * adu_univ +
						(1|Corpus/ID), data=humanOK.subset,
						family="binomial", control = glmerControl(optimizer="bobyqa"))

# Output:
     # AIC      BIC   logLik deviance df.resid 
 # 12385.9  12429.7  -6187.0  12373.9    10855 

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -7.1956 -0.8248  0.3615  0.7352  1.9204 

# Random effects:
 # Groups    Name        Variance  Std.Dev. 
 # ID:Corpus (Intercept) 1.305e+00 1.142e+00
 # Corpus    (Intercept) 2.094e-10 1.447e-05
# Number of obs: 10861, groups:  ID:Corpus, 61; Corpus, 4

# Fixed effects:
                       # Estimate Std. Error z value Pr(>|z|)   
# (Intercept)             0.50890    0.33958   1.499  0.13397   
# AgeMonthsC              0.20415    0.07052   2.895  0.00379 **
# adu_univyes             0.22133    0.38464   0.575  0.56500   
# AgeMonthsC:adu_univyes -0.06014    0.08029  -0.749  0.45387   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) AgMntC ad_nvy
# AgeMonthsC  -0.571              
# adu_univyes -0.879  0.503       
# AgMnthsC:d_  0.501 -0.877 -0.492

# Now how about specific corpora in which we expect some SES diffs?
BandM.corpus <-	xdsall.t2 %>%
				filter(Corpus == "Bergelson"|
					Corpus == "McDivitt")

bvsm.totals <-	BandM.corpus %>%
				filter(label_m == "CDS" |label_m == "ADS") %>%
				filter(adu_gender_m != "NMJ") %>%
				group_by(Corpus, AgeMonths) %>%
				summarise(a.total = n()) %>%
				arrange(Corpus, AgeMonths)

bvsm.interactions <-	BandM.corpus %>%
						filter(label_m == "CDS" |label_m == "ADS") %>%
						filter(adu_gender_m != "NMJ") %>%
						group_by(Corpus, AgeMonths, label_m) %>%
						summarise(N = n()) %>%
						arrange(Corpus, AgeMonths, label_m) %>%
						inner_join(bvsm.totals, by = c(
							"Corpus", "AgeMonths")) %>%
						mutate(mean = N / a.total) %>%
						filter(label_m == "CDS")

# Summary table too big to show here, so plotted instead:
# see chiage-BvsM.png
bvsm.effects <- ggplot(bvsm.interactions,
	aes(x=AgeMonths, y=mean)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(Corpus), fill = factor(Corpus)), show.legend=F) +
	geom_point(aes(color=factor(Corpus), size=N)) +
	scale_fill_manual(
		labels=c("Bergelson", "McDivitt"),
		values=c("gray60", "gray80")) +
	scale_color_manual(
		labels=c("Bergelson", "McDivitt"),
		values=c("gray30", "gray50")) +
	ylim(0,1) + xlim(0,20) +
	scale_size(range = c(5, 15)) +
	labs(x = "Age (months)", y = "Proportion CDS\n",
		color = "Corpus", size="N tokens") +
	guides(colour = guide_legend(order =1, override.aes = list(size=10)), 
            size = guide_legend(order = 2),
			fill = F) +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank())
png(paste(plot.path, "chiage-BvsM.png", sep=""),
    width=1000, height=400,units="px", bg = "transparent")
print(bvsm.effects)
dev.off()

# Model:
# humanOK.subset.bvsm <-	humanOK.subset %>%
						# filter(Corpus == "Bergelson"|
							# Corpus == "McDivitt")

# bvsm.model.fullrand <-	glmer(CDS_yes ~ AgeMonthsC * Corpus +
						# (1|Corpus/ID), data=humanOK.subset.bvsm,
						# family="binomial", control = glmerControl(optimizer="bobyqa"))

#-- Discussion --------------------------------------------------------
#   MC-comment:	No obvious differences in proportion CDS given binary
#				post-secondary education of caregiver (adu_univ) or
#				corpora with known SES differences
#
#				Model findings:
#				<increase in CDS with age; nothing else>
#
#   EB-comment:	yeah, nada! we should look for effects by-corpus
#				though... may be a confound where my moms are more 
#				educated than teenage moms in melanie's
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# What is the effect of SES on speech overall?
age.education.totals.indiv <-	xdsall.t2 %>%
						filter(label_m == "CDS" |label_m == "ADS") %>%
						filter(adu_gender_m != "NMJ") %>%
						group_by(AgeMonths, adu_univ, Corpus, ID) %>%
						summarise(sp.total = n()) %>%
						arrange(AgeMonths, adu_univ, Corpus, ID) %>%
						as.data.table()

age.education.totals.indiv[, adu_univ2 := factor(adu_univ,
	labels=c("No university", "University"))]

# Summary table too big to show here, so plotted instead:
# see indiv-raw-num-tokens.png
indivs.effects <- ggplot(age.education.totals.indiv,
	aes(x=AgeMonths, y=sp.total)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(Corpus), fill = factor(Corpus)), show.legend=F) +
	geom_point(aes(color=factor(Corpus)), size=8) +
	facet_grid(. ~ adu_univ2) +
	scale_fill_manual(
		labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
		values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
	scale_color_manual(
		labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
		values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
	ylim(0,500) + xlim(0,20) +
	labs(x = "Age (months)", y = "N tokens\n",
		color = "Corpus") +
	guides(colour = guide_legend(order =1, override.aes = list(size=10)), 
		fill = F) +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		legend.position = c(0.1, 0.7))
png(paste(plot.path, "indiv-raw-num-tokens-univ.png", sep=""),
    width=1400, height=400,units="px", bg = "transparent")
print(indivs.effects)
dev.off()

# And on CDS only?
age.education.totals.indiv.cds <-	xdsall.t2 %>%
						filter(label_m == "CDS") %>%
						filter(adu_gender_m != "NMJ") %>%
						group_by(AgeMonths, adu_univ, Corpus, ID) %>%
						summarise(sp.total = n()) %>%
						arrange(AgeMonths, adu_univ, Corpus, ID) %>%
						filter(ID != "B-30") %>% # outlier > 3*mean # CDS tokens
						as.data.table()

age.education.totals.indiv.cds[, adu_univ2 := factor(adu_univ,
	labels=c("No university", "University"))]

# Summary table too big to show here, so plotted instead:
# see indiv-raw-num-tokens-CDS.png
indivs.effects.cds <- ggplot(age.education.totals.indiv.cds,
	aes(x=AgeMonths, y=sp.total)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(Corpus), fill = factor(Corpus)), show.legend=F) +
	geom_point(aes(color=factor(Corpus)), size=8) +
	facet_grid(. ~ adu_univ2) +
	scale_fill_manual(
		labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
		values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
	scale_color_manual(
		labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
		values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
	ylim(0,500) + xlim(0,20) +
	labs(x = "Age (months)", y = "N tokens CDS\n",
		color = "Corpus") +
	guides(colour = guide_legend(order =1, override.aes = list(size=10)), 
		fill = F) +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		legend.position = c(0.1, 0.7))
png(paste(plot.path, "indiv-raw-num-tokens-CDS-univ.png", sep=""),
    width=1400, height=400,units="px", bg = "transparent")
print(indivs.effects.cds)
dev.off()

# Now one with a smaller y-axis
indivs.effects.cds.miniy <- ggplot(age.education.totals.indiv.cds,
	aes(x=AgeMonths, y=sp.total)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(Corpus), fill = factor(Corpus)), show.legend=F) +
	geom_point(aes(color=factor(Corpus)), size=8) +
	facet_grid(. ~ adu_univ2) +
	scale_fill_manual(
		labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
		values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
	scale_color_manual(
		labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
		values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
	ylim(0,300) + xlim(0,20) +
	labs(x = "Age (months)", y = "N tokens CDS\n",
		color = "Corpus") +
	guides(colour = guide_legend(order =1, override.aes = list(size=10)), 
		fill = F) +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		legend.position = c(0.1, 0.7))
png(paste(plot.path, "indiv-raw-num-tokens-CDS-univ-small-y.png", sep=""),
    width=1400, height=400,units="px", bg = "transparent")
print(indivs.effects.cds.miniy)
dev.off()

#  shape figure4 ------------------------------------------------------
#eb couldn't deal with greyscale, just changed shape
shapeindivs.effects.cds.miniy <- ggplot(age.education.totals.indiv.cds,
       aes(x=AgeMonths, y=sp.total)) +
  geom_smooth(method="lm", se=T,
              aes(color=factor(Corpus), fill = factor(Corpus), linetype= factor(Corpus)), show.legend=F) +
  geom_point(aes(color=factor(Corpus), shape = factor(Corpus)), size=8) +
  facet_grid(. ~ adu_univ2) +
  scale_fill_manual(
    labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
    values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
  scale_color_manual(
    labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
    values=c("firebrick1", "gold1", "forestgreen", "dodgerblue1")) +
  scale_shape_manual(labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
                       values=c(0,18,8,16))+
  ylim(0,300) + xlim(0,20) +
  labs(x = "Age (months)", y = "N tokens CDS\n",
       color = "Corpus", shape = "Corpus") +
  guides(colour = guide_legend(order = 1, override.aes = list(size=10)), 
         fill = "none",
         shape = guide_legend(order = 1)) +
  basic.theme + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.1, 0.7))

ggsave(plot = shapeindivs.effects.cds.miniy,
       filename = "shapeindiv-raw-num-tokens-CDS-univ-small-y.png",
       #"png",
       path = plot.path,
       width = 49.39,
       height = 14.11,
       units = "cm",dpi = 72,
       bg = "transparent"
)

# Model of all speech:
age.education.totals.indiv.2 <-	age.education.totals.indiv %>%
								mutate(AgeMonthsC = round(AgeMonths-mean(AgeMonths), 2)) %>%
								as.data.table()

sp.quant.by.corpora <-	lmer(sp.total ~ AgeMonthsC * adu_univ +
						(1|Corpus), data=age.education.totals.indiv.2)

# Should I somehow normalize the total speech tokens?
# Nah... the residuals look okay
#resid(sp.quant.by.corpora)
#plot(density(resid(sp.quant.by.corpora)))
#qqnorm(resid(sp.quant.by.corpora))
#qqline(resid(sp.quant.by.corpora))


# Output:
# REML criterion at convergence: 682.3

# Scaled residuals: 
     # Min       1Q   Median       3Q      Max 
# -2.00285 -0.71050 -0.03296  0.49545  3.13852 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # Corpus   (Intercept)    0      0.00   
 # Residual             6613     81.32   
# Number of obs: 61, groups:  Corpus, 4

# Fixed effects:
                       # Estimate Std. Error t value
# (Intercept)             104.349     20.011   5.214
# AgeMonthsC                2.024      4.792   0.422
# adu_univyes              94.368     23.580   4.002
# AgeMonthsC:adu_univyes  -20.444      5.490  -3.724

# Correlation of Fixed Effects:
            # (Intr) AgMntC ad_nvy
# AgeMonthsC  -0.287              
# adu_univyes -0.849  0.244       
# AgMnthsC:d_  0.251 -0.873 -0.185

# Model of CDS:
age.education.totals.indiv.cds.2 <-	age.education.totals.indiv.cds %>%
								mutate(AgeMonthsC = round(AgeMonths-mean(AgeMonths), 2),
									log.sp.total = log(sp.total)) %>%
								as.data.table()

cds.quant.by.corpora <- lmer(sp.total ~ AgeMonthsC * adu_univ +
					(1|Corpus), data=age.education.totals.indiv.cds.2)

cds.quant.by.corpora.log <- lmer(log.sp.total ~ AgeMonthsC * adu_univ +
					(1|Corpus), data=age.education.totals.indiv.cds.2)

# Should I somehow normalize the total CDS tokens?
# Residuals don't look as good as for all speech tokens
# resid(cds.quant.by.corpora)
# plot(density(resid(cds.quant.by.corpora)))
# qqnorm(resid(cds.quant.by.corpora))
# qqline(resid(cds.quant.by.corpora))
# ... but simple logging doesn't make things much better
# resid(cds.quant.by.corpora.log)
# plot(density(resid(cds.quant.by.corpora.log)))
# qqnorm(resid(cds.quant.by.corpora.log))
# qqline(resid(cds.quant.by.corpora.log))
# ... so I'll ignore this for now.

# Output:
# REML criterion at convergence: 621.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -1.5017 -0.6143 -0.1054  0.4550  2.6113 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # Corpus   (Intercept)    0      0.00   
 # Residual             2767     52.61   
# Number of obs: 60, groups:  Corpus, 4

# Fixed effects:
                       # Estimate Std. Error t value
# (Intercept)              67.831     12.991   5.222
# AgeMonthsC                4.135      3.100   1.334
# adu_univyes              46.544     15.347   3.033
# AgeMonthsC:adu_univyes  -11.348      3.558  -3.189

# Correlation of Fixed Effects:
            # (Intr) AgMntC ad_nvy
# AgeMonthsC  -0.298              
# adu_univyes -0.846  0.252       
# AgMnthsC:d_  0.260 -0.871 -0.190

#-- Discussion --------------------------------------------------------
#   MC-comment:	Large differences in raw amount of speech and raw
#				amount of CDS by education, with change across child
#				age, most strikingly for families with caregivesrs
#				who have a university degree (decreasing the # of
#				tokens overall)
#
#				Model findings (in both cases):
#				a - more tokens overall from university-edu families
#				b - big decrease with age in university-edu families
#
#				This makes for a somewhat complicated story given the
#				non-differences found across these groups for SES.
#				Though a major caveat is the age * SES sampling
#				across corpora.
#
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# What is the effect of age, chigender, and adugender on speech overall?
agg.totals.indiv <-	xdsall.t2 %>%
					filter(label_m == "CDS" |label_m == "ADS") %>%
					filter(adu_gender_m != "NMJ") %>%
					group_by(AgeMonths, adu_gender_m, chi_gender, ID, Corpus) %>%
					summarise(sp.total = n()) %>%
					arrange(AgeMonths, adu_gender_m, chi_gender, ID, Corpus) %>%
					as.data.table()

agg.totals.indiv[, chig2 := factor(chi_gender, labels=c("Girl", "Boy"))]
agg.totals.indiv[, adug2 := factor(adu_gender_m, labels=c("Woman", "Man"))]

# Summary table too big to show here, so plotted instead:
# see indiv-raw-num-tokens-CDS.png
indivs.effects.agg <- ggplot(agg.totals.indiv,
	aes(x=AgeMonths, y=sp.total)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(adug2), fill = factor(adug2)), show.legend=F) +
	geom_point(aes(color=factor(adug2)), size=8) +
	facet_grid(. ~ chig2) +
	scale_fill_manual(
		values=c("maroon", "mediumblue")) +
	scale_color_manual(
		values=c("maroon", "mediumblue")) +
	ylim(0,500) + xlim(0,20) +
	labs(x = "Age (months)", y = "N tokens\n",
		color = "Adult speaker") +
	guides(colour = guide_legend(order =1, override.aes = list(size=10)), 
		fill = F) +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		legend.position = c(0.1, 0.8))
png(paste(plot.path, "indiv-raw-num-tokens-agg.png", sep=""),
    width=1400, height=400,units="px", bg = "transparent")
print(indivs.effects.agg)
dev.off()

# Model of all speech:
agg.totals.indiv.2 <-	agg.totals.indiv %>%
						mutate(AgeMonthsC = round(AgeMonths-mean(AgeMonths), 2)) %>%
						as.data.table()

sp.quant.agg <-	lmer(sp.total ~ AgeMonthsC * chi_gender * adu_gender_m +
				(1|Corpus/ID), data=agg.totals.indiv.2)

# Should I somehow normalize the total speech tokens?
# Residuals are okayish
#resid(sp.quant.agg)
#plot(density(resid(sp.quant.agg)))
#qqnorm(resid(sp.quant.agg))
#qqline(resid(sp.quant.agg))


# Output:
# REML criterion at convergence: 1256.7

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -2.4133 -0.5779 -0.1919  0.5853  3.7289 

# Random effects:
 # Groups    Name        Variance Std.Dev.
 # ID:Corpus (Intercept)    0.00   0.00   
 # Corpus    (Intercept)   87.98   9.38   
 # Residual              5696.14  75.47   
# Number of obs: 114, groups:  ID:Corpus, 61; Corpus, 4

# Fixed effects:
                                           # Estimate Std. Error t value
# (Intercept)                                 114.925     15.455   7.436
# AgeMonthsC                                   -5.652      3.276  -1.726
# chi_gendermale                               33.233     19.429   1.710
# adu_gender_mMALE                            -66.882     21.115  -3.167
# AgeMonthsC:chi_gendermale                    -7.419      4.331  -1.713
# AgeMonthsC:adu_gender_mMALE                   2.811      5.007   0.561
# chi_gendermale:adu_gender_mMALE             -29.009     28.543  -1.016
# AgeMonthsC:chi_gendermale:adu_gender_mMALE    4.892      6.454   0.758

# Correlation of Fixed Effects:
            # (Intr) AgMntC ch_gnd a__MAL AgMC:_ AMC:__ c_:__M
# AgeMonthsC  -0.059                                          
# chi_gendrml -0.662  0.064                                   
# ad_gnd_MALE -0.605  0.060  0.484                            
# AgMnthsC:c_  0.067 -0.745 -0.047 -0.046                     
# AgMC:__MALE  0.056 -0.641 -0.042 -0.038  0.487              
# ch_g:__MALE  0.447 -0.045 -0.679 -0.740  0.030  0.028       
# AMC:_:__MAL -0.044  0.497  0.029  0.030 -0.667 -0.776 -0.005

# And on CDS?
agg.totals.indiv.cds <-	xdsall.t2 %>%
						filter(label_m == "CDS") %>%
						filter(adu_gender_m != "NMJ") %>%
						group_by(AgeMonths, adu_gender_m, chi_gender, ID, Corpus) %>%
						summarise(sp.total = n()) %>%
						arrange(AgeMonths, adu_gender_m, chi_gender, ID, Corpus) %>%
						filter(ID != "B-30") %>% # outlier > mean+3sd
						as.data.table()

agg.totals.indiv.cds[, chig2 := factor(chi_gender, labels=c("Girl", "Boy"))]
agg.totals.indiv.cds[, adug2 := factor(adu_gender_m, labels=c("Woman", "Man"))]

# Summary table too big to show here, so plotted instead:
# see indiv-raw-num-tokens-CDS.png
indivs.effects.agg.cds <- ggplot(agg.totals.indiv.cds,
	aes(x=AgeMonths, y=sp.total)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(adug2), fill = factor(adug2)), show.legend=F) +
	geom_point(aes(color=factor(adug2)), size=8) +
	facet_grid(. ~ chig2) +
	scale_fill_manual(
		values=c("maroon", "mediumblue")) +
	scale_color_manual(
		values=c("maroon", "mediumblue")) +
	ylim(0,500) + xlim(0,20) +
	labs(x = "Age (months)", y = "N tokens CDS\n",
		color = "Adult speaker") +
	guides(colour = guide_legend(order =1, override.aes = list(size=10)), 
		fill = F) +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		legend.position = c(0.1, 0.8))
png(paste(plot.path, "indiv-raw-num-tokens-CDS-agg.png", sep=""),
    width=1400, height=400,units="px", bg = "transparent")
print(indivs.effects.agg.cds)
dev.off()

# Now one with a smaller y-axis
indivs.effects.agg.cds.miniy <- ggplot(agg.totals.indiv.cds,
	aes(x=AgeMonths, y=sp.total)) +
	geom_smooth(method="lm", se=T,
		aes(color=factor(adug2), fill = factor(adug2)), show.legend=F) +
	geom_point(aes(color=factor(adug2)), size=8) +
	facet_grid(. ~ chig2) +
	scale_fill_manual(
		values=c("maroon", "mediumblue")) +
	scale_color_manual(
		values=c("maroon", "mediumblue")) +
	ylim(0,300) + xlim(0,20) +
	labs(x = "Age (months)", y = "N tokens CDS\n",
		color = "Adult speaker") +
	guides(colour = guide_legend(order =1, override.aes = list(size=10)), 
		fill = F) +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		legend.position = c(0.1, 0.8))
png(paste(plot.path, "indiv-raw-num-tokens-CDS-agg-small-y.png", sep=""),
    width=1400, height=400,units="px", bg = "transparent")
print(indivs.effects.agg.cds.miniy)
dev.off()


# Model of all speech:
agg.totals.indiv.cds.2 <-	agg.totals.indiv.cds %>%
						mutate(AgeMonthsC = round(AgeMonths-mean(AgeMonths), 2)) %>%
						as.data.table()

sp.quant.agg.cds <-	lmer(sp.total ~ AgeMonthsC * chi_gender * adu_gender_m +
				(1|Corpus/ID), data=agg.totals.indiv.cds.2)

# Should I somehow normalize the total speech tokens?
# Residuals are not great... should try to improve on future iterations
#resid(sp.quant.agg.cds)
#plot(density(resid(sp.quant.agg.cds)))
#qqnorm(resid(sp.quant.agg.cds))
#qqline(resid(sp.quant.agg.cds))


# Output:
# REML criterion at convergence: 1119.8

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -1.7068 -0.5689 -0.2378  0.3981  4.3812 

# Random effects:
 # Groups    Name        Variance Std.Dev.
 # ID:Corpus (Intercept)    0      0.00   
 # Corpus    (Intercept)    0      0.00   
 # Residual              2614     51.12   
# Number of obs: 109, groups:  ID:Corpus, 60; Corpus, 4

# Fixed effects:
                                           # Estimate Std. Error t value
# (Intercept)                                 75.6863     9.6838   7.816
# AgeMonthsC                                  -0.6361     2.2362  -0.284
# chi_gendermale                               4.5141    13.3469   0.338
# adu_gender_mMALE                           -46.6521    14.5914  -3.197
# AgeMonthsC:chi_gendermale                   -2.6378     2.9630  -0.890
# AgeMonthsC:adu_gender_mMALE                 -0.1457     3.4485  -0.042
# chi_gendermale:adu_gender_mMALE              1.9347    19.7765   0.098
# AgeMonthsC:chi_gendermale:adu_gender_mMALE  -0.6082     4.4339  -0.137

# Correlation of Fixed Effects:
            # (Intr) AgMntC ch_gnd a__MAL AgMC:_ AMC:__ c_:__M
# AgeMonthsC  -0.068                                          
# chi_gendrml -0.726  0.049                                   
# ad_gnd_MALE -0.664  0.045  0.482                            
# AgMnthsC:c_  0.051 -0.755 -0.026 -0.034                     
# AgMC:__MALE  0.044 -0.648 -0.032 -0.059  0.489              
# ch_g:__MALE  0.490 -0.033 -0.675 -0.738  0.018  0.044       
# AMC:_:__MAL -0.034  0.504  0.017  0.046 -0.668 -0.778 -0.013

### Other stuff below...








#----------------------------------------------------------------------
# Is CDS more likely vs. ADS when CHN is detected?
# Let's only use conversational blocks with 2-100 clips.
# Note: we don't have data here on the reliability of these
# CHN segments (i.e., we don't know how many are really JUNK)
xdsall.t2 <- as.data.table(xdsall.t2) #elika: i'll just keep doing this don't mind me
xdsall.t2[, CHNdetected := ifelse(lena_id == "CHN",1,0)]

clips.by.block <- xdsall.t2 %>%
				group_by(uniq_block) %>%
				summarise(num_blk_clips = n())

# Add total # clips per block into the main data table
xdsall.t2 <-		xdsall.t2 %>%
				left_join(clips.by.block, by = "uniq_block")

CHN.by.block <- xdsall.t2 %>%
				group_by(uniq_block) %>%
				summarise(ublockCHNs = sum(CHNdetected))

N.nonjunkXDSbyblock <-	xdsall.t2 %>%
						filter(label_m == "CDS" |label_m == "ADS") %>%
						filter(adu_gender_m != "NMJ") %>%
						filter(num_blk_clips > 1 &
							num_blk_clips < 101) %>%
						group_by(uniq_block) %>%
						summarise(xds.total = n())

XDSbyCHN <-	xdsall.t2 %>%
			filter(label_m == "CDS" |label_m == "ADS") %>%
			filter(adu_gender_m != "NMJ") %>%
			filter(num_blk_clips > 1 &
				num_blk_clips < 101) %>%
			group_by(uniq_block, label_m) %>%
			summarise(labelm.total = n()) %>%
			left_join(N.nonjunkXDSbyblock, by = "uniq_block") %>%
			mutate(mean.lab = labelm.total / xds.total) %>%
			filter(label_m == "CDS") %>%
			left_join(CHN.by.block, by = "uniq_block")

# Summary table too big to show here, so plotted instead:
# see CDS-by-CHN.png
CHN.effects <- ggplot(XDSbyCHN,
	aes(x=ublockCHNs, y=mean.lab)) +
	geom_smooth(method="lm", se=FALSE, color="red") +
	geom_point(aes(size=xds.total)) +
	ylim(0,1) + xlim(0,50) +
	scale_size(range = c(3, 10)) +
	labs(x = "# target child voc'ns in block", y = "Proportion CDS\n",
		size="Total utts\n(ADS + CDS)\nin block") +
	basic.theme + theme(
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank())
png(paste(plot.path, "CDS-by-CHN.png", sep=""),
    width=1000, height=400,units="px", bg = "transparent")
print(CHN.effects)
dev.off()
#
#-- Discussion --------------------------------------------------------
#   MC-comment:	It's hard to straightforwardly interpret this since:
#				1 - the CHN clips aren't verified
#				2 - CDS / (CDS+ADS) is hitting ceiling across the x
#				3 - there seems to be this ceiling stuff + some other
#					kind of block type in which the proport CDS varies
#					more. I'm not sure what underlies this variation.
#
#				So maybe this isn't so useful, at least summarized this
#				way. It seems like a binary version of "CHN or not"
#				won't do us much good either. What's another quick way
#				we could look at possible effects of increased CDS due
#				to contingent response on child vocalizations?
#   Elika: i guess we could look at block *length*, parsing the time column
#       on the underscore to be onset/offset in ms...but blocks are
#       already predefined by lena to have exchange within xx ms, right?
#       also not sure this one is helpful. pulling out anything where the
#       kid us under a year might clean this up some (my lexical bias)
#----------------------------------------------------------------------

##### Tier 2 questions (not checked) #####
# Was CDS more/less likely in blocks where MAN and FAN were detected,
# than either one alone?
# Was JUNK more/less likely in blocks were multiple instances of FAN/MAN
# were detected?
# Do the age/gender/age*gender effects seen above hold in each dataset?
# Were there overall differences between datasets?
# Were any of the datasets more "JUNK"-y than the others?
