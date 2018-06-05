# By child characteristics
# c.age.vol.all <-	vetteddata %>%
# 				        	group_by(AgeMonths, ID, Corpus, chi_gender, mat_ed_num3) %>%
# 				        	summarise(n = n()) %>%
#                   mutate(mat_ed_num3 = factor(mat_ed_num3))
# c.age.vol.ads <-	vetteddata %>%
#                   filter(label_m == "ADS") %>%
# 				        	group_by(AgeMonths, ID, Corpus, chi_gender, mat_ed_num3) %>%
# 				        	summarise(n = n()) %>%
#                   mutate(mat_ed_num3 = factor(mat_ed_num3))
# c.age.vol.cds <-	vetteddata %>%
#                   filter(label_m == "CDS") %>%
# 				        	group_by(AgeMonths, ID, Corpus, chi_gender, mat_ed_num3) %>%
# 				        	summarise(n = n()) %>%
#                   mutate(mat_ed_num3 = factor(mat_ed_num3))
# c.age.pro.cds <-	c.age.vol.cds %>%
#                   left_join(c.age.vol.all,
#                             by=c("AgeMonths", "ID", "Corpus",
#                                  "chi_gender", "mat_ed_num3")) %>%
#                   mutate(n = round((n.x / n.y)*100, 2),
#                          mat_ed_num3 = factor(mat_ed_num3))

# Considering adult gender too
# a.gen.vol.all <-	vetteddata %>%
# 			         		group_by(adu_gender_m, ID, Corpus) %>%
# 			        		summarise(n = n()) %>%
#                   left_join(IDS_demo_update, by = c("ID", "Corpus"))
# a.gen.vol.ads <-	vetteddata %>%
#                   filter(label_m == "ADS") %>%
# 			         		group_by(adu_gender_m, ID, Corpus) %>%
# 			        		summarise(n = n()) %>%
#                   left_join(IDS_demo_update, by = c("ID", "Corpus"))
# a.gen.vol.cds <-	vetteddata %>%
#                   filter(label_m == "CDS") %>%
# 			         		group_by(adu_gender_m, ID, Corpus) %>%
# 			        		summarise(n = n()) %>%
#                   left_join(IDS_demo_update, by = c("ID", "Corpus"))
# a.gen.pro.cds <-	a.gen.vol.cds %>%
#                   left_join(a.gen.vol.all,
#                             by=c("adu_gender_m", "ID", "Corpus")) %>%
#                   mutate(n = round((n.x / n.y)*100, 2)) %>%
#                   left_join(IDS_demo_update, by = c("ID", "Corpus"))

# Just to see how much ADS and CDS came from men and women
# a.gen.vol.sup <-	vetteddata %>%
# 			         		group_by(adu_gender_m, label_m, ID, Corpus) %>%
# 			        		summarise(n = n()) %>%
#                   left_join(IDS_demo_update, by = c("ID", "Corpus"))

# Print figs of the effect of age and its simple interactions
#source(paste0(scripts,"FIG-ageeffects-1and2way.R"))

