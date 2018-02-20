# Read in and set up block label data
blkall <- read_csv(paste0(input.path, blocks.input), 
          col_types = cols(
					dont_share = col_factor(levels = c("False", "True")), 
					gender = col_factor(levels = c("FEMALE","MALE")), 
					label = col_factor(levels = c("CDS", "ADS","JUNK")),
					reliability = col_factor(levels = c("False", "True")),
					training = col_factor(levels = c("False", "True")))) %>%
          rename(lena_id = tier, adu_gender = gender) %>%
			    mutate(ID = as.factor(str_match(clan_file, "[A-Z]-[A-Z0-9]+")), 
			           coder = as.factor(coder), lab_name = as.factor(lab_name)) %>%
      		# Remove unhelpful tiers
		      dplyr::select(-training, -reliability, -audiofile, -dont_share)

# Read in and set up participant data
chiall <- read_csv(paste0(input.path, ptcp.input),
				  col_types = cols(
					Gender = col_factor(levels = c("female","male")), 
					Corpus = col_factor(levels = c(
					  "Bergelson", "McDivitt","VanDam2", "Warlaumont")),
					`CaregiverBA/BS` = col_factor(levels = c("no", "yes")))) %>%
  				rename(chi_gender = Gender, ID = `Sample ID`,
  				       ChiID = `Child ID`, adu_univ = `CaregiverBA/BS`) %>%
  				mutate(ID = as.factor(ID), ChiID = as.factor(ChiID))

# Add ACLEW (extra) demographics to (only McDivitt, Warlaumont, & Bergelson)
aclew_extra_demo <- read_csv(paste0(input.path, aclew.input)) %>%
          filter(aclew_id %in% chiall$aclew_id) %>%
          dplyr::select(aclew_id, mat_ed,	number_older_sibs,mother_dob) %>%
          mutate(
            number_older_sibs = as.numeric(as.character(number_older_sibs)),
          mother_dob = as.numeric(as.character(ifelse(
            mother_dob=="unknown", NA, mother_dob))),
         aclew_id = as.character(aclew_id)) %>%
         left_join(
           dplyr::select(chiall, aclew_id, Row, Corpus,
                         ID,ChiID, chi_gender, adu_univ, AgeMonths),
           by = "aclew_id") %>%
         mutate_if(sapply(., is.character), as.factor)

IDS_demo_update <- read_csv(paste0(input.path, vd.demo.update)) %>%
                            bind_rows(aclew_extra_demo) %>% 
                   mutate(ID = as.factor(ID),
                          ChiID = as.factor(ChiID),
                          chi_gender = factor(chi_gender), 
                          adu_univ = factor(adu_univ),
                          Corpus = factor(Corpus),
                          mat_ed = fct_collapse(mat_ed,
                            PH = c("none","BHD"),
                            HS = c("HS", "hs","military_training"),
                            SC = c("SC","sc"),
                            BA = c("BA","ba","cd"),
                            AD = c("AD","ad","MA")),
                          mat_ed = fct_relevel(mat_ed,
                                               "PH","HS","SC","BA","AD"),
                          mat_ed_num = as.numeric(mat_ed),
                          mat_ed_num3 = recode(as.character(mat_ed_num),
                                    "1" = 1, "2" = 1, "3" = 1,
                                    "4" = 2, "5" = 3)) %>%
                          # replaces the one missing age with the median age
                          # for mothers with that degree in that corpus
                          replace_na(list(mother_dob = 32))
# SES explanation: PH is pre-high school degree, HS is high school degree,
#                  SC is some college, BA is college degree, and AD is
#                  advanced degree unknown got lumped with NA, military_training
#                  to lumped with HD, and 'none' from mcdivitt got lumped with
#                  PH. The mat_ed_num and fat_ed_num are just the numeric
#                  versions of these vars so you can relump as you'd like
IDS_demo_update$n_sibs <- IDS_demo_update$number_older_sibs
IDS_demo_update$n_sibs[which(IDS_demo_update$number_older_sibs > 1)] <- 2

# Merge the child data into the block coding
xdsall <- blkall %>%
			    left_join(IDS_demo_update, by = "ID") %>%
    			unite(uttID, ID, block, timestamp,
    			      sep = '.', remove = FALSE) %>%
          tidyr::separate(timestamp, c("utt_on", "utt_off")) %>%
          dplyr::mutate(uniq_block = paste(ID, block, sep='.'),
                        utt_dur = as.numeric(utt_off) - as.numeric(utt_on))

# Calculate agreed-upon "adu_gender" and "label" columns
# for each utterance
# Note: The suffix "_m" indicates a majority value
uniq.uttIDs <-	xdsall %>%
				        dplyr::select(-c(date, coder, lab_name)) %>%
        				dplyr::select(uttID) %>%
        				distinct() %>%
                mutate(adu_gender_m = "",
                       label_m = "")

# Table of block duration
blkdurs <-  xdsall %>%
            filter(instance == 0) %>%
            group_by(uniq_block, ID) %>%
            summarise(blk_dur = round((sum(utt_dur)/60000),2))

# Table of block duration information by corpus
blkdurs.cp <-  xdsall %>%
            filter(instance == 0) %>%
            group_by(uniq_block, Corpus) %>%
            summarise(blk_dur = sum(utt_dur)) %>%
            group_by(Corpus) %>%
            summarise(n = n(),
                      mean = round(mean(blk_dur/60000),2),
                      median = round(median(blk_dur/60000),2),
                      min = round(min(blk_dur/60000),2),
                      max = round(max(blk_dur/60000),2))

# Merge that with the xdsall data for a by-utterance summary
# (there's probably a faster solution here)
summ.lab.gend <-  xdsall %>%
                  dplyr::select(-c(date, coder, lab_name)) %>%
                  filter(lena_id == "MAN"|lena_id == "FAN") %>%
					        group_by(uttID) %>%
        					summarise(label_m = majority(label), 
        					          adu_gender_m = majority(adu_gender))


# Save a single copy (instance == 0) of the data to look at
# data outside the MAN and FAN labels; see figs in sanity-checks-figs
xdscopy <- xdsall %>%
    				dplyr::select(-c(date, coder, lab_name)) %>%
            filter(instance == 0) %>%
			    	dplyr::select(-c(instance))

# save a copy of xdsall with all three codes
xdsall.3instances <- xdsall %>%
			    	right_join(summ.lab.gend, by = "uttID")

# Merge the by-utterance summary back into the big xdsall matrix
xdsall <- xdsall %>%
    				dplyr::select(-c(date, coder, lab_name)) %>%
		    		filter(lena_id == "MAN"|lena_id == "FAN") %>%
				    filter(instance == 0) %>%
			    	dplyr::select(-c(instance)) %>%
			    	right_join(summ.lab.gend, by = "uttID")

