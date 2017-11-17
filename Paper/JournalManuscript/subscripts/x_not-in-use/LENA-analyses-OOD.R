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

