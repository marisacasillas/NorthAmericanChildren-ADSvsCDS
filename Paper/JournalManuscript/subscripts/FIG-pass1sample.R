pass1sample <-  ggplot(chiall, aes(x=AgeMonths, fill=Corpus)) +
  geom_histogram(binwidth=1, colour="black") +
  ylab("# individuals") + xlab("Age (months)") +
  scale_fill_manual(
    labels=c("Bergelson", "McDivitt", "VanDam", "Warlaumont"),
    values=c("firebrick1", "gold1",
             "forestgreen", "dodgerblue1")) +
    basic.theme + theme(legend.position = "right")
figname <- "pass1sample.png"

if (BW == "Y") {
  pass1sample <- pass1sample + scale_fill_grey(start = 1, end = 0)
  figname <- "pass1sample-BW.png"
}

ggsave(plot = pass1sample,
       filename = figname,
       path = results.plot.path,
       width = 35.28,
       height = 10.58,
       units = "cm",dpi = 72,
       bg = "transparent"
)