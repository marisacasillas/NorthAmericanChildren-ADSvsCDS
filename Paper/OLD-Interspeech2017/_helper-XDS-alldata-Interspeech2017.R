library(tidyverse)
library(data.table)
library(gtable)
library(arm)
library(gridExtra)
library(stringr)
library(grid)
library(lme4)
library(stringr)
options(dplyr.width = Inf)

# My basic/typical plotting theme settings
basic.theme <- theme(
	panel.background = element_rect(
		fill = "transparent",colour = NA),
	plot.background = element_rect(
		fill = "transparent",colour = NA),
	legend.background = element_rect(
		fill="transparent"),
	legend.text = element_text(size=30),
	legend.title = element_text(size=30),
	legend.key = element_rect(colour = NA, fill = NA),
	legend.key.height = unit(2, "lines"),
	axis.text.x = element_text(size=30),
	axis.title.x = element_text(size=30),
	axis.text.y = element_text(size=30),
	axis.title.y = element_text(size=30),
	strip.text = element_text(size=30),
	panel.margin = unit(2, "lines"))

# Used later to return majority annotations
# (here specifically those coded by 2/3+ of coders)
majority <- function(strlist) {
	annots <- rev(sort(table(strlist)))
	rel_score <- (annots[[1]]/sum(annots))
	if (!is.na(rel_score)) {
		if ((annots[[1]]/sum(annots)) > 0.66) {
			return (as.character(names(annots[1])))
		} else {
			return ("NMJ")
		}		
	} else {
		return ("NA")
	}
}