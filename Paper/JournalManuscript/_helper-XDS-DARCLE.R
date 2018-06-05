library(tidyverse)
library(broom)
library(data.table)
library(gtable)
library(arm)
library(gridExtra)
library(stringr)
library(grid)
library(lme4)
library(stringr)
library(forcats)
library(ggpirate)
library(ggpubr)
library(irr)
options(dplyr.width = Inf)

# Set this source file's directory as the working directory
here <- dirname(parent.frame(2)$ofile)
setwd(here)

# Relative paths used
input.path <- "input/"
results.plot.path <- "plots/results/"
support.plot.path <- "plots/supporting/"
scripts <- "subscripts/"
models <- "non-logged/"
#models <- "logged/"


# Basic plotting theme settings
basic.theme <- theme(
	panel.background = element_rect(
		fill = "transparent",colour = NA),
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
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
	panel.spacing = unit(2, "lines"),
	plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# To create multi-panel plots (option 1)
multiplot <- function(..., plotlist=NULL, cols) {
    require(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # Make the panel
    plotCols = cols                          # Number of columns of plots
    plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols

    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
    vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
        curRow = ceiling(i/plotCols)
        curCol = (i-1) %% plotCols + 1
        print(plots[[i]], vp = vplayout(curRow, curCol ))
    }

}

# To create multi-panel plots (option 2)
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  # grid.newpage()
  # grid.draw(combined)
  return(combined)

  # return gtable invisibly
  # invisible(combined)

}


# Used to return majority annotations
# (those coded by 2/3+ of coders)
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

# Helpful for plotting residuals with ggplot
qqplot.data <- function (vec) # argument: vector of numbers
{
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids = vec)

  residplot <- ggplot(d, aes(sample = resids)) +
    stat_qq() + geom_abline(slope = slope, intercept = int)

  return(residplot)
}

# Collinearity helpers
# (from https://github.com/aufrank/R-hacks/blob/master/mer-utils.R)
vif.mer <- function (fit) {
    ## adapted from rms::vif
    
    v <- vcov(fit)
    nam <- names(fixef(fit))

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0) {
        v <- v[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }
    
    d <- diag(v)^0.5
    v <- diag(solve(v/(d %o% d)))
    names(v) <- nam
    v
}

kappa.mer <- function (fit,
                       scale = TRUE, center = FALSE,
                       add.intercept = TRUE,
                       exact = FALSE) {
    X <- fit@pp$X
    nam <- names(fixef(fit))
    
    ## exclude intercepts
    nrp <- sum(1 * (nam == "(Intercept)"))
    if (nrp > 0) {
        X <- X[, -(1:nrp), drop = FALSE]
        nam <- nam[-(1:nrp)]
    }

    if (add.intercept) {
        X <- cbind(rep(1), scale(X, scale = scale, center = center))
        kappa(X, exact = exact)
    } else {
        kappa(scale(X, scale = scale, center = scale), exact = exact)
    }
}

colldiag.mer <- function (fit,
                          scale = TRUE, center = FALSE,
                          add.intercept = TRUE) {
    ## adapted from perturb::colldiag, method in Belsley, Kuh, and
    ## Welsch (1980).  look for a high condition index (> 30) with
    ## more than one high variance propotion.  see ?colldiag for more
    ## tips.
    result <- NULL
    if (center) 
        add.intercept <- FALSE
    if (is.matrix(fit) || is.data.frame(fit)) {
        X <- as.matrix(fit)
        nms <- colnames(fit)
    }
    else if (class(fit) == "mer") {
        nms <- names(fixef(fit))
        X <- fit@X
        if (any(grepl("(Intercept)", nms))) {
            add.intercept <- FALSE
        }
    }
    X <- X[!is.na(apply(X, 1, all)), ]

    if (add.intercept) {
        X <- cbind(1, X)
        colnames(X)[1] <- "(Intercept)"
    }
    X <- scale(X, scale = scale, center = center)

    svdX <- svd(X)
    svdX$d
    condindx <- max(svdX$d)/svdX$d
    dim(condindx) <- c(length(condindx), 1)

    Phi = svdX$v %*% diag(1/svdX$d)
    Phi <- t(Phi^2)
    pi <- prop.table(Phi, 2)
    colnames(condindx) <- "cond.index"
    if (!is.null(nms)) {
        rownames(condindx) <- nms
        colnames(pi) <- nms
        rownames(pi) <- nms
    } else {
        rownames(condindx) <- 1:length(condindx)
        colnames(pi) <- 1:ncol(pi)
        rownames(pi) <- 1:nrow(pi)
    }         

    result <- data.frame(cbind(condindx, pi))
    zapsmall(result)
}

maxcorr.mer <- function (fit,
                         exclude.intercept = TRUE) {
    so <- summary(fit)
    corF <- so@vcov@factors$correlation
    nam <- names(fixef(fit))

    ## exclude intercepts
    ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
    if (ns > 0 & exclude.intercept) {
        corF <- corF[-(1:ns), -(1:ns), drop = FALSE]
        nam <- nam[-(1:ns)]
    }
    corF[!lower.tri(corF)] <- 0
    maxCor <- max(corF)
    minCor <- min(corF)
    if (abs(maxCor) > abs(minCor)) {
        zapsmall(maxCor)
    } else {
        zapsmall(minCor)
    }
}