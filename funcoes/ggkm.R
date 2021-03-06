#’ Create a Kaplan-Meier plot using ggplot2
#’
#’ @param sfit a \code{\link[survival]{survfit}} object
#’ @param table logical: Create a table graphic below the K-M plot, indicating at-risk numbers?
#’ @param returns logical: if \code{TRUE}, return an arrangeGrob object
#’ @param xlabs x-axis label
#’ @param ylabs y-axis label
#’ @param ystratalabs The strata labels. \code{Default = levels(summary(sfit)$strata)}
#’ @param ystrataname The legend name. Default = “Strata”
#’ @param timeby numeric: control the granularity along the time-axis
#’ @param main plot title
#’ @param pval logical: add the pvalue to the plot?
#’ @return a ggplot is made. if return=TRUE, then an arrangeGlob object
#’ is returned
#’ @author Abhijit Dasgupta with contributions by Gil Tomas
#’ \url{http://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/}
#’ @export
#’ @examples
#’ \dontrun{
#’ data(colon)
#’  fit <- survfit(Surv(time,status)~rx, data=colon)
#'  ggkm(fit, timeby=500)
#' }
ggkm <- function(sfit, table = TRUE, returns = FALSE,
                 xlabs = "Time", ylabs = "survival probability",
                 ystratalabs = NULL, ystrataname = NULL,
                 timeby = 100, main = "Kaplan-Meier Plot",
                 pval = TRUE, ...) {
  require(ggplot2)
  require(survival)
  require(gridExtra)
  if(is.null(ystratalabs)) {
    ystratalabs <- as.character(levels(summary(sfit)$strata))
  }
  m <- max(nchar(ystratalabs))
  if(is.null(ystrataname)) ystrataname <- "Strata"
  times <- seq(0, max(sfit$time), by = timeby)
  .df <- data.frame(time = sfit$time, n.risk = sfit$n.risk,
                    n.event = sfit$n.event, surv = sfit$surv, strata = summary(sfit, censored = T)$strata,
                    upper = sfit$upper, lower = sfit$lower)
  levels(.df$strata) <- ystratalabs
  zeros <- data.frame(time = 0, surv = 1, strata = factor(ystratalabs, levels=levels(.df$strata)),
                      upper = 1, lower = 1)
  .df <- rbind.fill(zeros, .df)
  d <- length(levels(.df$strata))
  p <- ggplot(.df, aes(time, surv, groups = strata)) +
    geom_step(aes(linetype = strata), size = 0.7) +
    theme_bw() +
    theme(axis.title.x = element_text(vjust = 0.5)) +
    scale_x_continuous(xlabs, breaks = times, limits = c(0, max(sfit$time))) +
    scale_y_continuous(ylabs, limits = c(0, 1)) +
    theme(panel.grid.minor = theme_blank()) +
    theme(legend.position = c(ifelse(m < 10, .28, .35), ifelse(d < 4, .25, .35))) +
    theme(legend.key = theme_rect(colour = NA)) +
    labs(linetype = ystrataname) +
    theme(plot.margin = unit(c(0, 1, .5, ifelse(m < 10, 1.5, 2.5)), "lines")) +
    theme(title = main)
  
  ## Create a blank plot for place-holding
  ## .df <- data.frame()
  blank.pic <- ggplot(.df, aes(time, surv)) +
    geom_blank() +
    theme_bw() +
    theme(axis.text.x = theme_blank(), axis.text.y = theme_blank(),
         axis.title.x = theme_blank(), axis.title.y = theme_blank(),
         axis.ticks = theme_blank(), panel.grid.major = theme_blank(),
         panel.border = theme_blank())
  if(pval) {
    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pval <- pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    pvaltxt <- ifelse(pval < 0.0001, "p < 0.0001", paste("p =", signif(pval, 3)))
    p <- p + annotate("text", x = 0.6 * max(sfit$time), y = 0.1, label = pvaltxt)
  }
  if(table) {
    ## Create table graphic to include at-risk numbers
    risk.data <- data.frame(strata = summary(sfit, times = times, extend = TRUE)$strata,
                            time = summary(sfit, times = times, extend = TRUE)$time,
                            n.risk = summary(sfit, times = times, extend = TRUE)$n.risk)
    data.table <- ggplot(risk.data, aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
      #, color = strata)) +
      geom_text(size = 3.5) +
      theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)), labels = ystratalabs) +
      # scale_y_discrete(#format1ter = abbreviate,
      # breaks = 1:3,
      # labels = ystratalabs) +
      scale_x_continuous("Numbers at risk", limits = c(0, max(sfit$time))) +
      theme(axis.title.x = element_text(size = 10, vjust = 1), panel.grid.major = theme_blank(),
           panel.grid.minor = theme_blank(), panel.border = theme_blank(),
           axis.text.x = theme_blank(), axis.ticks = theme_blank(),
           axis.text.y = element_text(face = "bold", hjust = 1))
    data.table <- data.table + theme(legend.position = "none") +
      xlab(NULL) + ylab(NULL)
    data.table <- data.table +
      theme(plot.margin = unit(c(-1.5, 1, 0.1, ifelse(m < 10, 2.5, 3.5) - 0.28 * m), "lines"))
    ## Plotting the graphs
    ## p <- ggplotGrob(p)
    ## p <- addGrob(p, textGrob(x = unit(.8, "npc"), y = unit(.25, "npc"), label = pvaltxt,
    ## gp = gpar(fontsize = 12)))
    grid.arrange(p, blank.pic, data.table,
                 clip = FALSE, nrow = 3, ncol = 1,
                 heights = unit(c(2, .1, .25),c("null", "null", "null")))
    if(returns) {
      a <- arrangeGrob(p, blank.pic, data.table, clip = FALSE,
                       nrow = 3, ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null")))
      return(a)
    }
  }
  else {
    ## p <- ggplotGrob(p)
    ## p <- addGrob(p, textGrob(x = unit(0.5, "npc"), y = unit(0.23, "npc"),
    ## label = pvaltxt, gp = gpar(fontsize = 12)))
    print(p)
    if(returns) return(p)
  }
}