# Small helper function to plot Cox-Snell residuals (modificada)
# From:
# www.stat.uni-muenchen.de/~semwiso/lebensdaueranalyse_ws0910/
# download/diagnostics.R
plot.coxsnell <- function(m, which=rep(TRUE,m$n),...) {
  delta <- m$y[,"status"]
  r.cs <- delta - residuals(m, type="martingale")
  r.surv <- survfit(Surv(r.cs[which],delta[which])~1,
                    type="fleming-harrington")
  if (!is.null(list(...)$add)) {
    if (list(...)$add == TRUE) {
      lines(r.surv$time, -log(r.surv$surv),type="s",...)
    }
  } else {
    plot(r.surv$time, -log(r.surv$surv),
         xlab="Residuos Cox-Snell", ylab="Taxa de Falha Acumulada",
         type="s",ylim=c(0,max(r.cs)),...)
  }
  t <- seq(0, max(r.cs),length=100)
  lines(t,t,lwd=3)
  invisible()
}