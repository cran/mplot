## ---- eval = FALSE-------------------------------------------------------
#  # install.packages("mplot")
#  data("artificialeg", package = "mplot")
#  help("artificialeg", package = "mplot")

## ------------------------------------------------------------------------
library("mplot")
data("artificialeg")
full.model = lm(y ~ ., data = artificialeg)
round(summary(full.model)$coef, 2)

## ------------------------------------------------------------------------
par(mar=c(0,0,0,0), mgp=c(1,0.5,0), tcl=-0.3, bg = "transparent")
panel.cor <- function(x, y, digits=1, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste("", txt, sep="")
  text(0.5, 0.5, txt, cex = 1)
}
pairs(artificialeg, upper.panel = panel.cor, 
      pch = 19, col = '#22558866', oma = c(1.5,1.5,1.5,1.5),
      cex.labels = 1.25, gap = 0.2)

## ------------------------------------------------------------------------
step.model = step(full.model, trace = 0)
round(summary(step.model)$coef, 2)

