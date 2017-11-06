## ---- message = FALSE----------------------------------------------------
data("birthwt", package = "MASS")
bwt <- with(birthwt, {
  race <- factor(race, labels = c("white", "black", "other"))
  ptd <- factor(ptl > 0)
  ftv <- factor(ftv)
  levels(ftv)[-(1:2)] <- "2+"
  data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
})
options(contrasts = c("contr.treatment", "contr.poly"))
bw.glm <- glm(low ~ ., family = binomial, data = bwt)
round(summary(bw.glm)$coef, 2)

## ---- eval = FALSE-------------------------------------------------------
#  af.bw = af(bw.glm, B = 150, c.max = 20, n.c = 40)
#  vis.bw = vis(bw.glm, B = 150)
#  save(bw.glm, af.bw, vis.bw, file = "bw_main.RData")

## ---- results = "asis", message=FALSE------------------------------------
load("bw_main.RData")
plot(vis.bw, which = "vip", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(vis.bw, which = "boot", highlight = "htTRUE", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(af.bw, interactive = TRUE, tag = "chart")

## ------------------------------------------------------------------------
pihat = bw.glm$fitted.values
r = bw.glm$residuals 
z = log(pihat/(1 - pihat)) + r
v = pihat*(1 - pihat)
nbwt = bwt
nbwt$z = z
nbwt$low = NULL
bw.lm = lm(z ~ ., data = nbwt, weights = v)

## ---- eval = FALSE-------------------------------------------------------
#  bw.lm.vis = vis(bw.lm, B = 150)
#  bw.lm.af = af(bw.lm, B = 150, c.max = 20, n.c = 40)
#  save(bw.lm, bw.lm.vis, bw.lm.af, file = "bw_lm.RData")

## ---- results = "asis", message = FALSE----------------------------------
load("bw_lm.RData")
plot(bw.lm.vis, which = "vip", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(bw.lm.vis, which = "boot", highlight = "htTRUE", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(bw.lm.af, interactive = TRUE, tag = "chart")

