## ------------------------------------------------------------------------
library(mplot)
DT::datatable(diabetes, options = list(dom = 'tp'))

## ------------------------------------------------------------------------
lm.d = lm(y ~ ., data = diabetes)
summary(lm.d)

## ---- eval = FALSE-------------------------------------------------------
#  vis.d = vis(lm.d, B = 200)
#  af.d = af(lm.d, B = 200, n.c = 100, c.max = 100)
#  save(lm.d, vis.d, af.d, file = "diabetes_main.RData")

## ------------------------------------------------------------------------
load("diabetes_main.RData")

## ---- results = "asis", message=FALSE------------------------------------
plot(vis.d, which = "vip", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(vis.d, which = "boot", max.circle = 0.25, highlight = "hdl", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(af.d, best.only = TRUE, legend.position = "bottomright", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(af.d, best.only = FALSE, interactive = TRUE, tag = "chart")

## ------------------------------------------------------------------------
vis.d

## ------------------------------------------------------------------------
lm.d.main = lm(y ~ sex + bmi + map + hdl + ltg, data = diabetes)
summary(lm.d.main)
db.main = diabetes[, c("sex", "bmi", "map", "hdl", "ltg")]
db.main$y = lm.d.main$residuals
lm.d.int = lm(y ~ .*. - sex - bmi - map - hdl - ltg, data = db.main)
summary(lm.d.int)

## ---- eval = FALSE-------------------------------------------------------
#  vis.d.int = vis(lm.d.int, B = 200)
#  af.d.int = af(lm.d.int, B = 200, n.c = 100)
#  save(lm.d.int, vis.d.int, af.d.int, file = "diabetes_int.RData")

## ------------------------------------------------------------------------
load("diabetes_int.RData")
vis.d.int

## ---- results = "asis", message=FALSE------------------------------------
plot(vis.d.int, which = "vip", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(vis.d.int, which = "boot", max.circle = 0.25, highlight = "bmi.map", interactive = TRUE, tag = "chart")

## ---- results = "asis", message=FALSE------------------------------------
plot(af.d.int, best.only = TRUE, legend.position = "bottomright", interactive = TRUE, tag = "chart")

