library(tidyverse)
set.seed(987)
xmat = tibble(
  age_at_stroke = rdunif(150, 45,100),
  educ = rdunif(150, 15, 24),
  birthweight_kg = round(rnorm(150, 6.5, 2.3),1),
  apoe4 = sample(0:1, 150, replace = T, prob = c(.4,.6)),
  age_apoe = age_at_stroke*apoe4,
  lesionload = rnorm(150, 40,10),
  educ_ll = educ*lesionload
)
lp = as.matrix(xmat %>% mutate_all(scale)) %*% c(-1.6, 3, 0, -1, -1.5, 1.7, -4.5)

xmat$acer = (lp + rnorm(150, 0, 1))[,1]
lm(acer ~ ., xmat) %>% summary

xmat %>% select(-age_apoe) %>% mutate(pid = paste0("PPT",1:n())) %>% 
  rename(age = age_at_stroke) %>% relocate(pid) -> xmat


table(xmat$apoe4)

#xmat$acer <- pmin(100,(xmat$acer*2)+87)
lm(acer~age*apoe4,xmat) %>% summary 
#  sjPlot::plot_model(.,type="int")
lm(acer~educ*lesionload,xmat) %>% summary
#  sjPlot::plot_model(.,type="int")


xmat %>% transmute(
  pid = pid,
  age = age,
  yrs_educ = educ,
  birthweight_kg = birthweight_kg,
  apoe4status = ifelse(apoe4==1,"positive","negative"),
  lesionload_ml = lesionload,
  acer = round(pmin(100,(scale(xmat$acer)[,1]*3.5)+92))
) -> xmat
lm(acer~age*apoe4status,xmat) %>% summary
lm(acer~yrs_educ*lesionload_ml,xmat) %>% summary
hist(xmat$acer)


mod2 <- lm(acer ~ yrs_educ * lesionload_ml, data =xmat)

plot_model(mod2, type="eff", terms=c("yrs_educ","lesionload_ml"))


write_csv(xmat, "day3_practical/acer.csv")

