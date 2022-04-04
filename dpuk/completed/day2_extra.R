dpuk <- 
  read_csv("S:\\Spring_Academy_2022 - Spring Academy 2022\\synth1.0 datasets - SUB\\DPUK_synth1.2_sub.csv")
head(dpuk)
names(dpuk)


names(dpuk) <- str_replace(names(dpuk), "_0_0", "")
names(dpuk)

head(dpuk)


# MMSE SYN11_MMSE
# 
# Height SYN15_HEIGHTCM
# Weight SYN15_WEIGHTKG
# 
# Gender SYN01_SEX

dpuk %>%
  select(SYN01_SEX) %>%
  table()

dpuk$SYN01_SEX <- factor(dpuk$SYN01_SEX,
                         levels = c(0, 1),
                         labels = c("Female", "Male"))

dpuk %>%
  select(SYN01_SEX) %>%
  table()

dpuk %>%
  select(SYN15_HEIGHTCM, SYN15_WEIGHTKG, SYN11_MMSE) %>%
  pairs.panels()

dpuk %>%
  select(SYN15_HEIGHTCM, SYN15_WEIGHTKG, SYN11_MMSE) %>%
  describe()

dpuk %>%
  select(SYN15_HEIGHTCM, SYN15_WEIGHTKG, SYN11_MMSE) %>%
  cor(use = "pairwise.complete.obs")

# data <- data %>%
#   filter(!is.na(y) & !is.na(x1) & !is.na(x2))
