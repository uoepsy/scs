library(tidyverse)
set.seed(12417)
tibble(
    age = rdunif(100,40,70),
    height = rnorm(100,162,10),
    location = sample(0:1,100,replace=T),
    avg_Rusage = rnorm(100,80,25),
    agemot = location*avg_Rusage
) -> df
df$statpref = ((as.matrix(df %>% mutate_all(scale)) %*% c(0,0,1,1,3)) + rnorm(100))[,1]
df$statpref = scale(df$statpref)[,1]*20 + 45
df <- df %>% mutate(across(c(age,height,avg_Rusage,statpref), ~round(.)))
df$location = ifelse(df$location==1,"Edinburgh","Glasgow")
df$name <- randomNames::randomNames(100)
df$name[df$statpref>=82]<-c("King, Josiah","Noe, Umberto","Booth, Thomas")
df$statpref[df$statpref>=82]<-100
df$age[df$statpref>=82,]<-c(35,35,35)
df$avg_Rusage[df$statpref>=82]<-c(120,145,110)
df$statpref[df$avg_Rusage<5]<-25
df$name[df$avg_Rusage<5]<-"Peppapig"
df <- df %>% select(name, age,height,location,avg_Rusage,statpref)
ggplot(df,aes(x=avg_Rusage,y=statpref,col=location))+
    geom_point()
df<-sample_n(df,n())
df<-sample_n(df,n())
head(df)
write_csv(df, "day4_practical/sillydata.csv")
