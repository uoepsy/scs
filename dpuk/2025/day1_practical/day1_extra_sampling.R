# Extra: Sampling Variability
# ======================

# how tall are scottish people?  
mysample <- c(155, 183.8, 162.6, 164.5, 157.3, 
              189.7, 150.7, 154, 164.5, 167.8,
              151.5, 148.2, 162.9, 156.6, 162.2, 
              173.8, 164.4, 153.4, 151.4, 170.1)

mean(mysample)

# what if.. 
# we imagine doing our study again. and again.. and again

# pretend, that we have data from everyone in scotland
# this is scotland, with known mean and sd:  
scotland <- rnorm(n = 5500000, mean = 168, sd = 12)
# this is a sample
sample(scotland, size = 20)

# each time we run it, we get a different 20.

# different means from n=20
mean(sample(scotland, size = 20))

# do <code> many times:
replicate(10, "hello")

manymeans <- replicate(1000, mean(sample(scotland, size = 20)) )
hist(manymeans, breaks = 30)
sd(manymeans)

# how likely are we to see our sample n=20, with mean of, 
mean(mysample)
# **given that** the mean of the scottish population is 168?  
hist(manymeans, breaks = 30, freq = FALSE)
abline(v=162.22, col="red", lwd = 2)

sum(manymeans <= 162.22)/1000



## BUT WE DON'T HAVE ALL THAT DATA! 
rm(scotland)
rm(manymeans)
# this is all we have:
mysample

curve(dnorm(x, mean = 168, sd = sd(mysample)/sqrt(20) ), 
            from = 160, to = 178, add = TRUE, lwd = 2)

# SE:
sd(mysample)/sqrt(20)
# test statistic
(162.22 - 168) / (sd(mysample)/sqrt(20))

pnorm(-2.36)

pt(-2.36, df = 19)

t.test(mysample, mu = 168, alternative = "less")


abline(v=(168-162.22)+168, col="red", lwd = 2)

t.test(mysample, mu = 168, alternative = "two.sided")

pt(-2.36, df = 19) * 2





