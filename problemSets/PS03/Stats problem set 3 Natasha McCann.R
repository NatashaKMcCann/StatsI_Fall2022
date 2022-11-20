install.packages("stargazer")
install.packages("dplyr")
install.packages("ggplot2")

library(stargazer)
library(dplyr)
library(ggplot2)

inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/incumbents_subset.csv")
dat <- inc.sub
# Question 1 
# q1
tab1 <- lm(voteshare ~ difflog, data = dat)
stargazer(tab1, type = "text")
# q2
plot(dat$voteshare,dat$difflog,
     main='Regression for VoteShare and Difflog',
     xlab='voteshare',ylab='difflog')
abline(lm(voteshare~difflog,data=dat),col='blue')
#q3

summary(tab1)
res1
res1 <- resid(tab1)
#q4
# Y = 0.579031 + 0.41666(x)

# Question 2 
#1  pressvote difflog
tab2 <- lm(presvote ~ difflog, data = dat)
stargazer(tab2, type = "text")
#2 
plot(dat$presvote,dat$difflog,
     main='Regression for Presvote and Difflog',
     xlab='presvote',ylab='Difflog')
abline(lm(presvote ~ difflog, data = dat),col='blue')
#3
res2 <- resid(tab2)
res2 
#4
summary(tab2)
##

# Question 3
#1 
tab3 <- lm(voteshare ~ presvote, data = dat)
stargazer(tab3, type = "text")
#2 
plot(dat$voteshare,dat$presvote,
     main='Regression for Voteshare and Presvote',
     xlab='voteshare',ylab='presvote')
abline(lm(voteshare ~ presvote, data = dat),col='blue')
#3
res3 <- resid(tab3)
res3
#4
summary(tab3)
# Y =  0.441330 + 0.388018(x)
#Question 4
#1
tab4 <- lm(res1 ~ res2, data = dat)
stargazer(tab4, type = "text")
#2
plot(res1,res2,
     main='Regression for Residuals of question 1 and 2',
     xlab='res1',ylab='res2')
abline(lm(res1 ~ res2, data = dat),col='blue')
#3
summary(tab4)
# Y = -4.860e-18 + 2.569e-01

# Question 5
tab5 <- lm(voteshare ~ presvote + difflog , data = dat)
stargazer(tab5, type = "text")
#2
summary(tab5)
# Y = 0.4486442 + 0.2568770 + 0.0355431(x)
#3
# The outcomes were similar because in question 4 we were using the residuals of questions 1 and 2 which included voteshare, difflog and presvote. 