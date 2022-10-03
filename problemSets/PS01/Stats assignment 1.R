#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2022/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)
view(expenditure)
library(tidyverse)
expenditure %>%
  filter(expenditure == 2) %>%
  ggplot(aes(Y, X1, X2, X3))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~State)+
  theme_bw()+
  labs(title = "Relationship between Y, X1, X2 and X3")

plot(state(data$State),
     main = "Pdf of Height",
     xlab = "Height (cm)"
)
?ggplot
ggplot(data = expenditure)
geom_point(expenditure)
ggplot(data = expenditure, aes(y = y, X1 = X1)) +
  geom_point()
plot((expenditure))
plot(expenditure$Y, expenditure$X1)
?aes
aes(expenditure$Y, expenditure$X1, expenditure$X2, expenditure$x3, expenditure$Region, expenditure$STATE)
expenditure %>%
  filter(State %in% c("Y", "X1", "X2", "X3")) %>%
  group_by(State) %>%
  ggplot(aes(State, Region)) +
  geom_boxplot()
State <- state.abb
?pairs
pairs(expenditure$y, expenditure$STATE, expenditure$X1)
#q 1 
mean(y) #1. get mean
ymean <- mean(y)
ysd <- sd(y)
lensd <- length(y)
yse <- ysd/sqrt(lensd) 
yse # the standard error
alpha = 0.1
?sqrt
df = lensd - 1
t.score = qt(p=alpha/2, df=df,lower.tail=F)
t.score # found the t-score
lower <- ymean - 0.9
upper <- ymean + 0.9
print(c(lower,upper)) # 90% confidence interval 

# q1 part 2
mean(y) #1. get mean
ymean <- mean(y)
ysd <- sd(y)
lensd <- length(y)
yse <- ysd/sqrt(lensd) 
yse # the standard error
alpha2 = 0.05
df = lensd - 1
t.score2 = qt(p=alpha2/2, df=df,lower.tail=F)
t.score2 # found the t-score
lower2 <- ymean - 0.95
upper2 <- ymean + 0.95
print(c(lower2,upper2)) # 95% confidence interval
