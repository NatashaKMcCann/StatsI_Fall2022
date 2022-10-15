# need row total, grand total column total
> # not stopped - row- 27, - column 21 - grand 42
  > # bribe requested - 27 - 13 - 42
  > # stopped warning - 27 - 8 - 42
  > ns <- (27/42)*21
  > br <- (27/42)*13
  > sw <- (27/42)*8
  > # (original numb - ns)squared divided by ns plus the other columns
# upper class
      > nss <- (14-13.5)^2
    > nssd <- nss/13.5
    > brs <- (6-8.35)^2
    > brsd <- brs/8.35
    > sws <- (7-5.14)^2
    > swsd <- sws/5.14

# lower class 
# row total - 15, 
nsl <- (15/42)*21
brl <- (15/42)*13      
swl <- (15/42)*8      

nsls <- (7-7.5)^2    
nslsd <- nsls/7.5      
brls <- (7-4.64)^2
brlsd <- brls/4.64
swls <- (1-2.85)^2
swlsd <- swls/2.85
# add all of the totals together for the final 
f <- nssd + brsd + swsd + nslsd + brlsd + swlsd
f
# part 2
df <- (3-1)*(2-1)
pval <- pchisq(f, df, lower.tail = F)
pval
# the two variables are not indepentant of each other


# part 3 standard residuals 
# number in table - ns divided by ns(1- row totaldivided by grandtotal)(1-column total divided by grand total)

# NS 
nsone <- (0.0185185185185185*(1-(27/42)*(1-(21/42))))^2
nstwo<- ns-nssd
nsthree <- (nsone/nstwo)
# br
brone <- brsd*(1-(27/42)*(1-(13/42)))^2
brtwo <- br-brsd
brthree <- (brone/brtwo)
#sw
swone <- swsd*(1-(27/42)*(1-(8/42)))^2
swtwo <- sw-swsd
swthree <- (swone/swtwo)


#nsl
nslone <- nsl*(1-(15/42)*(1/(21/42)))^2
nsltwo <- nsl-nslsd           
nslthree <- (nslone/nsltwo)
#brl
brlone <- brl*(1-(15/42)*(1-(13/42)))^2
brltwo <- brl-brlsd
brlthree <- (brlone/brltwo)
#swl
swlone <- swl*(1-(15/42)*(1-(8/42)))^2
swltwo <- swl-swlsd
swlthree <- (swlone/swltwo)

nsthree

brthree
swthree
nslthree
brlthree
swlthree


# Question 2 
https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv
# null- male politicians are just as likely to improve drinking water
#alternative- female politicians are more likely than men to improbe drinking water
lm(formula= women$water ~ women$reserved, data= women)
summary(lm(formula= women$water ~ women$reserved, data= women))
