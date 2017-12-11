library(readr)
beav1 <- read_csv("~/Downloads/beav1.csv")
beav2 <- read_csv("~/Downloads/beav2.csv")
View(beav2)
View(beav1)

y = beav1$temp
hist(y, prob=TRUE,breaks=20)
curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)


hist(y, prob=TRUE,breaks=20)
curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)


t.test(beav1$temp,beav2$temp,alternative="two.sided")

#QUESTION TWO
proportions <- c(0.13,0.2,0.14,0.16,0.24,0.13)
values <- c(108,133,103,139,133,96)
barplot(values,proportions)

library(ggplot2)
RollDie <-  function(n,up) sample(1:up,n,replace=T)

set1 <- RollDie(10000,12)
set2 <- RollDie(10000,6)+RollDie(10000,6)

barplot(table(set1))
barplot(table(set2))

Samples <- function(vals,n){
  vector <- c()
  for(i in seq(1:n)){
    vector[i] = mean(sample(vals,5))
  }
  return(vector)
}
ggplot(aes(x=values, y=counts))+geom_bar(Samples(set1,5))
barplot(table(Samples(set1,5)),lines="l")
freq=table(Samples(set1,5))
freq
data.frame(cbind(freq))
combinevals <- function(vals){
  d = data.frame(cbind(table(Samples(vals,5))))
  for(i in c(25,125,625)){
    d = rbind(d,cbind(table(Samples(vals,i))))
  }
  return(d)
}
d <- combinevals(set1)

n <- 163+135+71+43+86+77+40+48
df <- 3
males <- c(163,135,71,43)
female <- c(86,77,40,48)
data <- matrix(c(males, female), nrow = 2, ncol = 4, byrow = T)
ctable <- addmargins(matrix(c(males, female), nrow = 2, ncol = 4, byrow = T))

row1 <- ctable[1,5] * ctable[3,]/200 #create expected values for "dead"
row2 <- ctable[2,5] * ctable[3,]/200 #expected values for "alive"
expected <- matrix(c(row1, row2), nrow = 2, ncol = 5, byrow = T)

x2obs <- sum((data[1,1:4]-expected[1,1:4])^2/expected[1,1:4] +(data[2,1:4]-expected[2,1:4])^2/expected[2,1:4]) #computing observed chi-square
pchisq(x2obs,df = 3,lower.tail = F)

install.packages("gmodels")
library("gmodels")
CrossTable(data, expected = T)

data
(males - expected[1,1:4])/sqrt(expected[1,1:4])
(female - expected[2,1:4])/sqrt(expected[2,1:4])

expected1 <- sum(males)*ctable[3,1:4]/n
expected2 <- sum(female)*ctable[3,1:4]/n

(males - expected1)/sqrt(expected1)
(female - expected2)/sqrt(expected2)
ctable

males[1:3]
males

tempmale <- males[1:3]
tempfemale <- female[1:3]
sum(tempmale)
sum(tempfemale)

ctable <- addmargins(matrix(c(tempmale, tempfemale), nrow = 2, ncol = 3, byrow = T))

tempn <- sum(tempmale) + sum(tempfemale)
tempn
expected1 <- sum(tempmale)*ctable[3,1:3]/tempn
expected2 <- sum(tempfemale)*ctable[3,1:3]/tempn
expected1
expected2

data1 <- matrix(c(tempmale, tempfemale), nrow = 2, ncol = 3, byrow = T)
CrossTable(data1, expected=T)
tempmale
tempfemale


finalmale <- c(sum(tempmale),males[4])
finalfemale <- c(sum(tempfemale),female[4])
finaldata <- matrix(c(finalmale,finalfemale),nrow=2,ncol=2,byrow=T)
CrossTable(finaldata,expected=T)

fish <- matrix(c(63,25,320,426), ncol = 2, byrow = T, dimnames = list(c("high", "low"), c("2012", "2014")))
ctable <- addmargins(fish)

n<- 63+25+320+426

row1 <- ctable[1,3]*ctable[3,]/n #the 1st row and 3rd column and the 3rd row of all the columns of
row2 <- ctable[2,3]*ctable[3,]/n #the 2st row and 3rd column and the 3rd row of all the columns
expected.fish <- matrix(c(row1, row2), nrow = 2, ncol = 3, byrow = T)

fij <- c(fish) #convetrting the matrix into vector

fij.hat <- c(expected.fish[,1:2])
x2obs <- sum((fij - fij.hat)^2/fij.hat)

ctable
min(fij)

63*426 - 320*25 
n/2
expected.fish
min(fij.hat)
2*min(fij)
426 - 50 - 0.5
fij
(18838 - 417)*834/(63*320*25*426)

pchisq(x2obs, 1, lower.tail = F) #returns the p value
