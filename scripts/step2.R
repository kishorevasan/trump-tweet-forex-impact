library(dplyr)
tweets <- read.csv(file='../data/unique_dates.csv', stringsAsFactors = FALSE)
currency <- read.csv(file='../data/kor_currency_cleaned.csv', stringsAsFactors = FALSE)

currency$date <- as.Date(currency$date)
tweets$created_at <- as.Date(tweets$created_at)

delta1 <- c()
delta2 <- c()

delta.rand1 <- c()
delta.rand2 <- c()

start.date <- tweets$created_at[1]
end.date <- tweets$created_at[length(tweets$created_at)]

dates <- seq(start.date, end.date, by="days")

new.data <- subset(currency, date %in% dates)
new.data <- new.data[!(new.data$date %in% tweets$created_at),]

for(i in 1:5) {
  rand <- new.data[sample(nrow(new.data), length(tweets$created_at)), ]
  for (i in 1:length(rand$date)) {
    index <- match(rand$date[i], currency$date)
    before1 <- as.numeric(currency$currency[index-1])
    current1 <- as.numeric(currency$currency[index])
    after1 <- as.numeric(currency$currency[index+1])
    delta.rand1[i] <- (current1-before1)
    delta.rand2[i] <- (after1-current1)
  }
  delta.rand1 <- unlist(delta.rand1)
  delta.rand2 <- unlist(delta.rand2)
  print(t.test(delta.rand1, delta.rand2, paired=TRUE))
}

for (i in 1:length(tweets$created_at)) {
  idx <- match(tweets$created_at[i], currency$date)
  if (!is.na(idx)) {
    before <- as.numeric(currency$currency[idx-1])
    current <- as.numeric(currency$currency[idx])
    after <- as.numeric(currency$currency[idx+1])
    delta1[i] <- (current-before)
    delta2[i] <- (after-current)
  }
}

delta1 <- unlist(delta1)
delta2 <- unlist(delta2)
t.test(delta1, delta2, paired=TRUE)