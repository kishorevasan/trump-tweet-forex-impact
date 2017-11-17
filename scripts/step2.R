library(dplyr)
tweets <- read.csv(file='../data/unique_dates.csv', stringsAsFactors = FALSE)
currency <- read.csv(file='../data/Korea_CER.csv', stringsAsFactors = FALSE)

colnames(currency) <- c('date', 'currency')

currency$date <- as.Date(currency$date)
tweets$created_at <- as.Date(tweets$created_at)

delta1 <- c()
delta2 <- c()
curr <- currency %>%
        filter(date == tweets$created_at[1]) %>%
        select(date)

for (i in 1:length(tweets)) {
  idx <- match(tweets$created_at[i], currency$date)
  print(idx)
  before <- as.numeric(currency$currency[idx-1])
  current <- as.numeric(currency$currency[idx])
  after <- as.numeric(currency$currency[idx+1])
  delta1[i] <- (current-before)
  delta2[i] <- (after-current)
}

delta1 <- unlist(delta1)
delta2 <- unlist(delta2)
t.test(delta1, delta2, paired=TRUE)
