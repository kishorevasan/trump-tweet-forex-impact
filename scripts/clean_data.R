library(dplyr)

format <- function(filename) {
  df <- read.table(filename, sep="\t")
  df <- filter(df, V1 %in% c("Currency", "Canadian Dollar", "Korean Won"))
  df <- t(df)
  colnames(df) = df[1, ]
  df = df[-1, ]
  return(df)
}

dates <- read.table("../data/unique_dates.csv", sep=",", header = TRUE)
dates$created_at <- as.Date(dates$created_at)
dates <- dates %>%
         mutate(next_day = created_at + 1) %>%
         select(created_at, next_day)
nrow(dates)
write.csv(dates, file="../data/unique_dates2.csv")

df <- format("../data/8_2016.xls")
newdf <- format("../data/9_2016.xls")
df <- rbind(df, newdf)
newdf <- format("../data/10_2016.xls")
df <- rbind(df, newdf)
newdf <- format("../data/11_2016.xls")
df <- rbind(df, newdf)
newdf <- format("../data/12_2016.xls")
df <- rbind(df, newdf)
newdf <- format("../data/1_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/2_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/3_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/4_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/5_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/6_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/7_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/8_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/9_2017.xls")
df <- rbind(df, newdf)
newdf <- format("../data/10_2017.xls")
df <- rbind(df, newdf)

colnames(df) <- c("date", "kor-can", "kor-us")
df <- as.data.frame(df)
df = df[!is.na(df$canada),]
df = df[!is.na(df$korea),]
df$date <- as.Date(df$date, "%m/%d/%y")

df$canada <- gsub(",", '', df$canada)
df$canada <- as.double(df$canada)
df$korea <- gsub(",", '', df$korea)
df$korea <- as.double(df$korea)
df$canada <- df$korea / df$canada

write.csv(df, file="../data/cleaned_currency.csv")

library(ggplot2)
# par(mfrow=c(1,2))


ggplot(data = df,aes(x=seq(1,268)))+geom_point(aes(y=df$canada))+geom_point(aes(y=df$korea))+ggtitle("Trend in Korea")


ggplot(data=df,aes(x=seq(1,268),y=korea))+geom_point()
ggplot(data=df,aes(x=seq(1,268),y=canada))+geom_point()

