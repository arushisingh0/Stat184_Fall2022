read.table(www.dropbox.com/s/ifh16ihiyl04gkt/rawArmyData.csv?dl=1)
library(dplyr)
library(janitor)

library(plyr)
count(armyData,"Marital.Status")

x <- subset(armyData, armyData$Marital.Status == "Single with Children")
x

x2 = select(x,-Marital.Status)


table(armyData$Pay.Grade, armyData$Sex)

xFreqTable <- x2 %>%
  tabyl(Pay.Grade,Sex) %>%
  adorn_totals(where = c("row", "col")) %>%
  adorn_percentages(denominator = "all") %>%
  adorn_pct_formatting(digits = 2) 

x2$FrequencyPerc = 100*(x2$Frequency/sum(x2$Frequency))  

xFreqTable <-x2 %>%
  kable(
    caption = ("Pay-Grade and Sex of those in the Army who are Single with Children"),
    booktabs = TRUE,
    digits = 2
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped","condensed"),
    font_size = 16
  )
xFreqTable

library(ggplot2)
data(diamonds)

xStats <- diamonds %>%
  group_by(cut) %>%
  select(cut, x) %>%
  summarize(
    across(
      .cols = where(is.numeric),
      .fns = list(
        min = ~min(.x, na.rm = TRUE),
        Q1 = ~quantile(.x, probs = 0.2, na.rm = TRUE),
        Q2 =  ~quantile(.x, probs = 0.4, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        Q3 = ~quantile(.x, probs = 0.6, na.rm = TRUE),
        Q4 =  ~quantile(.x, probs = 0.8, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE),
        sam = ~mean(.x, na.rm = TRUE),
        sasd = ~sd(.x, na.rm = TRUE)
      )
    ),
    count = n()
  )

library(kableExtra)
library(knitr)

xStatsTable <- as.data.frame(xStats)

xStatsTable %>%
  kable(
    caption = ("Summary of Length(x) of Diamonds by Cut"),
    booktabs = TRUE,
    col.names = c("Cut", "Minimum","Quintile 1", "Quintile 2", "Median", "Quintile 3", "Quintile 4", "Maximum","Arithmetic Mean", "Standard Deviation", "Count"),
    digits = 2
    ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped","condensed"),
    font_size = 16
  )
  
xStatsTable

collatz <-function(startNum, stoppingPoints= 0) {
  if (startNum == 1) {
    return (stoppingPoints)}
  else if (startNum%%2 == 0){
    collatz(startNum = startNum/2, stoppingPoints <- stoppingPoints+1)}
  else{
    collatz(startNum = 3 * startNum + 1, stoppingPoints <- stoppingPoints+1)}
}


stoppingNumbers <- sapply(
  X = 1:10000,
  FUN = collatz
)
hist(x = stoppingNumbers)

collatzCounter <- Vectorize(
  FUN = collatz, 
  vectorize.args = "startNum")

numbers <- as.numeric(unlist(stoppingNumbers))
hist(x = numbers)

collatzCounterNumeric<- as.numeric(unlist(collatzCounter(startNum = 1:10000)))
hist(x = collatzCounterNumeric)

View(diamonds)





  