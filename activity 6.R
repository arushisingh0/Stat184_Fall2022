library(dcData)
data(BabyNames)
library(dplyr)
library(ggplot2)

help ("BabyNames")

data("Minneapolis2013")
View("Minneapolis2013")
nrow("Minneapolis2013")

sort(table(Minneapolis2013$Second),descending = TRUE) #Fix
    
length(which(Minneapolis2013$First=="undervote"))

length(which(Minneapolis2013$Second=="undervote"))

length(which(Minneapolis2013$Third=="undervote"))

# most common first choice --- make a list of first by count 
# most common second choice --- same thing for second 

sort(table(Minneapolis2013$First))

sort(table(Minneapolis2013$Second),descending = TRUE) #Fix

library(dplyr)
rename(count(Minneapolis2013, First, Second),Freq = n) 
  

library(magrittr)
library(tidyverse)
Minneapolis2013 |> 
  group_by(First,Second) |>
  summarize(count = n()) |>
  arrange(desc(count))

table(Minneapolis2013$Precinct)

#popular names
View(BabyNames)

colnames(BabyNames) <- c("name", "sex", "count", "year")
head(BabyNames)

aNames <- BabyNames |>
  filter(str_detect(string = name,pattern = "A")) 
head(aNames)

aNames <- BabyNames |>
  filter(name %in% c("Anna", "Alice", "Annie", "Ada", "Agnes"))
  
ggplot(data = aNames, 
      mapping = aes(x = year, y = count, color = name)) + 
      geom_line()+
      geom_point()+
      theme_minimal() +
      ylab("Counts") +
      xlab("Year") + 
      theme(text = element_text(size = 10)) +
      ggtitle("The Popularity of the Most Popular Names starting with the Letter A in 1880 Over Time")

