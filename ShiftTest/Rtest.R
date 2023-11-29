install.packages(dplyr)
library(dplyr)


file <- "trout_standard.csv"

table <- read.csv(file)

head(table)

table1 <- filter(table, events == "double")
table2 <- filter(table, events == "triple")
xbh = nrow(table1) + nrow(table2)
count = nrow(table)

print(xbh)
print(count)

xbhRate = xbh/count
print(xbhRate*100)
  
  
  
  
  
  