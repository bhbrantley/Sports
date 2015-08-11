####################################################

#### PGA Tournament Prediction ---------------------

####################################################

####  Libraries -------------------
library(rvest)
library(XML)

# Set source
html <- html("http://www.pgatour.com/stats/stat.138.html")

# Player name
player <- html_nodes(html, ".player-name") %>%
  html_text(player, trim = TRUE)
head(player)
length(player)

# Number of Total Events
events <- html_nodes(html, ".hidden-small:nth-child(4)") %>%
  html_text(events)
head(events)
length(events)

# Number of Top Ten finished
stat.topten <- html_nodes(html, ".hidden-small:nth-child(5)") %>% 
  html_text(stat.topten)
length(stat.topten)

# Number of First place finishes
stat.first <- html_nodes(html, ".hidden-small:nth-child(6)") %>%
  html_text(stat.first)
length(stat.first)

# Number of Second place finishes
stat.second <- html_nodes(html, ".hidden-small:nth-child(7)") %>% 
  html_text(stat.second)
length(stat.second)


# Number of Third place finishes
stat.third <- html_nodes(html, ".hidden-small:nth-child(8)") %>%
  html_text(stat.third) %>%
length(stat.third)

####  Create Data frame -------------------
df <- as.data.frame(cbind(player, events, stat.topten, stat.first, stat.second, stat.third))

