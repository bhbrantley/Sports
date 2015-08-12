####################################################

#### PGA Player Information ---------------------

####################################################

####  Libraries -------------------
library(rvest)
library(XML)


####  Player Extraction -------------------

# Set source
html <- html("http://www.pgatour.com/stats/stat.138.html")

# Player name
player <- html_nodes(html, ".player-name") %>%
  html_text(player, trim = TRUE)
head(player)
length(player)
# Convert to dataframe and remove unnecessary rows
player <- as.data.frame(player)
player <- player[-c(1,2), ]

# Number of Total Events
events <- html_nodes(html, ".hidden-small:nth-child(4)") %>%
  html_text(events) %>%
  as.numeric()
head(events)
length(events)
events <- as.data.frame(events)

# Number of Top Ten finished
stat.topten <- html_nodes(html, ".hidden-small:nth-child(5)") %>% 
  html_text(stat.topten) %>%
  as.numeric()
length(stat.topten)
stat.topten <- as.data.frame(stat.topten)

# Number of First place finishes
stat.first <- html_nodes(html, ".hidden-small:nth-child(6)") %>%
  html_text(stat.first) %>%
  as.numeric()
length(stat.first)
stat.first <- as.data.frame(stat.first)

# Number of Second place finishes
stat.second <- html_nodes(html, ".hidden-small:nth-child(7)") %>% 
  html_text(stat.second) %>%
  as.numeric()
length(stat.second)
stat.second <- as.data.frame(stat.second)

# Number of Third place finishes
stat.third <- html_nodes(html, ".hidden-small:nth-child(8)") %>%
  html_text(stat.third) %>%
  as.numeric()
length(stat.third)
stat.third <- as.data.frame(stat.third)


####  Create Data frame -------------------
df <- as.data.frame(cbind(player, events, stat.topten, stat.first, stat.second))
