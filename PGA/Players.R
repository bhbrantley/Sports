####    PGA Player Information                          -----------------------------------------------------------------------
####    Libraries                                       -----------------------------------------------------------------------
library(rvest)
library(XML)
library(dplyr)

####  PGA Website Links                                 -----------------------------------------------------------------------
# Top Ten Finishes "http://www.pgatour.com/stats/stat.138.html"
# Birdie to Bogey Ratio "http://www.pgatour.com/stats/stat.02415.html"

#### Function to scrape a PGA website                   -----------------------------------------------------------------------
# 'site' is the PGA website link to the page
# 'column' is the string value from Selector Gadget
# 'what' is character string you are adding to name the column value
# Example: scrape("http://www.pgatour.com/stats/stat.02415.html", ".hidden-small:nth-child(5)", "birdie_to_bogey")
scrape <- function(site, column, what){
  print(paste("Scraping PGA website for", what))
  df <- html(paste(site)) %>%
    html_nodes(paste(column)) %>%
    html_text(trim=TRUE) %>%
    as.data.frame() %>%
    slice(-c(1,2))
  names(df)[1] <- what
  df$ID <- seq.int(nrow(df))
  print(paste("Preview of",what,"output"))
  print(head(df))
  return(df)
}

#### Function to scrape a PGA website for Player Names  -----------------------------------------------------------------------
# Used within other functions just for Player Name column
# 'site' is the PGA website link to the page
get_player_name <- function(site){
  print("Getting player names")
  player <- html(paste(site)) %>%
    html_nodes(".player-name") %>%
    html_text(trim=TRUE) %>%
    as.data.frame() %>%
    slice(-c(1,2))
    #slice(3:max(nrow(.)))
    #slice(3:73)
  names(player)[1] <- "PLAYER NAME"
  player$ID <- seq.int(nrow(player))
  return(player)
}

#### Scrape PGA website for stats on Top Ten finishes   -----------------------------------------------------------------------
# Source: "http://www.pgatour.com/stats/stat.138.html"
# 'site' is the PGA website link to the page
# requires get_player_name() function
get_player_stats <- function(site = "http://www.pgatour.com/stats/stat.138.html"){
  # Player name
  player <- get_player_name(site)
  
  # Number of Events
  print("Fetching number of events for each player")
  events <- html(paste(site)) %>%
    html_nodes(".hidden-small:nth-child(4)") %>%
    html_text() %>%
    as.numeric() %>%
    as.data.frame() %>%
    slice(-c(1))
  names(events)[1] <- "Events"
  events$ID <- seq.int(nrow(events))
  
  # Number of Top Ten finishes
  print("Fetching number of Top Ten PGA finishes")
  stat.topten <- html(paste(site)) %>%
    html_nodes(".hidden-small:nth-child(5)") %>% 
    html_text(stat.topten) %>%
    as.numeric() %>%
    as.data.frame() %>%
    slice(-c(1))
  names(stat.topten)[1] <- "TopTen"
  stat.topten$ID <- seq.int(nrow(stat.topten))
  
  # Number of First place finishes
  print("Fetching number of First Place finishes")
  stat.first <- html(paste(site)) %>%
    html_nodes(".hidden-small:nth-child(6)") %>%
    html_text() %>%
    as.numeric() %>%
    as.data.frame() %>%
    slice(-c(1))
  names(stat.first)[1] <- "First"
  stat.first$ID <- seq.int(nrow(stat.first))
  
  # Number of Second place finishes
  print("Fetching number of Second Place finishes")
  stat.second <- html(paste(site)) %>%
    html_nodes(".hidden-small:nth-child(7)") %>% 
    html_text() %>%
    as.numeric() %>%
    as.data.frame() %>%
    slice(-c(1))
  names(stat.second)[1] <- "Second"
  stat.second$ID <- seq.int(nrow(stat.second))
  
  # Number of Third place finishes
  print("Fetching number of Third Place finishes")
  stat.third <- html(paste(site)) %>%
    html_nodes(".hidden-small:nth-child(8)") %>%
    html_text() %>%
    as.numeric() %>%
    as.data.frame() %>%
    slice(-c(1))
  names(stat.third)[1] <- "Third"
  stat.third$ID <- seq.int(nrow(stat.third))
  
  print("building dataframe")
  df.final <- inner_join(player, events) %>%
    inner_join(., stat.topten) %>%
    inner_join(., stat.first) %>%
    inner_join(., stat.second) %>%
    inner_join(., stat.third)
  df.final[is.na(df.final)] <- 0
  print("Columns")
  print(names(df.final))
  return(df.final)
}

#### Scrape PGA website for stats on Scoring Ratio      -----------------------------------------------------------------------
# Source: "http://www.pgatour.com/stats/stat.02415.html"
# 'site' is the PGA website link to the page
# requires get_player_name() function
get_scoring <- function(site = "http://www.pgatour.com/stats/stat.02415.html"){
  # Get players
  player <- get_player_name(site)
  rounds <- scrape("http://www.pgatour.com/stats/stat.02415.html", ".hidden-small:nth-child(4)", "Rounds")
  bird_to_bogey <- scrape("http://www.pgatour.com/stats/stat.02415.html", ".hidden-small:nth-child(5)", "Ratio")
  num_birdies <- scrape("http://www.pgatour.com/stats/stat.02415.html", ".hidden-small:nth-child(6)", "Birdies")
  num_bogeys <- scrape("http://www.pgatour.com/stats/stat.02415.html", ".hidden-small:nth-child(7)", "Bogeys")
  
  df <- inner_join(player, rounds) %>%
    inner_join(., bird_to_bogey) %>%
    inner_join(., num_bogeys) %>%
    inner_join(., num_bogeys)
  df[is.na(df)] <- 0
  print("Preview of birdie to bogey output")
  print(names(df))
  print(head(df))
  return(df)
}

#### Scrape PGA website for stats on Scoring Average      -----------------------------------------------------------------------
# Source: "http://www.pgatour.com/stats/stat.120.html"
# 'site' is the PGA website link to the page
# requires get_player_name() function
get_strokes <- function(site = "http://www.pgatour.com/stats/stat.120.html"){
  # Get players name
  player <- get_player_name(site)
  # Number of rounds per player
  rounds <- scrape(paste(site), ".hidden-small:nth-child(4)", "Rounds")
  # Avg score
  avg_score <- scrape(paste(site), ".hidden-small:nth-child(5)", "AvgScore")
  # Total Strokes
  total_strokes <- scrape(paste(site), ".hidden-small:nth-child(6)", "Total Strokes")
  # Total Adjustment
  adjustment <- scrape(paste(site), ".hidden-small:nth-child(7)", "Adjustment")
  # Create df
  df <- inner_join(player, rounds) %>%
    inner_join(., avg_score) %>%
    inner_join(., total_strokes) %>%
    inner_join(., adjustment)
  df[is.na(df)] <- 0
  print("Preview of Scoring Average output")
  print(names(df))
  print(head(df))
  return(df)
}

#### Method to add two scrapes                          -----------------------------------------------------------------------
# 'x' is a function or df
# 'y' is a function or df
# merge done by 'PLAYER NAME' column
add_player_stats <- function(x, y){
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  df <- merge(x, y, by = "PLAYER NAME")
  return(df)
}

#### Test Case                                          -----------------------------------------------------------------------
add_player_stats(get_player_stats(), get_scoring())
