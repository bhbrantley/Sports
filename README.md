## Overview
Building sports related R scripts and analyses.

### PGA
> Enables scraping the [PGA website](http://www.pgatour.com/stats.html) for player information. This currently includes Top Ten finishes; Number of First, Second, or Third place finishes. This also contains a function to get scoring information, such as birdies, bogeys, and score ratios.

```
library(rvest)  
library(XML)  
library(dplyr)  
```

> As of now, scraping the following websites:    
Top Ten Finishes http://www.pgatour.com/stats/stat.138.html    
Birdie to Bogey Ratio http://www.pgatour.com/stats/stat.02415.html