# Date: 2/11/2021
# Purpose: This code calls the nba.com API to get isolation play type 
# data and then plots the data with ggplot.
# See this plot: https://twitter.com/tvbassine/status/1359854032806903810
########################################################################

require(httr)

y <- list()

season <- c(
  '2015-16','2016-17','2017-18','2018-19','2019-20', '2020-21')

count <- 1

for(i in 1:length(season)){
    
    headers = c(
      `Connection` = 'keep-alive',
      `Accept` = 'application/json, text/plain, */*',
      `x-nba-stats-token` = 'true',
      `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
      `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
      `x-nba-stats-origin` = 'stats',
      `Sec-Fetch-Site` = 'same-origin',
      `Sec-Fetch-Mode` = 'cors',
      `Referer` = 'https://stats.nba.com/',
      `Accept-Encoding` = 'gzip, deflate, br',
      `Accept-Language` = 'en-US,en;q=0.9'
    )
    
    params = list(
      `PerMode` = "PerGame",
      `LeagueID` ="00",
      `SeasonYear` = season[i],
      `SeasonType` = "Regular Season",
      `PlayerOrTeam` = "T",
      `PlayType` = "Isolation",
      `TypeGrouping` = 'Offensive'
      
    )
    
   
    
    res <- httr::GET(url = 'https://stats.nba.com/stats/synergyplaytypes', httr::add_headers(.headers=headers), query = params)
    # res <- httr::GET(url = url, httr::add_headers(.headers=headers))
    
    json_resp <- jsonlite::fromJSON(content(res, "text"))
    y[[count]] <- data.frame(json_resp$resultSets$rowSet, stringsAsFactors = F)
    y[[count]]$season = season[i]
    y[[count]]$yr = i + 2015
    
    count <- count + 1
    
}



z <- do.call('rbind', y)
z$ppp <- z$X10
z$freq <- z$X9
colnames(z)[1:22] = json_resp$resultSets$headers[[1]]

for(j in 9:10){
  z[,j] <- as.numeric(z[,j])
}

# Get a nice label for each team
z$label <- paste(z$TEAM_ABBREVIATION, z$yr)
z$label_use <- ''
z$label_use[z$PPP >= 1 | z$POSS_PCT >= .1 | z$PPP <.75] = z$label[z$PPP >= 1 | z$POSS_PCT >= .1 | z$PPP <.75]

# Make a nice ggplot
library(ggplot2)
library(ggrepel)

ggplot(z, aes(x=POSS_PCT, y=PPP, label = label_use)) + 
  geom_point() + 
  geom_text_repel() + 
  ggtitle('The Nets Are Isolation Maestros',
          subtitle = paste('Seasons from 2015-16 to 2020-21 | Plot by @tvbassine | Data from nba.com and Synergy')) +
  xlab('Proportion of possessions that are Isolation') +
  ylab('Points Per Poss. on Iso') +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  ylim(c(0.6,1.2))

