library(googleVis); library(RCurl)
library(RJSONIO)
#Use the SportsAnalytics API for R
library(SportsAnalytics)
library(XML); library(stringr)
library(httr)

#Retrieve the NBA data for the 2007-2008 season
nba.seasonYear <- "07-08"
nba.data <- fetch_NBAPlayerStatistics(nba.seasonYear)
save(nba.data, file="nba.data.RData")
load(file="nba.data.RData")
names(nba.data)


#Subset the data for your favorite team
LAL <- subset(nba.data, Team == 'LAL')
#Which player has the best three point percentage? 
LAL.ThreesMade <- aggregate(LAL$ThreesMade, by = list(LAL$Name),FUN = "sum")
names(LAL.ThreesMade)<-c("Names","ThreesMade")
LAL.ThreesAttempted <- aggregate(LAL$ThreesAttempted, by = list(LAL$Name),FUN = "sum")
names(LAL.ThreesAttempted)<-c("Names","ThreesAttempted")
LAL.Three <- merge(LAL.ThreesMade, LAL.ThreesAttempted, by = "Names")
ThreePercentage <- (LAL.ThreesMade$ThreesMade/LAL.ThreesAttempted$ThreesAttempted)*100
LAL.Three$ThreePercentage <- ThreePercentage
#LAL players highest three percentage in dedescending order
LAL.Three.Top <- LAL.Three[with(LAL.Three, order(-ThreePercentage)),]
#LAL best three point percentage
head(LAL.Three.Top,1)

#Which player has played the largest number of minutes?
LAL.minutes <- aggregate(LAL$TotalMinutesPlayed,by = list(LAL$Name),FUN = "sum")
names(LAL.minutes)<-c("Names","TotalMinutesPlayed")
head(LAL.minutes[order(-LAL.minutes$TotalMinutesPlayed),],1)

#Which player has the most "Steals"?
LAL.steals <- aggregate(LAL$Steals,by = list(LAL$Name),FUN = "sum")
names(LAL.steals)<-c("Names","Steals")
head(LAL.steals[order(-LAL.steals$Steals),],1)


#Show 5 teams for the 2007-2008 season that have the most wins in descending order
web <- GET("http://www.landofbasketball.com/yearbyyear/2007_2008_standings.htm")
west <- readHTMLTable(rawToChar(web$content), which=1, stringsAsFactors = FALSE)
east <- readHTMLTable(rawToChar(web$content), which=2, stringsAsFactors = FALSE)
# combine two conference 
nba.standing <- rbind(west, east) 
# converting to numeric 
nba.standing[, 3] <- as.numeric(as.character(nba.standing[, 3] )) 
nba.standing[, 4] <- as.numeric(as.character(nba.standing[, 4] ))
nba.standing[, 5] <- as.numeric(as.character(nba.standing[, 5] ))
nba.standing[, 6] <- as.numeric(as.character(nba.standing[, 6] ))
nba.wins<- aggregate(nba.standing$W,by = list(nba.standing$Team),FUN = "sum")
names(nba.wins)<-c("Team","Wins")
#Top 5 most wins
head(nba.wins[order(-nba.wins$Wins),],5)


#Use at least 5 Google charts to show relevant data from this dataset
#Google Visualization Table
nba.table.chart <- gvisTable(nba.standing[,c("Team", "W", "L", "Pct", "GB")])
plot(nba.table.chart)
#30 teams wins Google Pie Chart
nba.pct<- aggregate(nba.standing$Pct,by = list(nba.standing$Team),FUN = "sum")
names(nba.pct)<-c("Team","Pct")
nba.chart.pie <-  gvisPieChart(head(nba.pct[order(-nba.pct$Pct),],30), options=list(width=1200, height=800))
plot(nba.chart.pie)
#30 teams wins/loses Google Column Chart
nba.loses<- aggregate(nba.standing$L,by = list(nba.standing$Team),FUN = "sum")
names(nba.loses)<-c("Team","Loses")
nba.wl <- merge(nba.wins,nba.loses,by = "Team",sort = FALSE)
nba.chart.column <-  gvisColumnChart(head(nba.wl[order(-nba.wl$Wins),],30), options=list(width=1200, height=800))
plot(nba.chart.column)
#LAL players playing minutes Gauge Chart
LAL.minutes.gauge <- gvisGauge(head(LAL.minutes[order(-LAL.minutes$TotalMinutesPlayed),],16),
                                  options=list(min=0, max=3500,greenFrom=0, greenTo=1000,
                                  yellowFrom=1000, yellowTo=2000,redFrom=2000, redTo=3500,width=1000, height=1000))
plot(LAL.minutes.gauge)
#NBA Organizational Chart
conference <- c()
name <- c("Western","Eastern")
conference[1] <- NA
conference[2] <- "NBA"
conference[3] <- "NBA"
conference[4:18]<- unique(name)[1]
conference[19:33]<- unique(name)[2]
conference
newnode<- c(c("NBA"),name,nba.standing$Team)
newnode
data = data.frame(Node = newnode,Parent = conference,val = 1:33)
conference.org.chart <- gvisOrgChart(data,idvar="Node", parentvar="Parent",tipvar="val", 
options=list(width=1000, height=800, allowCollapse=TRUE))
plot(conference.org.chart)


#Use gvisGeoChart function to display the location on the world map all of the Basketball World Cup 
web1 <- GET("http://www.landofbasketball.com/world_cup_stats/medals_by_year.htm")
worldCup <- readHTMLTable(rawToChar(web1$content), which = 1, stringsAsFactors = FALSE)
names(worldCup)<-c("WorldCup","Space", "Gold", "Silver", "Bronze")
#remove space column 
worldCup <- subset(worldCup, select = c("WorldCup", "Gold", "Silver", "Bronze"))
# remove NA rows
worldCup <- na.omit(worldCup)
#remove first element of the table (table headers)
worldCup <- worldCup[2:nrow(worldCup), ] 
worldCup.winners <- subset(worldCup, select = c("WorldCup","Gold")) 
worldCup.winners$Gold[worldCup.winners$Gold=="FR of Yugoslavia"] <- "Serbia"
worldCup.winners$Gold[worldCup.winners$Gold=="USA"] <- "United States"
head(worldCup.winners, 18)
worldCup.winners.geochart <- gvisGeoChart(head(worldCup.winners,18),"Gold","WorldCup")
plot(worldCup.winners.geochart)
worldCup.winners


