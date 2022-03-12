library (dplyr)

#Fixture List for 2020/21
fixtures <- read.csv("Matches.csv", stringsAsFactors = FALSE)
teams <- unique(fixtures$HOME.TEAM)

#Results since 2010/11
Results <- read.csv("Results.csv", stringsAsFactors = FALSE)

#Average Goals
ave_home <- Results %>% 
  group_by(HomeTeam) %>%
  summarize (ave_scored_h = mean(FTHG), ave_conceded_h = mean(FTAG)) %>%
  filter (HomeTeam %in% teams) %>% rename(Team = HomeTeam)

ave_away <- Results %>%
  group_by(AwayTeam) %>%
  summarize (ave_scored_a = mean(FTAG), ave_conceded_a = mean(FTHG)) %>%
  filter (AwayTeam %in% teams)  %>% rename(Team = AwayTeam)

average <- merge(ave_home, ave_away, by = 'Team')

#Specific Fixture Results
hist_pair.pl <- Results %>%
  group_by(HomeTeam, AwayTeam) %>%
  filter (HomeTeam %in% teams, AwayTeam %in% teams) %>%
  summarize (match = n(), ave_home_scored = mean(FTHG), ave_away_scored = mean(FTAG))

rm(history, ave_home, ave_away)

home = "Arsenal"
away = "Tottenham"

Predicted_score <-(home, away) {
  subset <- hist_pair.pl[which(hist_pair.pl$HomeTeam == home & hist_pair.pl$AwayTeam ==away)]
  
  #Poisson distribution
  h_scored = rpois(1, subset$ave_home_scored[1])
  a_scored = rpois(1, subset$ave_away_scored[1])
  
  (list(h_scored, a_scored))
  
}

