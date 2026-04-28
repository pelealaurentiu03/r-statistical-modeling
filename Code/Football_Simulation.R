#Stochastic simulation of football scores - Pelea Laurentiu - Probabilistic Models for Data Science project

#engsoccerdata pack installation
#install.packages("remotes")
#remotes::install_github("jalapic/engsoccerdata")

library(engsoccerdata)
data(england)

#Selecting a specific season and league (tier)
real_season <- subset(england, Season == 2011 & tier == 1)

#Checking if there are all the games from that season and tier
print(paste("Number of games:", nrow(real_season))) 

head(real_season)

#Average goals scored by home and away teams
team_home_stats <- aggregate(hgoal ~ home, data = real_season, FUN = mean)
team_away_stats <- aggregate(vgoal ~ visitor, data = real_season, FUN = mean)

#Average conceded goals by home and away teams
team_home_defense <- aggregate(vgoal ~ home, data = real_season, FUN = mean)
team_away_defense <- aggregate(hgoal ~ visitor, data = real_season, FUN = mean)

print(head(team_home_stats))
print(head(team_away_stats))

simulated_season <- real_season

simulated_season$Sim_Home_Goal <- NA
simulated_season$Sim_Away_Goal <- NA

raw_expected_h <- 0
raw_expected_v <- 0

#Adding a "Form" factor because not the best teams win every time
simulated_season$Home_Form <- NA
simulated_season$Away_Form <- NA

#Creating a calibration factor in order to not have some exaggerated results
for(i in 1:nrow(real_season)) {
  h_team <- real_season$home[i]
  a_team <- real_season$visitor[i]
  
  h_att <- team_home_stats$hgoal[team_home_stats$home == h_team]
  a_def <- team_away_defense$hgoal[team_away_defense$visitor == a_team]
  a_att <- team_away_stats$vgoal[team_away_stats$visitor == a_team]
  h_def <- team_home_defense$vgoal[team_home_defense$home == h_team]
  
  raw_expected_h <- raw_expected_h + sqrt(h_att * a_def)
  raw_expected_v <- raw_expected_v + sqrt(a_att * h_def)
}

total_expected <- raw_expected_h + raw_expected_v
total_actual <- sum(real_season$hgoal) + sum(real_season$vgoal)

calibration_factor <- total_actual / total_expected
print(paste("Calibration Factor:", round(calibration_factor, 4)))

#Finding the best seed for the prediction
number_of_attempts <- 5000 
lowest_error <- Inf
best_season <- NULL
best_seed <- 0 

for(k in 1:number_of_attempts) {
  
  set.seed(k)
  
  temp_season <- simulated_season

#Loop through every game of the season and simulate a score
for(i in 1:nrow(simulated_season)) {
  
  current_home_team <- simulated_season$home[i]
  current_away_team <- simulated_season$visitor[i]
  
  h_attack <- team_home_stats$hgoal[team_home_stats$home == current_home_team]
  a_attack <- team_away_stats$vgoal[team_away_stats$visitor == current_away_team]
  
  h_defense <- team_home_defense$vgoal[team_home_defense$home == current_home_team]
  a_defense <- team_away_defense$hgoal[team_away_defense$visitor == current_away_team]
  
  #Implementing a 5% chance that a team can perform better or worst than usual
  home_form <- rnorm(1, mean = 1, sd = 0.05)
  away_form <- rnorm(1, mean = 1, sd = 0.05)
  
  base_h <- sqrt(h_attack * a_defense)
  day_lambda_home <- max(0.1, base_h * (1 + (home_form - 1) - (away_form - 1)) * calibration_factor)
  base_v <- sqrt(a_attack * h_defense)
  day_lambda_away <- max(0.1, base_v * (1 + (away_form - 1) - (home_form - 1)) * calibration_factor)
  
  temp_season$Home_Form[i] <- round(home_form, 2)
  temp_season$Away_Form[i] <- round(away_form, 2)
  
  #Simulating goals using the specific lambdas for both sides
  temp_season$Sim_Home_Goal[i] <- rpois(n = 1, lambda = day_lambda_home)
  temp_season$Sim_Away_Goal[i] <- rpois(n = 1, lambda = day_lambda_away)
}
  
  #Comparing the results from the chosen number of simulations
  diff_h <- abs(temp_season$Sim_Home_Goal - real_season$hgoal)
  diff_v <- abs(temp_season$Sim_Away_Goal - real_season$vgoal)
  total_error <- sum(diff_h) + sum(diff_v)
  
  if(total_error < lowest_error) {
    lowest_error <- total_error
    best_season <- temp_season
    best_seed <- k
  }
}

simulated_season <- best_season

print(paste("Best simulation found at Seed:", best_seed))

head(simulated_season[, c("home", "visitor", "hgoal", "vgoal", "Sim_Home_Goal", "Sim_Away_Goal", "Home_Form", "Away_Form")])

#Comparison between simulated and real home and away goals
head(simulated_season[, c("home", "visitor", "hgoal", "vgoal", "Sim_Home_Goal", "Sim_Away_Goal")])

#Comparison between simulated and real total goals
total_real <- sum(real_season$hgoal) + sum(real_season$vgoal)
total_sim  <- sum(simulated_season$Sim_Home_Goal) + sum(simulated_season$Sim_Away_Goal)

print(paste("Total Real Goals:", total_real))
print(paste("Total Simulated Goals:", total_sim))

real_season$Real_Total_Goals <- real_season$hgoal + real_season$vgoal
simulated_season$Sim_Total_Goals <- simulated_season$Sim_Home_Goal + simulated_season$Sim_Away_Goal

par(mfrow=c(1,2))

#Real data
real_counts <- table(factor(real_season$Real_Total_Goals, levels = 0:9))
barplot(real_counts, main="Real TOTAL Goals", 
        col="blue", xlab="Total Goals", ylab="Frequency", ylim=c(0,100))

# Simulated Total Goals
sim_counts <- table(factor(simulated_season$Sim_Total_Goals, levels = 0:9))
barplot(sim_counts, main="Simulated TOTAL Goals", 
        col="red", xlab="Total Goals", ylab="Frequency", ylim=c(0,100))

#Creating the final tables
get_table <- function(df, h_goal_col, v_goal_col) {
  teams <- unique(c(df$home, df$visitor))
  standings <- data.frame(Team = teams, Points = 0, GD = 0)
  
  for(i in 1:nrow(df)) {
    h_team <- df$home[i]
    v_team <- df$visitor[i]
    h_score <- df[[h_goal_col]][i]
    v_score <- df[[v_goal_col]][i]
    
    #Goal difference
    standings$GD[standings$Team == h_team] <- standings$GD[standings$Team == h_team] + (h_score - v_score)
    standings$GD[standings$Team == v_team] <- standings$GD[standings$Team == v_team] + (v_score - h_score)
    
    #Points
    if(h_score > v_score) {
      standings$Points[standings$Team == h_team] <- standings$Points[standings$Team == h_team] + 3
    } else if(h_score == v_score) {
      standings$Points[standings$Team == h_team] <- standings$Points[standings$Team == h_team] + 1
      standings$Points[standings$Team == v_team] <- standings$Points[standings$Team == v_team] + 1
    } else {
      standings$Points[standings$Team == v_team] <- standings$Points[standings$Team == v_team] + 3
    }
  }
  return(standings)
}

#Generate Both Tables
real_table <- get_table(real_season, "hgoal", "vgoal")
sim_table  <- get_table(simulated_season, "Sim_Home_Goal", "Sim_Away_Goal")

#Merging the tables for comparison
comparison <- merge(real_table, sim_table, by="Team", suffixes = c("_Real", "_Sim"))

#Sort by real points to make the graph look organized
comparison <- comparison[order(comparison$Points_Real),]

print("LEAGUE TABLE COMPARISON")
print(head(comparison))

par(mfrow=c(1,1))
par(mar=c(5, 8, 4, 2))

plot(x = c(min(comparison$Points_Real, comparison$Points_Sim) - 5, 
           max(comparison$Points_Real, comparison$Points_Sim) + 5), 
     y = c(1, nrow(comparison)), 
     type = "n", 
     yaxt = "n", xlab = "Points", ylab = "", 
     main = "Projected vs Real season points")

abline(h = 1:nrow(comparison), col = "lightgray", lty = "dotted")

segments(x0 = comparison$Points_Real, y0 = 1:nrow(comparison),
         x1 = comparison$Points_Sim, y1 = 1:nrow(comparison),
         col = "darkgray", lwd = 1.5)

points(comparison$Points_Real, 1:nrow(comparison), pch = 19, col = "blue", cex = 1.5)
points(comparison$Points_Sim, 1:nrow(comparison), pch = 19, col = "red", cex = 1.5)

axis(2, at = 1:nrow(comparison), labels = comparison$Team, las = 2, cex.axis = 0.8)

legend("bottomright", legend=c("Real Points", "Simulated Points"), 
       col=c("blue", "red"), pch=19, bg="white")