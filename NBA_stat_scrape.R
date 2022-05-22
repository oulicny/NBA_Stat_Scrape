setwd("/Users/owenulicny/Documents/DataScience/R_folder/Random/NBA_stats")

nba_stats <- function() {
 
   library(rvest)
  library(lubridate)
  library(tidyverse)
  library(janitor)
  
  nba_stats <- read_html("https://www.basketball-reference.com/leagues/NBA_2022.html")
  
  tables <- nba_stats %>% 
    html_table(fill = T)
  
  nba_stat_table <- tables[[5]]
  
  nba_team_stats <- nba_stat_table %>% 
    clean_names() %>% 
    mutate(orb_percent = (orb/trb),
           tov_percent = tov/100) %>% 
    select(team, fg_percent, tov_percent, orb_percent, ft_percent) %>% 
    arrange(team)
  
  
  nba_opp_stat_table <- tables[[6]]
  
  nba_opp_stats <- nba_opp_stat_table %>% 
    clean_names() %>% 
    mutate(opp_orb_pct = orb/trb,
           opp_tov_pct = tov/100) %>% 
    rename(
      opp_fg_pct = fg_percent,
      opp_ft_pct = ft_percent)
  
  nba_total_stats <- merge(nba_team_stats, nba_opp_stats, by = "team", all.x = T)
  
  nba_total_stats <- nba_total_stats %>% 
    select(team, fg_percent, tov_percent, orb_percent, ft_percent,
           opp_fg_pct, opp_tov_pct, opp_orb_pct, opp_ft_pct) %>% 
    arrange(team)
  
  nba_total_stats_clean <- nba_total_stats %>% 
    mutate(off_rating = ((.4*fg_percent)+(.25*(1-tov_percent)) + (.2*orb_percent)
                         + (.15*ft_percent)),
           def_rating = (.4*(1-opp_fg_pct))+ (.25*opp_tov_pct) + (.2*(1-opp_orb_pct)) 
           + (.15*(1-opp_ft_pct)),
           tot_rating = off_rating + def_rating)
  
  nba_total_stats_clean <- nba_total_stats_clean %>% 
    select(team, off_rating, def_rating, tot_rating) %>% 
    arrange(team)
  
  writexl::write_xlsx(nba_total_stats_clean, "NBA Bets.xlsx")

}

nba_stats()
