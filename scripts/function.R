create_matches_day <- function (number) 
  {
  
  tab <- results[[number]]
  
  names(tab)[1]<-"date_time"
  names(tab)[2]<-"match"
  names(tab)[3]<-"result_1st_half"
  names(tab)[4]<-"result_2nd_half"
  
  tab <- tab %>% 
    mutate(day = number) %>% 
    tidyr::separate(match, c("home_team", "away_team"), sep = "-", remove = FALSE) %>%
    tidyr::separate(result_2nd_half, c("score_home_team", "score_away_team"), sep = "-", remove = FALSE) %>% 
    mutate(home_team = str_trim(home_team),
           away_team = str_trim(away_team)) %>%
    mutate_at(c("score_home_team", "score_away_team"), as.numeric) %>%
    mutate(points_home_team = str_trim(case_when(score_home_team > score_away_team ~ 3,
                                                 score_home_team == score_away_team ~ 1,
                                                 score_home_team < score_away_team ~ 0))) %>% 
    mutate(points_away_team = str_trim(case_when(score_away_team > score_home_team ~ 3,
                                                 score_away_team == score_home_team ~ 1,
                                                 score_away_team < score_home_team ~ 0))) %>%
    mutate(date = dmy_hm(date_time)) %>% 
    tidyr::fill(date, .direction = "down") %>% 
    mutate(date_day = day(date),
           date_wday = wday(date, label = TRUE, abbr = FALSE),
           date_month = months(date),
           date_year = year(date))
  
  home_teams_day <- tab %>% 
    select(home_team, points_home_team, day) %>% 
    rename(team = home_team,
           points = points_home_team)
  
  away_teams_day <- tab %>% 
    select(away_team, points_away_team, day) %>% 
    rename(team = away_team,
           points = points_away_team)
  
  points_teams_day <- rbind(home_teams_day, away_teams_day) %>% 
    arrange(desc(points))
  }   

points_teams_final <- rbind(create_matches_day(1),
                            create_matches_day(2),
                            create_matches_day(3),
                            create_matches_day(4),
                            create_matches_day(5),
                            create_matches_day(6)
                            # create_matches_day(7) problema con due squadre -> punti a NA
                            )

points_teams_final_milan <- points_teams_final %>% 
  filter(team == "Milan")

points_teams_final_milan %>% 
ggplot(aes(x= day, y = points, group = team)) + 
  geom_line(size = 1.0)

# NO!!! i punti al passare delle giornate vanno sommati


ggplot(points_teams_final, aes(x= day, y = points, group = team)) + 
  geom_line(size = 1.0) +
  geom_segment(aes(xend = 31, yend = points), linetype = 2, colour = 'grey') +
  geom_point(size = 2) +
  geom_text(aes(x = 31.1, label = team), hjust = 0) +
  transition_reveal(Day) +
  scale_color_viridis_d() +
  coord_cartesian(clip = 'off') +
  labs(title = 'Temperature in New York', y = 'Temperature (°F)') + 
  theme_minimal() + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5),
        legend.position="none")

animate(p, fps= 2, end_pause = 30)




identical(points_teams_day1, points_teams_day1f)

--------------------
  
points_teams_finalone <- map(create_matches_day(i),rbind)
