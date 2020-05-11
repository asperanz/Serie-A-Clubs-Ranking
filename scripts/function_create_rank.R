create_rank <- function (day_n) {
  
   for (i in day_n) {
    
    tab <- results[[day_n]]
    
    names(tab)[1]<-"date_time"
    names(tab)[2]<-"match"
    names(tab)[3]<-"result_1st_half"
    names(tab)[4]<-"result_2nd_half"
    
    tab <- tab %>% 
      mutate(day = day_n) %>% 
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
    
    points_teams_day <- dplyr::bind_rows(home_teams_day, away_teams_day) %>%
      mutate(points = as.numeric(points)) %>%
      arrange(desc(points))
    
     if (day_n == 1) {    
      teams_day0 <- points_teams_day$team
      points_day0 <- 0
      day_day0 <- 0
      
      points_teams_day0 <- tibble(team = teams_day0, 
                                  points = points_day0, 
                                  day = day_day0)
      
      points_teams_day_prev <<- dplyr::bind_rows(points_teams_day0, points_teams_day)
      return(points_teams_day_prev)
    }

    else {
      points_teams_day <- inner_join(points_teams_day, points_teams_day_prev, by = "team") %>%
        mutate(points.x = as.numeric(points.x),
               points.y = as.numeric(points.y)) %>%
        mutate(points = points.x + points.y) %>%
        rename(day = day.x) %>%
        select(team, points, day) %>%
        arrange(desc(points))
      
      points_teams_day_prev <<- points_teams_day
    }
   }
  return(points_teams_day)
}
