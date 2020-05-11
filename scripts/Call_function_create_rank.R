# Calls to the function

points_teams_final_1 <- as_tibble(map_dfr(1, create_rank))
points_teams_final_2 <- as_tibble(map_dfr(2, create_rank))
points_teams_final_3 <- as_tibble(map_dfr(1:2, create_rank))


