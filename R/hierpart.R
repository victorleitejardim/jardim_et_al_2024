hp_main <- function(data, rda){
  list.hp4 <- list(Depth = data %>% select(Depth), Hydrodynamics = data %>% select(Current_mean, Fetch_max), Granulometry = data %>% select(Mud, OM, Gravel), Temperature = data %>% select(T_mean, T_sd), Complexity = data %>% select(Branching_density, D_gray, D_bin, Sphericity, DR3, Total_Density, L), Sites = data %>%  select(Site), Time = data %>% select(Year))
  rdamaerl.hp4 <- rdacca.hp::rdacca.hp(rda, list.hp4, var.part = TRUE)
  return(rdamaerl.hp4)
  }

hp_sep <- function(data, rda){
  list.hp4 <- list(Depth = data %>% select(Depth), Hydrodynamics = data %>% select(Current_mean, Fetch_max), Granulometry = data %>% select(Mud, OM, Gravel), Temperature = data %>% select(T_mean, T_sd), `Rhodolith complexity` = data %>% select(Branching_density, D_gray, D_bin, Sphericity, DR3), `Bed complexity` = data %>%  select(Total_Density, L), Sites = data %>%  select(Site), Time = data %>% select(Year))
  rdamaerl.hp4 <- rdacca.hp::rdacca.hp(rda, list.hp4, var.part = TRUE)
  return(rdamaerl.hp4)
}