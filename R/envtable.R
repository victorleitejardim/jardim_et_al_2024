require(dplyr)


envtable <- function(granulo, hydro, bathy, fetch){
  env <- granulo %>% 
    left_join(hydro) %>% 
    left_join(bathy) %>% 
    left_join(fetch) %>% 
    select(-Season) %>% 
    set_rownames(paste(.$Point, .$Year, sep = "_")) %>% 
    arrange(rownames(.))
  return(env)
}

envmean <- function(data){
  env_mean <- data %>% 
    mutate(Year = as.factor(Year)) %>% 
    group_by(Site, Point) %>% 
    summarise_if(is.numeric, mean)
  return(env_mean)
}