require(dplyr)
sphericity <- function(data){
  res <- data %>% 
    mutate(Sphericity = ((S^2)/(L*I))^(1/3)) %>% 
    mutate(DR1 = S/L) %>% 
    mutate(DR2 = ((L-I)/(L-S))) %>% 
    mutate(DR3 = I/L)
}