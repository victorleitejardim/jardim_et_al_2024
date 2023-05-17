screep <- function(rda, nb = 10){
  var_exp <- data.frame(PC = ordered(1:length(rda$CA[["eig"]])), eig=rda$CA[["eig"]], bstick = bstick(rda)) 
  
  p <- ggplot(data = var_exp %>%  filter(PC <= nb))+
  geom_col(aes(x = PC, y =  eig), fill = "gray25")+
  geom_point(aes(x = PC, y =  bstick), size = 4, col = "indianred")+
  geom_line(aes(x = PC, y =  bstick, group = 1), col = "indianred")+
  labs(x = "PC", y = "Inertia")

p
}

