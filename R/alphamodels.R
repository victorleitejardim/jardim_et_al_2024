alpha.mod <- function(x, y, z){
  
  alphamod <- x %>%
    left_join(y) %>%
    left_join(z) %>%
    set_rownames(paste(.$Point, .$Year))
  
  return(alphamod)
}

f.sel <- function(data){
  mod0 <- lm(S.obs ~ 1, data)
  modF <- lm(S.obs ~., data)
  (fsel <- MASS::stepAIC(mod0, direction = "forward",
                scope = list(lower = mod0, upper = modF)))
  return(fsel)
}
