require(EnvStats)
bc.comp <- function(data){
  bccomp <- data %>% 
    mutate(L = boxcoxTransform(L, boxcox(.$L, optimize = TRUE )$lambda)) %>%
    mutate(I = boxcoxTransform(I, boxcox(.$I, optimize = TRUE )$lambda)) %>%
    mutate(S = boxcoxTransform(S, boxcox(.$S, optimize = TRUE )$lambda)) %>%
    mutate(Total_Density = boxcoxTransform(Total_Density, boxcox(.$Total_Density, optimize = TRUE )$lambda)) %>%
    set_rownames(paste(.$Mini, .$Sample, sep = "_")) %>% 
    ungroup()
}

comp.med <- function(data){
  
  comp_med <- data %>% 
    select(-Sample) %>% 
    group_by(Mini, Site, Point) %>% 
    summarise_if(is.numeric, median) %>%
    ungroup() %>% 
    set_rownames(paste(.$Point))
  
  return(comp_med)
}

comp.site <- function(data){
  
  comp_med <- data %>% 
    select(-Sample) %>% 
    group_by(Site) %>% 
    summarise_if(is.numeric, median) %>%
    ungroup() %>% 
    set_rownames(paste(.$Site))
  
  return(comp_med)
}

bc.compmed <- function(data){
  bccomp_med <- data %>% 
    mutate(L = boxcoxTransform(L, boxcox(.$L, optimize = TRUE )$lambda)) %>%
    mutate(I = boxcoxTransform(I, boxcox(.$I, optimize = TRUE )$lambda)) %>%
    mutate(S = boxcoxTransform(S, boxcox(.$S, optimize = TRUE )$lambda)) %>%
    mutate(Total_Density = boxcoxTransform(Total_Density, boxcox(.$Total_Density, optimize = TRUE )$lambda)) %>% 
    set_rownames(paste(.$Mini)) %>% 
    ungroup()
  
  return(bccomp_med)
}

bc.compsite <- function(data){
  bccomp_med <- data %>% 
    mutate(L = boxcoxTransform(L, boxcox(.$L, optimize = TRUE )$lambda)) %>%
    mutate(I = boxcoxTransform(I, boxcox(.$I, optimize = TRUE )$lambda)) %>%
    mutate(S = boxcoxTransform(S, boxcox(.$S, optimize = TRUE )$lambda)) %>%
    mutate(Total_Density = boxcoxTransform(Total_Density, boxcox(.$Total_Density, optimize = TRUE )$lambda)) %>% 
    set_rownames(paste(.$Site)) %>% 
    ungroup()
  
  return(bccomp_med)
}

comp.num <- function(data){
  compnum <- data %>% 
    select(-Mini:-Sample)
  
  return(compnum)
}

compmed.num <- function(data){
  compnum <- data %>% 
    ungroup() %>% 
    select(-Mini, -Site) %>% 
    column_to_rownames(.,"Point")
  
  return(compnum)
}

compsite.num <- function(data){
  compnum <- data %>% 
    ungroup() %>% 
    column_to_rownames(.,"Site")
  
  return(compnum)
}

extract.pc <- function(data, pca){
  pc.scores <- data %>% 
    mutate(PC1_score =  as.vector(scores(pca,choices = 1, scaling = 1, display = "sites"))) %>% 
    mutate(PC2_score =  as.vector(scores(pca,choices = 2, scaling = 1, display = "sites"))) #reverse y axis
  return(pc.scores)
}

cor.plot <- function(data, pal){
 p1 <- ggplot(data, aes(PC1_score, PC1_c, colour = Site)) +
    geom_point()+
    scale_colour_manual(values = pal) +
    geom_smooth(colour = "gray", method = lm)
 p2 <- ggplot(data, aes(PC1_score, PC1_c, colour = Site)) +
    geom_point()+
    scale_colour_manual(values = pal) +
    geom_smooth(colour = "gray", method = lm)
 return(list(p1, p2))
}


