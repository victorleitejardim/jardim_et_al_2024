require(tidyverse)
fauna.num <- function(data){
  maerl_num <- data %>%
    rownames_to_column("Code") %>% 
    select(-Point:-count) %>% 
    column_to_rownames("Code")
  return(maerl_num)
}

stot <- function(data){
  Spoint <- as.data.frame(t(as.matrix(estimateR(data))))
  Spoint <- Spoint %>% 
    tibble::rownames_to_column(., "code") %>% 
    mutate_if(is.character, as.factor)
 return(Spoint) 
}

sphy <- function(data){
  ls <- list()
  data <- data %>% 
    filter(Phylum %in% c("Arthropoda", "Annelida", "Mollusca"))
  
  for(i in unique(data$Phylum)){
    data_i <- data %>% 
      filter(Phylum == i) %>% 
      group_by(Point, Site, Year, Season, Species) %>% 
      summarise(Abundance =  sum(Abundance)) %>%
      ungroup() %>% 
      spread(key = Species, value = Abundance, fill = 0) %>% 
      mutate(code = paste(Point, Year, sep = "_")) %>% 
      select(-Point,-Site,-Year,-Season) %>% 
      column_to_rownames("code")
    
    ls[[i]] <- as.data.frame(t(as.matrix(estimateR(data_i))))
    ls[[i]] <- ls[[i]] %>%
      rename_all(~ paste(., i, sep = "_")) %>% 
      tibble::rownames_to_column(., "code") %>% 
      mutate_if(is.character, as.factor)
  }
  
  ls <- ls %>%
    .[order(names(.))]
  
  sphy <- ls %>% reduce(left_join, by = "code")
  rownames(sphy) <- sphy$code
  return(sphy)
}

strait <- function(data){
  ls <- list()
  
  for(i in unique(data$Position)){
    data_i <- data %>% 
      filter(Position == i) %>% 
      group_by(Point, Site, Year, Season, Species) %>% 
      summarise(Abundance =  sum(Abundance)) %>%
      ungroup() %>% 
      spread(key = Species, value = Abundance, fill = 0) %>% 
      mutate(code = paste(Point, Year, sep = "_")) %>% 
      select(-Point,-Site,-Year,-Season) %>% 
      column_to_rownames("code")
    
    ls[[i]] <- as.data.frame(t(as.matrix(estimateR(data_i))))
    ls[[i]] <- ls[[i]] %>%
      rename_all(~ paste(., i, sep = "_")) %>% 
      tibble::rownames_to_column(., "code") %>% 
      mutate_if(is.character, as.factor)
  }
  
  ls <- ls %>%
    .[order(names(.))]
  
  strait <- ls %>% reduce(left_join, by = "code")
  rownames(strait) <- strait$code
  return(strait)
}

dens.num <- function(data){
  dens_num <- data %>% 
    rownames_to_column("Code") %>% 
    select(-Point:-Season) %>% 
    column_to_rownames("Code")
  return(dens_num)
}

dens.tot <- function(data){
  Abpoint <- data.frame("Fauna_Density" = round(rowSums(data))) %>% 
    tibble::rownames_to_column(., "code")
  return(Abpoint)
}

phylum.df <- function(data){
  phylumdf <- list()
  
  data <- data %>% 
    mutate_if(is.character, as.factor)
  for(i in unique(data$Phylum)){
    phylumdf[[i]] <- data %>% 
      filter(Phylum == i) %>%
      group_by(Point, Site, Year, Season, Species) %>% 
      summarise(Abundance =  sum(Abundance)) %>%
      ungroup() %>% 
      spread(key = Species, value = Abundance, fill = 0)
    
    
    phylumdf[[i]] <- phylumdf[[i]] %>%
      mutate(Code = paste(phylumdf[[i]]$Point, phylumdf[[i]]$Year, sep = "_")) %>% 
      select(-Point:-Season) %>% 
      column_to_rownames("Code")
    
    phylumdf[[i]] <- data.frame(round(rowSums(phylumdf[[i]]))) %>% 
      tibble::rownames_to_column(., "code")
    colnames(phylumdf[[i]]) <- c("code", i)
    

  }
  phylumdf <- phylumdf %>%
    .[order(names(.))]
  
  phylumdf <- phylumdf %>% reduce(left_join, by = "code")
  rownames(phylumdf) <- phylumdf$code
  phylumdf[is.na(phylumdf)] <- 0 
  return(phylumdf)
}

dens.phylum <- function(phylumdf, fauna){

  phylumdens <-  phylumdf %>% 
    mutate_if(is.numeric, funs(round(./(fauna$count*0.3))))  #change all the abundances to densities by dividing by the number of grabs available X the surface of a grab sample (0.3 m2)
  
  return(phylumdens)
  }
  


trait.df <- function(data){
  trait_dens <- list()
  
  data <- data %>% 
    mutate_if(is.character, as.factor)
  
  for(i in unique(data$Position)){
    trait_dens[[i]] <- data %>% 
      filter(Position == i) %>%
      group_by(Point, Site, Year, Season, Species) %>% 
      summarise(Abundance =  sum(Abundance)) %>%
      ungroup() %>% 
      spread(key = Species, value = Abundance, fill = 0)
    
    
    trait_dens[[i]] <- trait_dens[[i]] %>%
      mutate(Code = paste(trait_dens[[i]]$Point, trait_dens[[i]]$Year, sep = "_")) %>% 
      select(-Point:-Season) %>% 
      column_to_rownames("Code")
    
    trait_dens[[i]] <- data.frame(round(rowSums(trait_dens[[i]]))) %>% 
      tibble::rownames_to_column(., "code")
    colnames(trait_dens[[i]]) <- c("code", i)
  }
  trait_dens <- trait_dens  %>%
    .[order(names(.))] %>%
    reduce(left_join, by = "code")
  rownames(trait_dens) <- trait_dens$code
  trait_dens[is.na(trait_dens)] <- 0 
  
  return(trait_dens)
}


dens.trait <- function(traitdf, fauna){
  
  trait_dens <-  traitdf %>% 
    mutate_if(is.numeric, funs(round(./(fauna$count*0.3))))
  
  return(trait_dens)
}


dens.all <- function(x,y, z){
  densall <- y %>% 
    left_join(x) %>% 
    left_join(z) %>% 
    mutate_if(is.character, as.factor)

  return(densall)
}


alpha.div <- function(data, richall, densall){
  alphapoint <- data %>%
    rownames_to_column("code") %>%
    select(Point:Year, code) %>% 
    left_join(., richall) %>% 
    left_join(., densall) %>% 
    #Inverse Simpson diversity
    mutate(N2 = diversity(data[-c(1:4)], "inv")) %>% 
    mutate(E20 = N2/log(specnumber(data[-c(1:4)]))) %>%  #Simpson evenness (Hill's ratio) %>% 
    mutate(J = diversity(data[-c(1:4)])/log(specnumber(data[-c(1:4)]))) %>%  #pielou's evenness
    column_to_rownames("code")
  
  return(alphapoint)
}
