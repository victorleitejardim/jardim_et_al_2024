require(dplyr)
require(tidyr)
require(tidyverse)
require(magrittr)
require(stringr)

tidy.depth <- function(data) {
  bathy <- data %>% 
    filter(theme == "Bancs de Maërl") %>% 
    select(-c(theme, method_echant))%>% #remove useless info
    filter(site!= "Rhuys") %>%   #remove rows on Rhuys site, not analysed in this project
    mutate(point_name = gsub("Belle-Ile", "Belle-ile", point_name)) %>% 
    mutate(point_name = gsub("Rade de Brest - ", "", point_name)) %>%
    mutate(point_name = gsub("Rade de Brest-", "", point_name)) %>%
    mutate(point_name = gsub("è", "e", point_name)) %>% 
    mutate(point_name = gsub("é", "e", point_name)) %>% #remove special ch.
    mutate(site = gsub("Rade de Brest - ", "", .$site)) %>% 
    mutate(site = gsub("Baie de ", "", .$site)) %>% 
    rename(Point = point_name, Site = site) %>% 
    mutate(Site = factor(Site, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret", "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc"))) %>% 
    mutate(Point = factor(Point, levels = c("Belle-ile 1","Belle-ile 2", "Belle-ile 3", "Meaban 1", "Meaban 2", "Meaban 3", "Glenan 1", "Glenan 2", "Glenan 3", "Trevignon 1", "Trevignon 2", "Trevignon 3", "Camaret 1", "Camaret 2", "Camaret 3", "Keraliou 1", "Keraliou 2", "Keraliou 3", "Rozegat 1", "Rozegat 2", "Rozegat 3", "Molene 1", "Molene 2", "Molene 3", "Morlaix 1", "Morlaix 2", "Morlaix 3", "Saint-Brieuc 1", "Saint-Brieuc 2", "Saint-Brieuc 3"))) %>% 
    mutate(depth = -depth) %>% 
    as.data.frame() %>%
    set_rownames(paste( .$Point)) %>% 
    mutate_if(is.character, as.factor)
  
  bathy <- droplevels(bathy)
  return(bathy)
}

tidy.granulo <- function(data){
  granulo <-  data %>% 
    filter(site!= "Rhuys") %>%     #remove rows on Rhuys site, not analysed in this project, only spring data
    filter(annee %in% c(2007:2018)) %>% 
    mutate(point_name = gsub("Belle-Ile", "Belle-ile", point_name)) %>% 
    mutate(point_name = gsub("Rade de Brest - ", "", point_name)) %>%
    mutate(point_name = gsub("Rade de Brest-", "", point_name)) %>%
    mutate(point_name = gsub("è", "e", point_name)) %>% 
    mutate(point_name = gsub("é", "e", point_name)) %>% 
    mutate(site = gsub("Rade de Brest - ", "", site)) %>%
    mutate(site = gsub("Baie de ", "", site)) %>%
    rename(Point = point_name, Site = site, Year = annee, Season = saison) %>% 
    mutate(Site = factor(Site, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret", "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc"))) %>% 
    mutate(Point = factor(Point, levels = c("Belle-ile 1","Belle-ile 2", "Belle-ile 3", "Meaban 1", "Meaban 2", "Meaban 3", "Glenan 1", "Glenan 2", "Glenan 3", "Trevignon 1", "Trevignon 2", "Trevignon 3", "Camaret 1", "Camaret 2", "Camaret 3", "Keraliou 1", "Keraliou 2", "Keraliou 3", "Rozegat 1", "Rozegat 2", "Rozegat 3", "Molene 1", "Molene 2", "Molene 3", "Morlaix 1", "Morlaix 2", "Morlaix 3", "Saint-Brieuc 1", "Saint-Brieuc 2", "Saint-Brieuc 3"))) %>%
    select(mini, everything()) %>%
    as.data.frame() %>%
    set_rownames(paste( .$Point, .$Year, sep = "_")) %>% 
    rename(OM = pourcentage_MO) %>% 
    select(-boulder, -Sand, -`D50(um)`) %>% #remove highly correlated variables
    rename(Mud = mud, Grain_size = Mean.fw.um) %>% 
    mutate_if(is.character, as.factor)
  
  granulo <- droplevels(granulo)
  
  return(granulo)
}

tidy.hydro <- function(data){
  hydro <- data %>% 
    select(-c(theme, method_echant))%>% #remove useless info
    filter(site!= "Rhuys",saison == "Printemps") %>%   #remove rows on Rhuys site, not analysed in this project, only spring data
    filter(annee %in% c(2007:2018)) %>% 
    mutate(point_name = gsub("Belle-Ile", "Belle-ile", point_name)) %>% 
    mutate(point_name = gsub("Rade de Brest - ", "", point_name)) %>%
    mutate(point_name = gsub("Rade de Brest-", "", point_name)) %>%
    mutate(point_name = gsub("è", "e", point_name)) %>% 
    mutate(point_name = gsub("é", "e", point_name)) %>% #remove special ch.
    mutate(site = gsub("Rade de Brest - ", "", .$site)) %>% 
    mutate(site = gsub("Baie de ", "", .$site)) %>% 
    mutate(annee = as.numeric(levels(.$annee)[.$annee])) %>% 
    rename(Point = point_name, Site = site, Year = annee, Season = saison) %>% 
    mutate(Site = factor(Site, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret", "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc"))) %>% 
    mutate(Point = factor(Point, levels = c("Belle-ile 1","Belle-ile 2", "Belle-ile 3", "Meaban 1", "Meaban 2", "Meaban 3", "Glenan 1", "Glenan 2", "Glenan 3", "Trevignon 1", "Trevignon 2", "Trevignon 3", "Camaret 1", "Camaret 2", "Camaret 3", "Keraliou 1", "Keraliou 2", "Keraliou 3", "Rozegat 1", "Rozegat 2", "Rozegat 3", "Molene 1", "Molene 2", "Molene 3", "Morlaix 1", "Morlaix 2", "Morlaix 3", "Saint-Brieuc 1", "Saint-Brieuc 2", "Saint-Brieuc 3"))) %>% 
    select(Site:Year, bottomT_mean, bottomT_max, bottomT_sd, current_mean) %>% 
    mutate_if(is.character, as.factor) %>% 
    as.data.frame() %>% 
    set_rownames(paste(.$Point, .$Year, sep = "_"))
  
  hydro <- droplevels(hydro)
  colnames(hydro) <- str_remove(colnames(hydro), "bottom")
  colnames(hydro) <- str_replace(colnames(hydro), "current", "Current")
  return(hydro)
}

tidy.fetch <- function(data){
  fetch <- data %>% 
    mutate(site = recode(site, "Trevignon " = "Trevignon")) %>%
    mutate(point_name = paste(site, point, sep = " ")) %>% 
    mutate(point_name = gsub("Rade de Brest - ", "", point_name)) %>%
    mutate(point_name = gsub("Rade de Brest-", "", point_name)) %>%
    mutate(point_name = gsub("Baie de ", "", point_name)) %>%
    mutate(site = gsub("Rade de Brest - ", "", site)) %>% 
    mutate(site = gsub("Baie de ", "", site)) %>% 
    mutate(site = gsub("Rade de Brest - ", "", .$site)) %>% 
    mutate(site = gsub("Baie de ", "", .$site)) %>% 
    rename(Point = point_name, Site = site) %>% 
    mutate(Site = factor(Site, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret", "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc"))) %>% 
    mutate(Point = factor(Point, levels = c("Belle-ile 1","Belle-ile 2", "Belle-ile 3", "Meaban 1", "Meaban 2", "Meaban 3", "Glenan 1", "Glenan 2", "Glenan 3", "Trevignon 1", "Trevignon 2", "Trevignon 3", "Camaret 1", "Camaret 2", "Camaret 3", "Keraliou 1", "Keraliou 2", "Keraliou 3", "Rozegat 1", "Rozegat 2", "Rozegat 3", "Molene 1", "Molene 2", "Molene 3", "Morlaix 1", "Morlaix 2", "Morlaix 3", "Saint-Brieuc 1", "Saint-Brieuc 2", "Saint-Brieuc 3"))) %>%
    as.data.frame() %>% 
    set_rownames(paste( .$Point)) %>% 
    mutate_if(is.character, as.factor)
  
  fetch_max <- fetch %>% 
    gather(North:West, key = "Direction", value = Fetch) %>% 
    group_by(Point, Site, latitude, longitude) %>% 
    summarise(Fetch_max = max(Fetch)) %>% 
    left_join(fetch) %>% 
    select(Site, Point, North:Average, Fetch_max) %>% 
    as.data.frame() %>% 
    set_rownames(paste(.$Point))
  
  return(fetch_max)
}

tidy.traits <- function(data){
  traits <- data[!duplicated(data$Species),] %>% 
    mutate(Species =  trimws(Species)) %>% 
    select(-Species.y)

return(traits)
  
}

merge.traits <- function(fauna, classif, traits){
  fauna_traits <- fauna %>% 
  rename(species_init = Species) %>% 
    left_join(classif %>% select(-Species)) %>% 
    rename(Species = species_init) %>% 
    merge(traits, by = "worms_id") %>% 
    select(worms_id:Genus.x, Mobility:Position)
  
  for (col in 1:ncol(fauna_traits)){
    colnames(fauna_traits)[col] <-  sub(".x", "", colnames(fauna_traits)[col])
  }  
  
  fauna_traits <- fauna_traits %>% 
    mutate(Position = if_else(Species == "Sphaerodoridae spp.", "Endofaune", Position)) %>%  #Here I chose to change sphaerodoridae to infauna as the only other sphaerodoridae species in the study area is an infaunal species
    mutate(Position =  recode(Position, "Endofaune" = "Infauna", "Epifaune" = "Epifauna")) %>% 
    mutate(Site = factor(Site, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret", "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc"))) %>% 
    mutate(Point = factor(Point, levels = c("Belle-ile 1","Belle-ile 2", "Belle-ile 3", "Meaban 1", "Meaban 2", "Meaban 3", "Glenan 1", "Glenan 2", "Glenan 3", "Trevignon 1", "Trevignon 2", "Trevignon 3", "Camaret 1", "Camaret 2", "Camaret 3", "Keraliou 1", "Keraliou 2", "Keraliou 3", "Rozegat 1", "Rozegat 2", "Rozegat 3", "Molene 1", "Molene 2", "Molene 3", "Morlaix 1", "Morlaix 2", "Morlaix 3", "Saint-Brieuc 1", "Saint-Brieuc 2", "Saint-Brieuc 3")))
  
  return(fauna_traits)
}

fauna.matrix <- function(fauna_traits){
  maerl_fauna <- fauna_traits %>% 
    group_by(Point, Site, Year, Season, Species) %>% 
    summarise(Abundance =  sum(Abundance)) %>%
    ungroup() %>% 
    spread(key = Species, value = Abundance, fill = 0)
  
  
  samples_maerl <- fauna_traits %>% 
    distinct(Site, Year, Point, Season, Replicate) %>%
    group_by(Site, Point, Season, Year) %>%
    summarise(count = n()) %>%
    ungroup()
  
  maerl_fauna <- maerl_fauna %>% 
    left_join(samples_maerl) %>% 
    select(Point:Season, count, everything()) %>% 
    set_rownames(paste(.$Point, .$Year, sep = "_"))
  
  return(maerl_fauna)
}


fauna.dens <- function(data, fauna){
   maerl_dens <- data %>% 
    group_by(Point, Site, Year, Season, Species) %>% 
    summarise(Abundance =  sum(Abundance)) %>%
    ungroup() %>% 
    spread(key = Species, value = Abundance, fill = 0) %>% 
    mutate(Year = as.factor(Year)) %>% 
    mutate_if(is.numeric, funs(./(fauna$count*0.3))) %>% #change all the abundances to densities by dividing by the number of grabs available X the surface of a grab sample (0.3 m2)
    set_rownames(paste(.$Point, .$Year, sep = "_")) %>% 
    arrange(rownames(.))
}