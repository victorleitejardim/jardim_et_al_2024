require(dplyr)
require(ggplot2)
require(viridis)
relab.mean <- function(data, classif, taxlvl, value = 5){
  # relab <- data %>% 
  #   mutate(Year = as.ordered(Year)) %>% 
  #   gather(., -Point:-Season, key = "Species", value = Abundance) %>% 
  #   group_by(Site, Year) %>% 
  #   mutate(abtot = sum(Abundance)) %>% 
  #   ungroup() %>% 
  #   group_by(Point, Year, Species) %>% 
  #   mutate(abrel = Abundance/abtot * 100) %>% 
  #   ungroup() %>% 
  #   left_join(classif, by = c("Species" = "species_init"))
  
  relab <- data %>% 
    gather(., -Point:-Season, key = "Species", value = Abundance) %>% 
    group_by(Site, Species) %>% 
    summarise(Abundance = mean(Abundance)) %>% 
    ungroup() %>% 
    group_by(Site) %>% 
    mutate(abtot = sum(Abundance)) %>%
    ungroup() %>% 
    group_by(Site, Species) %>% 
    mutate(abrel = Abundance/abtot * 100) %>% 
    left_join(classif, by = c("Species" = "species_init"))
  #Sum the relative abundance of all species that add up to less then a % of the relative abundance to check in which habitats there's dominance
  relabfil_mean <- relab %>% 
    mutate(thresh = case_when(abrel <= value ~ "Others",
                              TRUE ~ eval(parse(text = paste(taxlvl))))) %>% 
    group_by(Site, thresh) %>% 
    summarise(abrel = sum(abrel)) %>% 
    ungroup() %>% 
    mutate_if(is.character, as.factor)
  
  relabfil_mean <- relabfil_mean %>% 
    mutate(thresh = forcats::fct_relevel(thresh, "Others", after = Inf))
  
  p <- ggplot(data = relabfil_mean, aes(x= Site, y = abrel, fill = thresh)) 
  p <- p + geom_col(width = .95)
  p <- p + labs(x = "Site", y = "Relative Abundance")
  p <- p + scale_fill_viridis_d(option = "mako", begin = 0.1, end = .9, direction = -1,  name = taxlvl)
  p <- p + ggfittext::geom_bar_text(aes(label = thresh), position = "stack", colour = "white", fontface = "bold", place = "centre", min.size = 0, reflow = TRUE,fullheight = TRUE, contrast = TRUE, padding.y = grid::unit(0.01, units = "mm"))
  p <- p +theme(axis.title.y = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.key.size = unit(.2, "cm"), legend.position = "none")
  plot(p)

}

relab.phyl <- function(data, classif, taxlvl, value = 5, phyl){
  
  relab <- data %>% 
    gather(., -Point:-Season, key = "Species", value = Abundance) %>%
    left_join(classif, by = c("Species" = "species_init")) %>% 
    filter(Phylum %in% c(phyl)) %>% 
    group_by(Site, Species) %>% 
    summarise(Abundance = mean(Abundance)) %>% 
    ungroup() %>% 
    group_by(Site) %>% 
    mutate(abtot = sum(Abundance)) %>%
    ungroup() %>% 
    group_by(Site, Species) %>% 
    mutate(abrel = Abundance/abtot * 100) %>% 
    ungroup() %>% 
    left_join(classif, by = c("Species" = "species_init"))

 
   ph_sel <- relab %>% 
    mutate(thresh = case_when(abrel <= value ~ "Others",
                              TRUE ~ eval(parse(text = paste(taxlvl))))) %>% 
    group_by(Site, thresh) %>% 
    summarise(abrel = sum(abrel)) %>% 
    ungroup()
  
  ph_sel <- ph_sel %>% 
    mutate(thresh = forcats::fct_relevel(thresh, "Others", after = Inf))
  
  p <- ggplot(data = ph_sel, aes(x= Site, y = abrel, fill = thresh)) 
  p <- p + geom_col(width = .95)
  p <- p + labs(x = "Site", y = "Relative Abundance", title = paste(phyl))
  p <- p + scale_fill_viridis_d(option = "mako", begin = 0.1, end = .9, direction = -1,  name = taxlvl)
  p <- p + ggfittext::geom_bar_text(aes(label = thresh), position = "stack", colour = "white", fontface = "bold", place = "centre", min.size = 0, reflow = TRUE,fullheight = TRUE, contrast = TRUE, padding.y = grid::unit(0.01, units = "mm"))
  p <- p +theme(axis.title.y = element_blank(), axis.title.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), legend.position = "none")
  plot(p)
  
}