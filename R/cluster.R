clustdend <- function(data, pal){
  datanum <- data %>% 
    rownames_to_column("code") %>% 
    select_if(Negate(is.factor)) %>%
    column_to_rownames("code") %>% 
    mutate_all(scale)
  
  model <- hclust(dist(datanum), "ward.D2")

  dhc <- as.dendrogram(model)
  
  dendro <- ggdendro::dendro_data(dhc, type = "triangle")
  dendro[["labels"]] <- dendro[["labels"]] %>% 
    mutate_if(is.character, as.factor) %>% 
    left_join(data, by = c("label" = "Site")) %>% 
    mutate(label = factor(label, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret",  "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc")))
  
  p <- ggplot() +
    geom_segment(data = ggdendro::segment(dendro), aes(x = x, y = y, xend = xend, yend = yend), linewidth = 1.2, colour = "gray25")+
    geom_text(data = dendro[["labels"]], aes(x = x, y = y, label = label, colour = label, hjust = 0), size = 20/.pt, nudge_y = .2)+
    coord_flip() +
    scale_y_reverse(expand=c(0.2, 0))+
    scale_color_manual(values = pal)+
    theme_void()+
    theme(legend.position = "none")
  p
}

clustfauna <- function(data, pal){
  meta <- data %>% 
    rownames_to_column("Site")
  
  model <- hclust(dist(data), "ward.D2")
  
  dhc <- as.dendrogram(model)
  
  dendro <- ggdendro::dendro_data(dhc, type = "triangle")
  dendro[["labels"]] <- dendro[["labels"]] %>% 
    mutate_if(is.character, as.factor) %>% 
    left_join(meta, by = c("label" = "Site")) %>% 
    mutate(label = factor(label, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret",  "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc")))
  
  p <- ggplot() +
    geom_segment(data = ggdendro::segment(dendro), aes(x = x, y = y, xend = xend, yend = yend), linewidth = 1.2, colour = "gray25")+
    geom_text(data = dendro[["labels"]], aes(x = x, y = y, label = label, colour = label, hjust = 0), size = 20/.pt, nudge_y = .2)+
    coord_flip() +
    scale_y_reverse(expand=c(0.2, 0))+
    scale_colour_manual(values = pal)+
    theme_void()+
    theme(legend.position = "none")
  p
}
