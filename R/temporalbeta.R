bd.tot <- function(fauna_dens, comp_med){
  betalist <- list()
  for(i in unique(fauna_dens$Point)){
    betalist[[i]] <- fauna_dens %>% filter(Point == i) %>% select(-Point:-Season)
  }
  
  reslist <- list()
  for(i in names(betalist)){
    reslist[[i]] <- adespatial::beta.div.comp(betalist[[i]], coef = "J", quant = FALSE)$part
  }
  
  bdruzicka <- as.data.frame(reslist) %>%
    rownames_to_column %>% 
    gather(Point, value, -rowname) %>% 
    spread(rowname, value) %>% 
    mutate(Point =  str_replace(Point, "\\.", " ")) %>% 
    mutate(Point =  str_replace(Point, "\\.", " ")) %>% 
    mutate(Point = str_replace(Point, "Belle ile", "Belle-ile")) %>% 
    mutate(Point = str_replace(Point, "Saint Brieuc", "Saint-Brieuc")) %>% 
    mutate(Point = as.factor(Point)) %>%
    mutate(Point = factor(Point, levels = c("Belle-ile 1","Belle-ile 2", "Belle-ile 3", "Meaban 1", "Meaban 2", "Meaban 3", "Glenan 1", "Glenan 2", "Glenan 3", "Trevignon 1", "Trevignon 2", "Trevignon 3", "Camaret 1", "Camaret 2", "Camaret 3", "Keraliou 1", "Keraliou 2", "Keraliou 3", "Rozegat 1", "Rozegat 2", "Rozegat 3", "Molene 1", "Molene 2", "Molene 3", "Morlaix 1", "Morlaix 2", "Morlaix 3", "Saint-Brieuc 1", "Saint-Brieuc 2", "Saint-Brieuc 3"))) %>% 
    left_join(comp_med %>% select(Point, Site, PC1_score, PC2_score)) %>% 
    mutate(Site = factor(Site, levels = c("Belle-ile", "Meaban", "Glenan", "Trevignon", "Camaret", "Keraliou", "Rozegat", "Molene", "Morlaix", "Saint-Brieuc"))) %>% 
    select(Point, Site, everything()) %>% 
    arrange(Point)

}

bd.plot <- function(bdtot, pal){
  bdplot <- bdtot %>% 
    mutate(Rf = 1-BDtotal) %>% 
    select(-`Repl/BDtotal`, -`RichDif/BDtotal`, -PC1_score, -PC2_score) %>% 
    gather(.,"Component", "Value", Repl:Rf) %>% 
    mutate(Component = factor(Component, levels = c("Repl", "Rf", "RichDif"))) %>% 
    arrange(Site, Component) %>%
    mutate(Code = factor(paste(.$Site, .$Component, sep = "_"), levels = rev(do.call(paste, c(tidyr::expand_grid(levels(.$Site), levels(.$Component), sep = '_')))))) %>% 
    mutate(Point = as.factor(Point)) %>% 
    mutate(Point = factor(Point, levels = rev(levels(Point))))
  
  bdplotmean <- bdplot %>%
    group_by(Site) %>%
    summarise_if(is.numeric, mean)%>%
    rename(mean = BDtotal) %>%
    select(-Value)
  
  bdplot <- bdplot %>%
    left_join(bdplotmean)
  
  pal3 <- c(rbind(pal, paste(pal, "4D", sep = ""), rep("transparent", 10)))
  
  (bdp2 <- ggplot(bdplot %>% mutate(Component = factor(Component, levels = c("Repl", "RichDif", "Rf"))) %>% 
                    arrange(Site, Component) %>%
                    mutate(Code = factor(paste(.$Site, .$Component, sep = "_"), levels = rev(do.call(paste, c(tidyr::expand_grid(levels(.$Site), levels(.$Component), sep = '_')))))), aes(x = Value, y  = reorder(Point, -mean), fill = Code)) + 
      geom_bar(position="fill", stat="identity") + 
      scale_fill_manual(values = rev(pal3))+
      labs(x = "BDtotal")+
      scale_x_continuous(n.breaks = 6)+
      theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_text(face = "bold", colour = "gray35", size = 22), legend.text = element_text(size = 20, colour = "gray35"), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text.y = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =22, face = "bold"), axis.ticks.y = element_blank(), axis.title.y = element_blank())+
      coord_cartesian(xlim = c(0,.6), expand = FALSE))
  p <- ggplot(bdplot %>% filter(Component != "Rf"), aes(x = Value, y  = Point, fill = Component)) + 
    geom_bar(position="fill", stat="identity") + 
    scale_fill_manual(values = c("#000000", "#0000004D"))+
    theme(legend.title = element_text(face = "bold", colour = "gray35", size = 22), legend.text = element_text(size = 20, colour = "gray35"), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =22, face = "bold"), legend.direction = "horizontal")+
    coord_cartesian(xlim = c(0,1), expand = FALSE)
  legp <- get_legend(p)
  
  p2 <- ggplot(bdplot %>% filter(Component != "Rf"), aes(x = Value, y  = reorder(Point, -mean), fill = Site)) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(values = c("#d44e65","#EE8866",  "#858c64", "#44AA99", "#4477AA", "#8ECDDE", "#FFAABB", "#7b538c", "#e8bb5a", "#80898f"), breaks = c("Rozegat", "Keraliou", "Trevignon", "Glenan", "Belle-ile", "Meaban", "Molene", "Morlaix", "Camaret", "Saint-Brieuc"))+
    theme(panel.grid = element_blank(), legend.title = element_text(face = "bold", colour = "gray35", size = 22), legend.text = element_text(size = 20, colour = "gray35"), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =22, face = "bold"))+
    coord_cartesian(xlim = c(0,1), expand = FALSE)
  legp2 <- get_legend(p2)
  design2 <- c(patchwork::area(2, 1, 10, 3), patchwork::area(1, 1, 1, 3), patchwork::area(2, 3, 10, 3))
  bdp2 + legp + legp2 + patchwork::plot_layout(design = design2)
}
