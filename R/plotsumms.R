p.summs <- function(dmodsel, dannsel, dartsel, dmolsel, dmodrand, dannrand, dartrand, dmolrand, pal = pal, coefs = NULL, model.names = NULL){
  p1 <- jtools::plot_summs(dmodsel, dannsel, dartsel, dmolsel, colors = pal, robust = FALSE, model.names = model.names,
                          coefs = coefs,
                           point.size = 4, inner_ci_level =  .9, scale = TRUE, transform.response = TRUE)+
    ggtitle("LMs")+
    theme(plot.title = element_text(hjust = .5), legend.position = "right", legend.title = element_text(face = "bold", size = 22), legend.text = element_text(size = 20), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =20, face = "bold"))+
    guides(colour = guide_legend(title.position = "top"))
  
  p2 <- jtools::plot_summs(dmodrand, dannrand, dartrand, dmolrand, model.names = model.names, coefs = coefs, colors = pal, robust = FALSE, 
                           point.size = 4, inner_ci_level =  .9, scale = TRUE, transform.response = TRUE)+
    ggtitle("LMMs")+
    theme(plot.title = element_text(hjust = .5), legend.position = "right", legend.title = element_text(face = "bold", size = 22), legend.text = element_text(size = 20), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =20, face = "bold"), axis.text.y = element_text(colour = "transparent"))+
    guides(colour = guide_legend(title.position = "top"))
  
  yaxis <- cowplot::get_plot_component(p1, "axis-l")
  p1 <- p1 + theme(axis.text.y=element_blank(),
                           axis.ticks.y=element_blank()
                           )
  p2 <- p2 + theme(axis.text.y=element_blank(),
                           axis.ticks.y=element_blank()
  )
  require(patchwork)
  layout <- c(
    area(2, 1, 59, 1),
    area(1, 2, 60, 5),
    area(1, 6, 60, 9),
    area(2, 10, 59, 11)
    )
  
  wrap_plots(yaxis, heights =  1)+p1+p2+guide_area()+plot_layout(guides = "collect", design = layout, heights = 1)
  #ggarrange(p1, p2, common.legend = TRUE, ncol = 2, legend = "right")
}

alpha.summs <- function(mod1, mod2, mod3, mod0, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, pal = pal, coefs = NULL, model.names = NULL, data){
  mod0 <- lm(S.obs ~ Gravel + Current_mean, data = data)
  p0 <- jtools::plot_summs(mod1,
                           mod2,
                           mod3,
                           mod0,
                           colors = c(pal[c(5:7)], 'transparent'),
                           robust = FALSE,
                           model.names = c(
                             "S ~ Complexity",
                             "S ~ Complexity + Environment",
                             "S ~ Complexity + Environment + (1|Site)",
                             ''
                           ),
                           coefs = coefs,
                           point.size = 4,
                           inner_ci_level =  .9,
                           scale = TRUE) +
    theme(
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 22),
      legend.text = element_text(size = 20),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 20, face = "bold")
    ) +
    guides(colour = guide_legend(title.position = "top"))
  
  p1 <- jtools::plot_summs(mod4,
                           mod5,
                           mod6,
                           mod7,
                           colors = pal[c(9, 1, 3:4)], 
                           robust = FALSE, 
                           model.names = model.names,
                           coefs = coefs,
                           point.size = 4,
                           inner_ci_level =  .9,
                           scale = TRUE,
                           transform.response = TRUE)+
    ggtitle("LMs")+
    theme(
      plot.title = element_text(hjust = .5), 
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 22),
      legend.text = element_text(size = 20),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(), 
      axis.text = element_text(size = 20),
      axis.title = element_text(size =20, face = "bold")
    ) +
    guides(colour = guide_legend(title.position = "top"))
  
  p2 <-
    jtools::plot_summs(
      mod8,
      mod9,
      mod10,
      mod11,
      model.names = model.names,
      coefs = coefs,
      colors = pal[c(9, 1, 3:4)],
      robust = FALSE,
      point.size = 4,
      inner_ci_level =  .9,
      scale = TRUE,
      transform.response = TRUE) +
    ggtitle("LMMs") +
    theme(
      plot.title = element_text(hjust = .5),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 22),
      legend.text = element_text(size = 20),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 20, face = "bold"),
      axis.text.y = element_text(colour = "transparent")
    ) +
    guides(colour = guide_legend(title.position = "top"))
  
  yaxis <- cowplot::get_plot_component(p1, "axis-l")
  p0 <- p0 + theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
  p1 <- p1 + theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
  p2 <- p2 + theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
  require(patchwork)
  layout <- c(
    area(2, 1, 59, 1),
    area(1, 2, 60, 5),
    area(1, 6, 60, 9),
    area(1, 10, 60, 13),
    area(2, 16, 59, 16)
  )
  
  wrap_plots(yaxis, heights =  1)+p0+p1+p2+guide_area()+plot_layout(guides = "collect", design = layout)
  #ggarrange(p1, p2, common.legend = TRUE, ncol = 2, legend = "right")
}

p.summsbd <- function(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10, mod11, mod12, pal, coefs = NULL, model.names = NULL){
  (ps1 <- jtools::plot_summs(mod1, mod2, mod3, mod4, model.names = model.names, colors = pal, robust = FALSE, coefs = coefs, point.size = 4, inner_ci_level =  .9, scale = TRUE, transform.response = TRUE)+
     ggtitle("BDTotal")+
     theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold", size = 22), axis.text.x = element_text(angle = 45),legend.text = element_text(size = 20), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =20, face = "bold"))+
     guides(colour = guide_legend(title.position = "top")))
  
  (ps2 <-jtools::plot_summs(mod5, mod6, mod7, mod8, model.names = model.names, colors = pal, robust = FALSE, coefs = coefs, point.size = 4 , inner_ci_level =  .9, scale = TRUE, transform.response = TRUE)+
      ggtitle("Replacement")+
      theme(plot.title = element_text(hjust = .5), axis.text.y = element_text(colour = "transparent"), axis.text.x = element_text(angle = 45), legend.title = element_text(face = "bold", colour = "gray35", size = 22), legend.text = element_text(size = 20, colour = "gray35"), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =20, face = "bold")))
  
  (ps3 <- jtools::plot_summs(mod9, mod10, mod11, mod12, model.names = model.names, colors = pal, robust = FALSE, coefs = coefs, point.size = 4 , inner_ci_level =  .9, scale = TRUE, transform.response = TRUE)+
      ggtitle("Richness Difference")+
      theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold", size = 22), axis.text.y = element_text(colour = "transparent"),  axis.text.x = element_text(angle = 45), legend.text = element_text(size = 20, colour = "gray35"), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =20, face = "bold")))
  
  ggarrange(ps1, ps2, ps3, common.legend = TRUE, ncol = 3, legend = "top")
}
