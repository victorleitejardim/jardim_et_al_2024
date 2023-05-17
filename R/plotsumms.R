p.summs <- function(dmodsel, dannsel, dartsel, dmolsel, dmodrand, dannrand, dartrand, dmolrand, pal = pal, coefs = NULL, model.names = NULL){
  p1 <- jtools::plot_summs(dmodsel, dannsel, dartsel, dmolsel, colors = pal, robust = FALSE, model.names = model.names,
                          coefs = coefs,
                           point.size = 4, inner_ci_level =  .9, scale = TRUE, transform.response = TRUE)+
    ggtitle("LMs")+
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold", size = 22), legend.text = element_text(size = 20), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =20, face = "bold"))+
    guides(colour = guide_legend(title.position = "top"))
  
  p2 <- jtools::plot_summs(dmodrand, dannrand, dartrand, dmolrand, model.names = model.names, coefs = coefs, colors = pal, robust = FALSE, 
                           point.size = 4, inner_ci_level =  .9, scale = TRUE, transform.response = TRUE)+
    ggtitle("LMMs")+
    theme(plot.title = element_text(hjust = .5), legend.title = element_text(face = "bold", size = 22), legend.text = element_text(size = 20), legend.background = element_blank(), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20), axis.title = element_text(size =20, face = "bold"), axis.text.y = element_text(colour = "transparent"))+
    guides(colour = guide_legend(title.position = "top"))
  
  ggarrange(p1, p2, common.legend = TRUE, ncol = 2, legend = "top")
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
