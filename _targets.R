library(targets)

tar_option_set(packages = c("dplyr", "ggplot2", "ggvegan", "vegan", "knitr"))

file.sources <- list.files(
  here::here("R"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

sapply(file.sources, source, .GlobalEnv)

list(
  
  #- complexity ----
  tar_target(compraw, here::here("data", "complexity.Rdata"), format = "file"), 
  tar_target(complexity, load.data(compraw)),
  tar_target(comp, sphericity(complexity)),  #calculate sphericity metrics
  tar_target(comp_med, comp.med(comp)), #median values at the point level
  tar_target(comp_site, comp.site(comp)), #median values at the site level
  tar_target(bccomp, bc.comp(comp)), #boxcox transformation
  tar_target(comp_num, comp.num(bccomp)), #only numeric
  tar_target(comp_sel, bccomp %>% select(-DR1,-DR2, -I, -S)),
  tar_target(num_sel, comp.num(comp_sel)),
  tar_target(bcmed, bc.compmed(comp_med)), #boxcox transformation for median values
  tar_target(med_num, compmed.num(bcmed)), #only numeric
  tar_target(med_sel, bcmed %>% select(-DR1,-DR2, -I, -S)),
  tar_target(bcsite, bc.compsite(comp_site)),
  tar_target(site_sel, bcsite %>% select(-DR1,-DR2, -I, -S) %>%  set_rownames(paste(.$Site))),
  tar_target(med_numsel, compmed.num(med_sel)),
  tar_target(pairscomp, GGally::ggpairs(bccomp %>% select_if(is.numeric) %>% select(-Sample))),
    #-- pca ----
  tar_target(pcatotal, rda(comp_num, scale = TRUE)),
  tar_target(sptot, screep(pcatotal)),
  tar_target(
    pcatot,
    bg_pca(
      pca = pcatotal,
      metadata = bccomp,
      main.group = "Site",
      scale.fill = pal,
      scale.colour = pal,
      goodness.thresh = 0.0,
      add.centroids = TRUE,
      nudge.x = c(.1, .2, -.1, -.22, -.22, 0, -.22, -0.14, 0, 0),
      nudge.y = c(-.06, .08,-.06, .005, 0,-.08, .005, .005, -.06,-.06),
      stat1 = "ellipse",
      conf.level = .8,
      ysites = c(-.75, .75),
      xsites = c(-.75, .75),
      ysp = c(-3, 2),
      xsp = c(-3, 2),
      font.size = 20 / .pt,
      axis.size = 22,
      axis.text = 20,
      c.size = 4,
      reverse.x = TRUE
    )
  ), 
  
  tar_target(pcatotsel, rda(num_sel, scale = TRUE)),
  tar_target(bccomp2, extract.pc(bccomp, pcatotsel)), 
  tar_target(sptotsel, screep(pcatotsel)),
  tar_target(
    pcasel,
    bg_pca(
      pca = pcatotsel,
      metadata = bccomp,
      main.group = "Site",
      scale.fill = pal,
      scale.colour = pal,
      goodness.thresh = 0.0,
      add.centroids = TRUE,
      stat1 = "ellipse",
      conf.level = .8,
      ysites = c(-.56, .56),
      xsites = c(-.56, .56),
      ysp = c(-1.9, 1.5),
      xsp = c(-1.9, 1.5),
      font.size = 20 / .pt,
      axis.size = 22,
      axis.text = 20,
      c.size = 4
    )
  ),
  
  tar_target(pcamedsel, rda(med_numsel, scale = TRUE)),
  tar_target(spmedsel, screep(pcamedsel)),
  tar_target(
    pcamed,
    bg_pca(
      pca = pcamedsel,
      metadata = bcmed,
      main.group = "Site",
      scale.fill = pal,
      scale.colour = pal,
      goodness.thresh = 0.0,
      add.centroids = TRUE,
      stat1 = "chull",
      conf.level = .8,
      ysites = c(-.75, .75),
      xsites = c(-.75, .75),
      ysp = c(-1.7, 1.7),
      xsp = c(-1.7, 1.7),
      axis.size = 16,
      axis.text = 20,
      c.size = 3,
      nudge.x = c(0, 0, 0, 0, .08, .16, .12, 0, 0, 0),
      nudge.y = c(.11,.11,-.1,-.1,-.09,-.1, .12, .1, -.1, -.24),
      font.size = 11 / .pt,
      ext.plot.scale = 2.5,
      reverse.y = FALSE
    )
  ),
  tar_target(
    bccomp_cent,
    bccomp2 %>%
      group_by(Mini, Site, Point) %>%
      summarise(PC1_c = mean(PC1_score), PC2_c = mean(PC2_score)) %>%
      ungroup()
  ), 
  tar_target(med_sel2, extract.pc(med_sel, pcamedsel) %>% left_join(bccomp_cent)), 
  tar_target(corcomp, cor.plot(med_sel2, pal)),
  tar_target(cortestcomp1, cor.test(med_sel2$PC1_c, med_sel2$PC1_score)),
  tar_target(cortestcomp2, cor.test(med_sel2$PC2_c, med_sel2$PC2_score)),
  tar_target(rv, FactoMineR::coeffRV(med_sel2 %>% select(PC1_c, PC2_c), med_sel2 %>% select(PC1_score, PC2_score))),
  
    #-- cluster----
  tar_target(clustcomp, clustdend(site_sel, pal)),
  #- fauna ----
  tar_target(faunaraw, here::here("data","maerl_fauna.Rdata")),
  tar_target(faunadata, load.data(faunaraw)),
  ##taxonomic classification
  tar_target(classifdata, here::here("data","classif.Rdata"), format = "file"),
  tar_target(classif, load.data(classifdata)),
  ##traits
  tar_target(traitsdata, here::here("data", "traits_adeline.Rdata"), format = "file"),
  tar_target(traitsraw, load.data(traitsdata)),
  tar_target(traits, tidy.traits(traitsraw)),
  ##merge
  tar_target(faunaclass, merge.traits(faunadata, classif, traits)),
  
  ##species by sites matrix
  tar_target(fauna, fauna.matrix(faunaclass)),
  tar_target(fauna_dens, fauna.dens(faunaclass, fauna)),
  tar_target(inf, fauna.compart(faunaclass, "Infauna")),
  tar_target(epi, fauna.compart(faunaclass, "Epifauna")),
  tar_target(int, fauna.compart(faunaclass, "Interstice")),
  tar_target(art, fauna.phyl(faunaclass, "Arthropoda")),
  tar_target(ann, fauna.phyl(faunaclass, "Annelida")),
  tar_target(mol, fauna.phyl(faunaclass, "Mollusca")),
  
  #relative abundance
  tar_target(relab_phy, relab.mean(fauna_dens, classif, taxlvl = "Phylum", value = .5)),
  tar_target(relab_ord, relab.mean(fauna_dens, classif, taxlvl = "Order")),
  tar_target(relab_arth, relab.phyl(fauna_dens, classif, taxlvl = "Family", phyl = "Arthropoda", value = 8)),
  tar_target(relab_mol, relab.phyl(fauna_dens, classif, taxlvl = "Family", phyl = "Mollusca", value = 8)),
  tar_target(relab_ann, relab.phyl(fauna_dens, classif, taxlvl = "Family", phyl = "Annelida", value = 8)),
  
  #alpha diversity
  tar_target(fauna_num, fauna.num(fauna)), # numeric-only raw fauna data
  tar_target(rich, stot(fauna_num)), # get several alpha div metrics
  tar_target(dens_num, dens.num(fauna_dens)), #numeric-only density data
  tar_target(denstot, dens.tot(dens_num)), # get total density of macrofauna
  tar_target(phydf, phylum.df(faunaclass)),
  tar_target(phylumdens, dens.phylum(phydf, fauna)),
  tar_target(traitdf, trait.df(faunaclass)),
  tar_target(traitdens, dens.trait(traitdf, fauna)),
  tar_target(richphy, sphy(faunaclass)),
  tar_target(richtrait, strait(faunaclass)),
  tar_target(densall, dens.all(phylumdens, denstot, traitdens)),
  tar_target(richall, dens.all(richphy, rich, richtrait)),
  tar_target(alphadiv, alpha.div(fauna, richall, densall)),
  
  #- physical environmental variables ----
  ##bathymetry
  tar_target(bathydata, here::here("data", "depth.Rdata"), format = "file"),
  tar_target(bathyraw, load.data(file = bathydata)),
  tar_target(bathy, tidy.depth(bathyraw)),
  ##fetch
  tar_target(fetchdata, here::here("data", "fetchDCE.Rdata"), format = "file"),
  tar_target(fetchraw, load.data(file = fetchdata)),
  tar_target(fetch, tidy.fetch(fetchraw)),
  ##granulometry
  tar_target(granulodata, here::here("data", "Granulo_imputed_point_21_04_2021.Rdata"), format = "file"),
  tar_target(granuloraw, load.data(file = granulodata)),
  tar_target(granulo, tidy.granulo(granuloraw)),
  ##hydrology
  tar_target(hydrodata, here::here("data", "MA_hydrology_point.Rdata"), format = "file"),
  tar_target(hydroraw, load.data(file = hydrodata)),
  tar_target(hydro, tidy.hydro(hydroraw)),
  ##environment tables
  tar_target(env, envtable(granulo, hydro, bathy, fetch)),
  tar_target(env_mean, envmean(env)),
  
  #- alpha diversity ~ complexity + env ----
  tar_target(alphamod, alpha.mod(alphadiv, y = env, z = med_sel2)),
  tar_target(correl, GGally::ggpairs(data = alphamod %>% select(PC1_score, PC2_score, Current_mean, T_mean, T_sd, Depth, Fetch_max, Mud, Gravel, OM))),
    #-- richness ----
  tar_target(modfixrich1, lm(data = alphamod, formula = S.obs ~ PC1_score)),
  tar_target(modfixrichpoly1, lm(data = alphamod, formula = S.obs ~ poly(PC1_score,2))),
  tar_target(totrichpc1, ggplot(data = alphamod, aes(x = PC1_score,
                                                  y = S.obs,
                                                  colour = Site))+
               scale_color_manual(values = alpha(pal, .4)) +
               scale_fill_manual(values = pal) +
               geom_violin(aes(group = Point,
                               fill = Site),
                           width = .08,
                           alpha = .5,
                           # trim = FALSE,
                           draw_quantiles = .5)+
               geom_point(alpha = .6, 
                          size = 2.5)+
               geom_smooth(colour = "gray45", 
                           method = lm
               )+
               ylab("Species Richness")+
               xlab("Nodule Complexity (PC1)")+
               scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125))+
               scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1))+
               theme(legend.title = element_text(face = "bold",
                                                 size = 22),
                     legend.text = element_text(size = 20),
                     legend.background = element_blank(),
                     legend.box.background = element_blank(),
                     legend.key = element_blank(),
                     axis.text = element_text(size = 20),
                     axis.title = element_text(size =22,
                                               face = "bold")) +
               guides(fill = guide_legend(title.position = "top"))),
  tar_target(modfixrich2, lm(data = alphamod, formula = S.obs ~ PC2_score)),
  tar_target(modfixrichpoly2, lm(data = alphamod, formula = S.obs ~ poly(PC2_score,2))),
  tar_target(modfixrich12, lm(data = alphamod, formula = S.obs ~ PC1_score * PC2_score)),
  tar_target(modfixrichpoly12, lm(data = alphamod, formula = S.obs ~ poly(PC1_score,2) * poly(PC2_score,2))),
  tar_target(modranrich12, lme4::lmer(data = alphamod, formula = S.obs ~ PC1_score * PC2_score + (1|Site))),
  tar_target(richselR2, adespatial::forward.sel(alphamod %>% select(S.obs), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score, Year))),
  tar_target(richselAIC, f.sel(alphamod %>% select(S.obs, Year, Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modfrsel, lm(formula = S.obs ~ PC1_score * PC2_score + Depth + Fetch_max + T_mean + OM + Year, data = alphamod)),
  tar_target(modrrsel, lme4::lmer(formula = S.obs ~ PC1_score * PC2_score + Depth + Fetch_max + T_mean + OM + Year + (1|Site), data = alphamod)),
  tar_target(
    psrich,
    jtools::plot_summs(
      modfixrich12,
      modfrsel,
      modrrsel,
      colors = pal[c(5:7)],
      robust = FALSE,
      model.names = c(
        "S ~ Complexity",
        "S ~ Complexity + Environment",
        "S ~ Complexity + Environment + (1|Site)"
      ),
      coefs = c(
        "Nodule complexity (PC1)" = "PC1_score", 
        "Bed complexity (PC2)" = "PC2_score",
        "PC1:PC2" = "PC1_score:PC2_score",
        "Depth" =  "Depth",
        "Exposure" = "Fetch_max",
        "Organic matter %" = "OM",
        "Mean current velocity" = "Currrent_mean",
        "Mean bottom temperature" = "T_mean",
        "Year" =  "Year"
      ),
      point.size = 5,
      inner_ci_level =  .9,
      scale = TRUE) +
        theme(
          text = element_text(size = 30),
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
    ),
  tar_target(figure2a, s_surfplot(modrrsel, pal, "Species richness <br> (marginal predictions)")),
  
      #--- by phylum ----
  tar_target(modfrann, lm(data = alphamod, formula = S.obs_Annelida ~ PC1_score * PC2_score)),
  tar_target(modrrann, lme4::lmer(data = alphamod, formula = S.obs_Annelida ~ PC1_score * PC2_score + (1|Site))),
  
  tar_target(modfrart, lm(data = alphamod, formula = S.obs_Arthropoda ~ PC1_score * PC2_score)),
  tar_target(modrrart, lme4::lmer(data = alphamod, formula = S.obs_Arthropoda ~ PC1_score * PC2_score + (1|Site))),
  
  tar_target(modfrmol, lm(data = alphamod, formula = S.obs_Mollusca ~ PC1_score * PC2_score)),
  tar_target(modrrmol, lme4::lmer(data = alphamod, formula = S.obs_Mollusca ~ PC1_score * PC2_score + (1|Site))),
  
  tar_target(annrich, s_surfplot(modrrann, pal, "Annelid richness <br> (marginal predictions)")),
  tar_target(artrich, s_surfplot(modrrart, pal, "Arthropod richness <br> (marginal predictions)")),
  tar_target(molrich, s_surfplot(modrrmol, pal, "Mollusc richness <br> (marginal predictions)")),
      #--- by trait ----
  tar_target(modfrepi, lm(data = alphamod, formula = S.obs_Epifauna ~ PC1_score * PC2_score)),
  tar_target(modrrepi, lme4::lmer(data = alphamod, formula = S.obs_Epifauna ~ PC1_score * PC2_score + (1|Site))),
  
  tar_target(modfrinf, lm(data = alphamod, formula = S.obs_Infauna ~ PC1_score * PC2_score)),
  tar_target(modrrinf, lme4::lmer(data = alphamod, formula = S.obs_Infauna ~ PC1_score * PC2_score + (1|Site))),
  
  tar_target(modfrint, lm(data = alphamod, formula = S.obs_Interstice ~ PC1_score * PC2_score)),
  tar_target(modrrint, lme4::lmer(data = alphamod, formula = S.obs_Interstice ~ PC1_score * PC2_score + (1|Site))),
  
  tar_target(epirich, s_surfplot(modrrepi, pal, "Epifauna richness <br> (marginal predictions)")),
  tar_target(infrich, s_surfplot(modrrinf, pal, "Infauna richness <br> (marginal predictions)")),
  tar_target(intrich, s_surfplot(modrrint, pal, "Interstitial fauna richness <br> (marginal predictions)")),
  
    #-- Density ----
  tar_target(modfixdens1, lm(data = alphamod, formula = log(Fauna_Density) ~ PC1_score)),
  tar_target(modfixdens2, lm(data = alphamod, formula = log(Fauna_Density) ~ PC2_score)),
  tar_target(modfixdens12, lm(data = alphamod, formula = log(Fauna_Density) ~ PC1_score * PC2_score)),
  tar_target(modrandens12, lme4::lmer(data = alphamod, formula = log(Fauna_Density) ~ PC1_score * PC2_score + (1|Site))),
  tar_target(densselR2, adespatial::forward.sel(alphamod %>% select(Fauna_Density), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modfdsel, lm(formula = log(Fauna_Density) ~ PC1_score * PC2_score + Current_mean + Gravel + Depth + Fetch_max + T_mean + OM + Year, data = alphamod)),
  tar_target(modrdsel, lme4::lmer(formula = log(Fauna_Density) ~ PC1_score * PC2_score + Current_mean + Gravel + Depth + Fetch_max + T_mean + OM + Year + (1|Site), data = alphamod)),
  tar_target(surfdens, dens_surfplot(modrdsel, pal, "Total density")),
 
      #--- by phylum ----
  tar_target(anndensselR2, adespatial::forward.sel(alphamod %>% select(Annelida), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modfdann, lm(data = alphamod, formula = log(Annelida) ~ PC1_score * PC2_score + Fetch_max  + Depth + Mud + T_mean + OM + Year)),
  tar_target(modrdann, lme4::lmer(data = alphamod, formula = log(Annelida) ~ PC1_score * PC2_score + Fetch_max  + Depth + T_mean + Year + (1|Site))),
  tar_target(surfann, dens_surfplot(modrdann, pal, "Annelid density")),
  
  tar_target(artdensselR2, adespatial::forward.sel(alphamod %>% select(Arthropoda), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modfdart, lm(data = alphamod, formula = log(Arthropoda) ~ PC1_score * PC2_score + Current_mean + Fetch_max + Gravel + OM + Year)),
  tar_target(modrdart, lme4::lmer(data = alphamod, formula = log(Arthropoda) ~ PC1_score * PC2_score + Current_mean + Fetch_max + Gravel + OM + Year + (1|Site))),
  tar_target(surfart, dens_surfplot(modrdart, pal, "Arthropod density")),
  tar_target(moldensselR2, adespatial::forward.sel(alphamod %>% select(Mollusca), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modfdmol, lm(data = alphamod, formula = log(Mollusca) ~ PC1_score * PC2_score + Gravel  + OM + T_mean + Depth + Year)),
  tar_target(modrdmol, lme4::lmer(data = alphamod, formula = log(Mollusca) ~ PC1_score * PC2_score + Gravel  + OM + T_mean + Depth + Year + (1|Site))),
  tar_target(surfmol, dens_surfplot(modrdmol, pal, "Mollusc density")),
  tar_target(
    psdens,
    alpha.summs(
      mod1 = modfixrich12,
      mod2 = modfrsel,
      mod3 = modrrsel,
      mod4 = modfdsel,
      mod5 = modfdann,
      mod6 = modfdart,
      mod7 = modfdmol,
      mod8 = modrdsel,
      mod9 = modrdann,
      mod10 = modrdart,
      mod11 =modrdmol,
      pal = pal,
      coefs = c(
        "Nodule complexity (PC1)" = "PC1_score", 
        "Bed complexity (PC2)" = "PC2_score",
        "PC1:PC2" = "PC1_score:PC2_score",
        "Depth" = "Depth",
        "Exposure" = "Fetch_max",
        "Gravel" = "Gravel",
        "Organic matter %" = "OM",
        "Mean temperature" = "T_mean",
        "Mean current velocity" = "Current_mean",
        "Year" = "Year"
      ),
      model.names = c("1. Total macrofauna", "2. Annelids", "3. Arthropods", "4. Molluscs"),
      data = alphamod
    ),
    ),
  
  
  
      #--- by trait ----
  tar_target(epidensselR2, adespatial::forward.sel(alphamod %>% select(Epifauna), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modfdepi, lm(data = alphamod, formula = log(Epifauna) ~ PC1_score * PC2_score + Current_mean + Fetch_max  + OM + Gravel + T_mean + Year)),
  tar_target(modrdepi, lme4::lmer(data = alphamod, formula = log(Epifauna) ~ PC1_score * PC2_score + Current_mean + Fetch_max  + OM + Gravel + T_mean + Year +(1|Site))),
  tar_target(surfepi, dens_surfplot(modrdepi, pal, "Epifauna density")),
  tar_target(infdensselR2, adespatial::forward.sel(alphamod %>% select(Infauna), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modfdinf, lm(data = alphamod, formula = log(Infauna) ~ PC1_score * PC2_score + Depth + T_sd + T_mean + Gravel + Year)),
  tar_target(modrdinf, lme4::lmer(data = alphamod, formula = log(Infauna) ~ PC1_score * PC2_score + Depth + T_sd + T_mean + Gravel + Year + (1|Site))),
  tar_target(surfinf, dens_surfplot(modrdinf, pal, "Infauna density")),
  tar_target(intdensselR2, adespatial::forward.sel(alphamod %>% select(Interstice), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modfdint, lm(data = alphamod, formula = log(Interstice+1) ~ PC1_score * PC2_score + Gravel  + OM + Fetch_max + Mud + Year)),
  tar_target(modrdint, lme4::lmer(data = alphamod, formula = log(Interstice+1) ~ PC1_score * PC2_score + Gravel  + OM + Fetch_max + Mud + Year + (1|Site))),
  tar_target(surfint, dens_surfplot(modrdint, pal, "Interstitial fauna density")),
  tar_target(
    psdtrait,
    p.summs(
      modfdsel,
      modfdepi,
      modfdinf,
      modfdint,
      modrdsel,
      modrdepi,
      modrdinf,
      modrdint,
      pal[c(2,9,8,10)],
      coefs = c(
        "Nodule complexity (PC1)" = "PC1_score", 
        "Bed complexity (PC2)" = "PC2_score",
        "PC1:PC2" = "PC1_score:PC2_score",
        "Mean current velocity" = "Current_mean",
        "Depth" = "Depth",
        "Exposure" = "Fetch_max",
        "Gravel" = "Gravel",
        "Organic Matter %" = "OM",
        "Mud" = "Mud",
        "Mean Temperature" = "T_mean",
        "Temperature Variation (sd)" = "T_sd",
        "Year" = "Year"
      ),
      model.names = c("1. Total macrofauna", "2. Epifauna", "3. Infauna", "4. Interstitial Fauna")
    )
  ),
  
    #-- N2 ----
  tar_target(modfixN21, lm(data = alphamod, formula = N2 ~ PC1_score)),
  tar_target(modfixN2poly1, lm(data = alphamod, formula = N2 ~ poly(PC1_score,3))),
  tar_target(N2PC1, ggplot(data = alphamod, aes(x = PC1_score,
                                                   y = N2,
                                                   colour = Site))+
               scale_color_manual(values = alpha(pal, .4)) +
               scale_fill_manual(values = pal) +
               geom_violin(aes(group = Point,
                               fill = Site),
                           width = .4,
                           alpha = .5,
                           # trim = FALSE,
                           draw_quantiles = .5)+
               geom_point(alpha = .6, 
                          size = 2.5)+
               geom_smooth(colour = "gray45", 
                           method = lm
               )+
               ylab("N2")+
               xlab("Nodule Complexity (PC1)")+
               #scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125))+
               #scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1))+
               theme(legend.title = element_text(face = "bold",
                                                 size = 22),
                     legend.text = element_text(size = 20),
                     legend.background = element_blank(),
                     legend.box.background = element_blank(),
                     legend.key = element_blank(),
                     axis.text = element_text(size = 20),
                     axis.title = element_text(size =22,
                                               face = "bold")) +
               guides(fill = guide_legend(title.position = "top"))),
  tar_target(modfixN22, lm(data = alphamod, formula = N2 ~ PC2_score)),
  tar_target(N2PC2, ggplot(data = alphamod, aes(x = PC2_score,
                                                y = N2,
                                                colour = Site))+
               scale_color_manual(values = alpha(pal, .4)) +
               scale_fill_manual(values = pal) +
               geom_violin(aes(group = Point,
                               fill = Site),
                           width = .3,
                           alpha = .5,
                           # trim = FALSE,
                           draw_quantiles = .5)+
               geom_point(alpha = .6, 
                          size = 2.5)+
               geom_smooth(colour = "gray45", 
                           method = lm
               )+
               ylab("N2")+
               xlab("Bed Complexity (PC2)")+
               #scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125))+
               #scale_x_continuous(breaks = c(-.6, -.4, -.2, 0, .2, .4, .6))+
               theme(legend.title = element_text(face = "bold",
                                                 size = 22),
                     legend.text = element_text(size = 20),
                     legend.background = element_blank(),
                     legend.box.background = element_blank(),
                     legend.key = element_blank(),
                     axis.text = element_text(size = 20),
                     axis.title = element_text(size =22,
                                               face = "bold")) +
               guides(fill = guide_legend(title.position = "top"))),
  
  tar_target(modfixN212, lm(data = alphamod, formula = N2 ~ PC1_score * PC2_score)),
  tar_target(modranN212, lme4::lmer(data = alphamod, formula = N2 ~ PC1_score * PC2_score + (1|Site))),
  tar_target(N2selR2, adespatial::forward.sel(alphamod %>% select(N2), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
  tar_target(modN2sel, lm(formula = N2 ~ PC1_score * PC2_score + Gravel + T_mean + T_sd + Current_mean + Fetch_max + Year, data = alphamod)),
  tar_target(modN2rsel, lme4::lmer(formula = N2 ~ PC1_score * PC2_score + Gravel + T_mean + T_sd + Current_mean + Fetch_max + Year + (1|Site), data = alphamod)),
  tar_target(
    psN2,
    jtools::plot_summs(
      modfixN212,
      modN2sel,
      modN2rsel,
      colors = pal[c(5:7)],
      robust = FALSE,
      model.names = c(
        "N2 ~ Complexity",
        "N2 ~ Complexity + Environment",
        "N2 ~ Complexity + Environment + (1|Site)"
      ),
      coefs = c(
        "Nodule complexity (PC1)" = "PC1_score", 
        "Bed complexity (PC2)" = "PC2_score",
        "PC1:PC2" = "PC1_score:PC2_score",
        "Gravel" = "Gravel",
        "Depth" =  "Depth",
        "Exposure" = "Fetch_max",
        "Mean current velocity" = "Currrent_mean",
        "Mean bottom temperature" = "T_mean",
        "Bottom temperature sd" = "T_sd",
        "Year" =  "Year"
      ),
      point.size = 5,
      inner_ci_level =  .9,
      scale = TRUE) +
      theme(
        text = element_text(size = 30),
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 22),
        legend.text = element_text(size = 20),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 20, face = "bold")
      ) +
      guides(colour = guide_legend(title.position = "top"))
  ),
    #-- J ----
    tar_target(modfixJ1, lm(data = alphamod, formula = J ~ PC1_score)),
tar_target(modfixJpoly1, lm(data = alphamod, formula = J ~ poly(PC1_score,3))),
tar_target(totJpc1, ggplot(data = alphamod, aes(x = PC1_score,
                                                   y = J,
                                                   colour = Site))+
             scale_color_manual(values = alpha(pal, .4)) +
             scale_fill_manual(values = pal) +
             geom_violin(aes(group = Point,
                             fill = Site),
                         width = .08,
                         alpha = .5,
                         # trim = FALSE,
                         draw_quantiles = .5)+
             geom_point(alpha = .6, 
                        size = 2.5)+
             geom_smooth(colour = "gray45", 
                         method = lm
             )+
             ylab("Pielou's J")+
             xlab("Nodule Complexity (PC1)")+
             scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125))+
             scale_x_continuous(breaks = c(-1, -.5, 0, .5, 1))+
             theme(legend.title = element_text(face = "bold",
                                               size = 22),
                   legend.text = element_text(size = 20),
                   legend.background = element_blank(),
                   legend.box.background = element_blank(),
                   legend.key = element_blank(),
                   axis.text = element_text(size = 20),
                   axis.title = element_text(size =22,
                                             face = "bold")) +
             guides(fill = guide_legend(title.position = "top"))),
tar_target(modfixJ2, lm(data = alphamod, formula = J ~ PC2_score)),
tar_target(JPC2, ggplot(data = alphamod, aes(x = PC2_score,
                                                y = J,
                                                colour = Site))+
             scale_color_manual(values = alpha(pal, .4)) +
             scale_fill_manual(values = pal) +
             geom_violin(aes(group = Point,
                             fill = Site),
                         width = .08,
                         alpha = .5,
                         # trim = FALSE,
                         draw_quantiles = .5)+
             geom_point(alpha = .6, 
                        size = 2.5)+
             geom_smooth(colour = "gray45", 
                         method = lm
             )+
             ylab("Pielou's J")+
             xlab("Bed Complexity (PC2)")+
             scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125))+
             scale_x_continuous(breaks = c(-.6, -.4, -.2, 0, .2, .4, .6))+
             theme(legend.title = element_text(face = "bold",
                                               size = 22),
                   legend.text = element_text(size = 20),
                   legend.background = element_blank(),
                   legend.box.background = element_blank(),
                   legend.key = element_blank(),
                   axis.text = element_text(size = 20),
                   axis.title = element_text(size =22,
                                             face = "bold")) +
             guides(fill = guide_legend(title.position = "top"))),

tar_target(modfixJ12, lm(data = alphamod, formula = J ~ PC1_score * PC2_score)),
tar_target(modranJ12, lme4::lmer(data = alphamod, formula = J ~ PC1_score * PC2_score + (1|Site))),
tar_target(JselR2, adespatial::forward.sel(alphamod %>% select(J), alphamod %>% select(Mud:Fetch_max, PC1_score, PC2_score))),
tar_target(modJsel, lm(formula = J ~ PC1_score * PC2_score + Gravel + Current_mean + Fetch_max + T_sd + OM + T_mean + Year, data = alphamod)),
tar_target(modJrsel, lme4::lmer(formula = J ~ PC1_score * PC2_score + Gravel + Current_mean + Fetch_max + T_sd + OM + T_mean + Year + (1|Site), data = alphamod)),
tar_target(
  psJ,
  jtools::plot_summs(
    modfixJ12,
    modJsel,
    modJrsel,
    colors = pal[c(5:7)],
    robust = FALSE,
    model.names = c(
      "J ~ Complexity",
      "J ~ Complexity + Environment",
      "J ~ Complexity + Environment + (1|Site)"
    ),
    coefs = c(
      "Nodule complexity (PC1)" = "PC1_score", 
      "Bed complexity (PC2)" = "PC2_score",
      "PC1:PC2" = "PC1_score:PC2_score",
      "Gravel" =  "Gravel",
      "Mean current velocity" = "Currrent_mean",
      "Exposure" = "Fetch_max",
      "Bottom temperature sd" = "T_sd",
      "Organic matter %" = "OM",
      "Mean bottom temperature" = "T_mean",
      "Year" =  "Year"
    ),
    point.size = 5,
    inner_ci_level =  .9,
    scale = TRUE) +
    theme(
      text = element_text(size = 30),
      legend.position = "top",
      legend.title = element_text(face = "bold", size = 22),
      legend.text = element_text(size = 20),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 20, face = "bold")
    ) +
    guides(colour = guide_legend(title.position = "top"))
),
  #- beta diversity ~ complexity + env----
    #-- fauna pca ----
  tar_target(bcdens, box.cox.chord(dens_num)),
  tar_target(sitedens, fauna_dens %>%   
               group_by(Site) %>% 
               summarise_if(is.numeric, mean) %>%
               ungroup() %>%
               column_to_rownames("Site")),
  tar_target(sitebcdens, box.cox.chord(sitedens)),
  tar_target(pcafauna, rda(bcdens)),
  tar_target(spfauna, screep(pcafauna)),
  tar_target(faunapca, bg_pca(pcafauna, metadata = fauna_dens, main.group = "Site", scale.fill = pal, scale.colour = pal, goodness.thresh = .40, stat1 = "ellipse", add.centroids = TRUE, ysites = c(-0.18, .18), xsites = c(-.18,.18), ysp = c(-.5,.5), xsp = c(-.5,.5), nudge.y = c(0,0,-.022,.02,-.022,-.022,0,-.022,-.022,-.022), nudge.x = c(-.05, .05, -.02, 0, 0, .02, -.05, 0, 0, 0), conf.level = .8, font.size = 11/.pt, ext.plot.scale = 2.5, point.size = 2.5, axis.size = 16, axis.text = 20, c.size = 3, reverse.y = FALSE, reverse.x = TRUE)),
    #-- fauna cluster----
  tar_target(faunaclust, clustfauna(sitebcdens, pal)),
    #-- rda ----
  tar_target(envcomp, env %>%
               left_join(med_sel2) %>%
               select(-PC1_score, -PC2_score, -PC1_c, -PC2_c) %>% 
               set_rownames(paste(.$Point, .$Year, sep = "_"))),
  
  tar_target(envcomp_num, envcomp %>%
               #mutate(code = paste(.$Point, .$Year, sep = "_")) %>% 
               select_if(Negate(is.factor))), #%>% 
               #column_to_rownames("code")),
  tar_target(rdacomp, rda(formula = bcdens ~ ., envcomp_num %>% select(Branching_density:DR3))),
  tar_target(rdagranulo, rda(formula = bcdens ~ ., envcomp_num %>% select(Mud:OM))),
  tar_target(rdatemp, rda(formula = bcdens ~ ., envcomp_num %>% select(T_mean:T_sd))),
  tar_target(rdahydro, rda(formula = bcdens ~ ., envcomp_num %>% select(Current_mean, Fetch_max))),
  tar_target(rdafselcomp, adespatial::forward.sel(bcdens, envcomp_num %>% select(Branching_density:DR3), nperm = 999)),
  tar_target(rdafselgranulo, adespatial::forward.sel(bcdens, envcomp_num %>% select(Mud:OM), nperm = 999)),
  tar_target(rdafseltemp, adespatial::forward.sel(bcdens, envcomp_num %>% select(T_mean:T_sd), nperm = 999)),
  tar_target(rdafselhydro, adespatial::forward.sel(bcdens, envcomp_num %>% select(Current_mean, Fetch_max), nperm = 999)),
  tar_target(rdafull, 
             rda(formula = bcdens ~ Mud + Depth + Current_mean + Branching_density +  Fetch_max + T_sd + Sphericity + T_mean + DR3 + Gravel +  Total_Density + L + OM + Year,
                 data = envcomp)
             ),
  tar_target(vifrda, vif.cca(rdafull)),
  tar_target(rdasite, 
             rda(formula = bcdens ~ Mud + Depth + Current_mean + Branching_density +  Fetch_max + T_sd + Sphericity + T_mean + DR3 + Gravel +  Total_Density + L + OM + Year + Site,
                 data = envcomp)
  ),
  tar_target(triplotrda, autoplot.rda.victor(object = rdasite, axes = c(1,2), metadata = envcomp, scale.fill = pal, scale.colour = pal, layers = c("sites", "biplot", "regression", "centroids"), thresh = 0.24, stat = "ellipse", lvl = .8, legend.position = c(.5,.9), title.size = 22, font.size = 22/.pt, arrows = FALSE) + 
               theme(legend.title = element_text(face = "bold", colour = "gray35", size = 22), legend.text = element_text(size = 20, colour = "gray35"), legend.position = "top", legend.background = element_blank(), legend.spacing.x = unit(0.5, "cm"), legend.box.background = element_blank(), legend.key = element_blank(), axis.text = element_text(size = 20)) + guides(fill = guide_legend(title.position = "top"), )+
               ylim(c(-.6, 0.6))+
               xlim(c(-.6, 0.6))
             ),
  tar_target(aovsite, anova(rdasite, permutations = how(nperm = 999))),
  tar_target(aovterm, anova(rdasite, permutations = how(nperm = 99), by = "term")),
  tar_target(aovaxis, anova(rdasite, permutations = how(nperm = 99), by = "axis")),
  
    #-- hierarchical partitioning ----
  tar_target(hierpartmain, hp_main(envcomp, bcdens)),
  tar_target(upsetmain, upset_vp_victor(hierpartmain, cutoff = 0.008, int.var = "Complexity", pal = pal[1:8], title.cex = 22, axis.cex = 20, pch.size = 6, col.width = .8, effect.cex = 6)),
  
  tar_target(hierpartsep, hp_sep(envcomp, bcdens)),
  tar_target(upsetsep, upset_vp_victor(hierpartsep, cutoff = 0.005, int.var = "Nodule complexity", pal = pal, title.cex = 22, axis.cex = 20, pch.size = 6, col.width = .8, effect.cex = 6, int.col = 5)),
  #- temporal diversity ----
  tar_target(bdtot, bd.tot(fauna_dens, med_sel2)),
  tar_target(bdplot, bd.plot(bdtot, pal)),
  tar_target(replplot, ggplot(bdtot, aes(y = Point, x = `Repl/BDtotal`, fill = Site))+
    geom_bar(stat = "identity")+
    scale_y_discrete(limits = rev)+
    scale_fill_manual(values = pal)+
      ylab("")+
      xlab("Replacement/BDtotal")+
      theme(legend.title = element_text(face = "bold",
                                        size = 22),
            legend.text = element_text(size = 20),
            legend.background = element_blank(),
            legend.box.background = element_blank(),
            legend.key = element_blank(),
            legend.position = "bottom",
            axis.text = element_text(size = 20),
            axis.title = element_text(size =22,
                                      face = "bold"),
            axis.text.y = element_text(colour = "transparent"),
            panel.grid = element_blank()) +
      coord_cartesian(xlim = c(0,1), expand = FALSE) +
      guides(fill = guide_legend(title.position = "top"))
    ),
  tar_target(bdepi, bd.tot(epi, med_sel2)),
  tar_target(bdint, bd.tot(int, med_sel2)),
  tar_target(bdinf, bd.tot(inf, med_sel2)),
  tar_target(bdart, bd.tot(art, med_sel2)),
  tar_target(bdann, bd.tot(ann, med_sel2)),
  tar_target(bdmol, bd.tot(mol, med_sel2)),
  
    #-- BD ~ PC1 * PC2 ----
      #--- Total fauna ----
  tar_target(modbdtot, lm(BDtotal ~ poly(PC1_score, 2) * PC2_score, bdtot)),
  tar_target(fig3a, bd_surfplot(modbdtot, pal, "Temporal beta diversity <br> (total macrofauna)")),
  tar_target(modrdftot, lm(RichDif ~ poly(PC1_score, 2) * PC2_score, bdtot)),
  tar_target(surfrdf, bd_surfplot(modrdftot, pal, "Richness difference <br> (total macrofauna)")),
  tar_target(modrptot, lm(Repl ~ poly(PC1_score, 2) * PC2_score, bdtot)),
  tar_target(surfrp, bd_surfplot(modrptot, pal, "Replacement <br> (total macrofauna)")),
      #--- Epifauna ----
  tar_target(modbdepi, lm(BDtotal ~ poly(PC1_score, 2) * PC2_score, bdepi)),
  tar_target(surfbdepi, bd_surfplot(modbdepi, pal, "Temporal beta diversity <br> (Epifauna)")),
  tar_target(modrdfepi, lm(RichDif ~ poly(PC1_score, 2) * PC2_score, bdepi)),
  tar_target(surfrdfepi, bd_surfplot(modrdfepi, pal, "Richness difference <br> (Epifauna)")),
  tar_target(modrpepi, lm(Repl ~ poly(PC1_score, 2) * PC2_score, bdepi)),
  tar_target(surfrpepi, bd_surfplot(modrpepi, pal, "Replacement <br> (Epifauna)")),
      #--- Infauna ----
  tar_target(modbdinf, lm(BDtotal ~ poly(PC1_score, 2) * PC2_score, bdinf)),
  tar_target(surfbdinf, bd_surfplot(modbdinf, pal, "Temporal beta diversity <br> (Infauna)")),
  tar_target(modrdfinf, lm(RichDif ~ poly(PC1_score, 2) * PC2_score, bdinf)),
  tar_target(surfrdfinf, bd_surfplot(modrdfinf, pal, "Richness difference <br> (Infauna)")),
  tar_target(modrpinf, lm(Repl ~ poly(PC1_score, 2) * PC2_score, bdinf)),
tar_target(surfrpinf, bd_surfplot(modrpinf, pal, "Replacement <br> (Infauna)")),
      #--- Interstitial Fauna ----
  tar_target(modbdint, lm(BDtotal ~ poly(PC1_score, 2) * PC2_score, bdint)),
  tar_target(surfbdint, bd_surfplot(modbdint, pal, "Temporal beta diversity <br> (Interstitial fauna)")),
  tar_target(modrdfint, lm(RichDif ~ poly(PC1_score, 2) * PC2_score, bdint)),
  tar_target(surfrdfint, bd_surfplot(modrdfint, pal, "Richness difference <br> (Interstitial fauna)")),
  tar_target(modrpint, lm(Repl ~ poly(PC1_score, 2) * PC2_score, bdint)),
  tar_target(surfrpint, bd_surfplot(modrpint, pal, "Replacement <br> (Interstitial fauna)")),
  tar_target(
    bdtrait,
    p.summsbd(
      modbdtot,
      modbdepi,
      modbdinf,
      modbdint,
      modrptot,
      modrpepi,
      modrpinf,
      modrpint,
      modrdftot,
      modrdfepi,
      modrdfinf,
      modrdfint,
      pal = pal[c(2, 9, 8, 10)],
      coefs = c(
        "Nodule complexity (PC1)" = "`poly(PC1_score, 2)`1",
        "PC1^2" = "`poly(PC1_score, 2)`2",
        "Bed complexity (PC2)" = "PC2_score",
        "PC1:PC2" = "`poly(PC1_score, 2)`1:PC2_score",
        "PC1ˆ2:PC2" = "`poly(PC1_score, 2)`2:PC2_score"
      ),
      model.names = c("Total",  "Epifauna", "Infauna", "Interstitial"))),
    
      #--- Arthropoda ----
  tar_target(modbdart, lm(BDtotal ~ poly(PC1_score, 2) * PC2_score, bdart)),
  tar_target(surfbdart, bd_surfplot(modbdart, pal, "Temporal beta diversity <br> (Arthropods)")),
  tar_target(modrdfart, lm(RichDif ~ poly(PC1_score, 2) * PC2_score, bdart)),
tar_target(surfrdfart, bd_surfplot(modrdfart, pal, "Richness difference <br> (Arthropods)")),
  tar_target(modrpart, lm(Repl ~ poly(PC1_score, 2) * PC2_score, bdart)),
  tar_target(surfrpart, bd_surfplot(modrpart, pal, "Replacement <br> (Arthropods)")),
      #--- Annelida ----
  tar_target(modbdann, lm(BDtotal ~ poly(PC1_score, 2) * PC2_score, bdann)),
  tar_target(surfbdann, bd_surfplot(modbdann, pal, "Temporal beta diversity <br> (Annelids)")),
  tar_target(modrdfann, lm(RichDif ~ poly(PC1_score, 2) * PC2_score, bdann)),
  tar_target(surfrdfann, bd_surfplot(modrdfann, pal, "Richness difference <br> (Annelids)")),
  tar_target(modrpann, lm(Repl ~ poly(PC1_score, 2) * PC2_score, bdann)),
  tar_target(surfrpann, bd_surfplot(modrpann, pal, "Replacement <br> (Annelids)")),
      #--- Mollusca ----
  tar_target(modbdmol, lm(BDtotal ~ poly(PC1_score, 2) * PC2_score, bdmol)),
  tar_target(surfbdmol, bd_surfplot(modbdmol, pal, "Temporal beta diversity <br> (Molluscs)")),
  tar_target(modrdfmol, lm(RichDif ~ poly(PC1_score, 2) * PC2_score, bdmol)),
  tar_target(surfrdfmol, bd_surfplot(modrdfmol, pal, "Richness difference <br> (Molluscs)")),
  tar_target(modrpmol, lm(Repl ~ poly(PC1_score, 2) * PC2_score, bdmol)),
  tar_target(surfrpmol, bd_surfplot(modrpmol, pal, "Replacement <br> (Molluscs)")),
  tar_target(
    bdphyl,
    p.summsbd(
      modbdtot,
      modbdann,
      modbdart,
      modbdmol,
      modrptot,
      modrpann,
      modrpart,
      modrpmol,
      modrdftot,
      modrdfann,
      modrdfart,
      modrdfmol,
      pal = pal[c(1, 3:5)],
      coefs = c(
        "Nodule complexity (PC1)" = "`poly(PC1_score, 2)`1",
        "PC1^2" = "`poly(PC1_score, 2)`2",
        "Bed complexity (PC2)" = "PC2_score",
        "PC1:PC2" = "`poly(PC1_score, 2)`1:PC2_score",
        "PC1ˆ2:PC2" = "`poly(PC1_score, 2)`2:PC2_score"
      ),
      model.names = c("Total",  "Annelids", "Arthropods", "Molluscs"))),
    #- palette ----
  tar_target(pal, c("#4477AA", "#8ECDDE", "#44AA99", "#858c64", "#e8bb5a",  "#EE8866", "#d44e65", "#FFAABB", "#7b538c", "#80898f" ))
)
