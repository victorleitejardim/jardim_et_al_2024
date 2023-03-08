library(targets)
library(tarchetypes)
library(vegan)
library(dplyr)

file.sources <- list.files(
  here::here("R"),
  pattern = "*.R$",
  full.names = TRUE,
  ignore.case = TRUE
)

sapply(file.sources, source, .GlobalEnv)

list(
  
  #complexity
  tar_target(compraw, here::here("data", "complexity.Rdata"), format = "file"), 
  tar_target(complexity, load.data(compraw)),
  tar_target(comp, sphericity(complexity)),  #calculate sphericity metrics
  tar_target(comp_med, comp.med(comp)), #median values at the point level
  tar_target(bccomp, bc.comp(comp)), #boxcox transformation
  tar_target(comp_num, comp.num(bccomp)), #only numeric
  tar_target(comp_sel, bccomp %>% select(-DR1,-DR2,  -Broken_density, -I, -S, -Dry_weight)),
  tar_target(num_sel, comp.num(comp_sel)),
  tar_target(bcmed, bc.compmed(comp_med)), #boxcox transformation for median values
  tar_target(med_num, compmed.num(bcmed)), #only numeric
  tar_target(med_sel, bcmed %>% select(-DR1,-DR2,  -Broken_density, -I, -S, -Dry_weight)),
  tar_target(med_numsel, compmed.num(med_sel)),
  tar_target(pairscomp, GGally::ggpairs(bccomp %>% select_if(is.numeric) %>% select(-Sample))),
  ##pca
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
      c.size = 4
    )
  ), 
  
  tar_target(pcatotsel, rda(num_sel, scale = TRUE)),
  tar_target(bccomp2, extract.pc(bccomp, pcatotsel)), 
  tar_target(sptotsel, screep(pcatotsel, nb = 7)),
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
  tar_target(spmedsel, screep(pcamedsel, nb = 7)),
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
      ysites = c(-1.1, .8),
      xsites = c(-1.1, .8),
      ysp = c(-1.7, 1.7),
      xsp = c(-1.7, 1.7),
      axis.size = 16,
      axis.text = 20,
      c.size = 3,
      nudge.x = c(-.16, .11, 0, 0, .1, .12, .12, .22, 0, -.28),
      nudge.y = c(.11,-.08,-.1,-.1,-.09,-.1, .1, 0, -.1, 0),
      font.size = 11 / .pt,
      ext.plot.scale = 2.5,
      reverse.y = TRUE
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
  
  #fauna
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
  
  #relative abundance
  tar_target(relab_phy, relab.mean(fauna_dens, classif, taxlvl = "Phylum", value = .5)),
  tar_target(relab_ord, relab.mean(fauna_dens, classif, taxlvl = "Order")),
  tar_target(relab_arth, relab.phyl(fauna_dens, classif, taxlvl = "Family", phyl = "Arthropoda", value = 8)),
  tar_target(relab_mol, relab.phyl(fauna_dens, classif, taxlvl = "Family", phyl = "Mollusca", value = 8)),
  tar_target(relab_ann, relab.phyl(fauna_dens, classif, taxlvl = "Family", phyl = "Annelida", value = 8)),
  #bathymetry
  tar_target(bathydata, here::here("data", "depth.Rdata"), format = "file"),
  tar_target(bathyraw, load.data(file = bathydata)),
  tar_target(bathy, tidy.depth(bathyraw)),
  #fetch
  tar_target(fetchdata, here::here("data", "fetchDCE.Rdata"), format = "file"),
  tar_target(fetchraw, load.data(file = fetchdata)),
  tar_target(fetch, tidy.fetch(fetchraw)),
  #granulometry
  tar_target(granulodata, here::here("data", "Granulo_imputed_point_21_04_2021.Rdata"), format = "file"),
  tar_target(granuloraw, load.data(file = granulodata)),
  tar_target(granulo, tidy.granulo(granuloraw)),
  #hydrology
  tar_target(hydrodata, here::here("data", "MA_hydrology_point.Rdata"), format = "file"),
  tar_target(hydroraw, load.data(file = hydrodata)),
  tar_target(hydro, tidy.hydro(hydroraw)),
  #environment tables
  tar_target(env, envtable(granulo, hydro, bathy, fetch)),
  tar_target(env_mean, envmean(env)),
  
  #palette
  tar_target(pal, c("#4477AA", "#8ECDDE", "#44AA99", "#858c64", "#e8bb5a",  "#EE8866", "#d44e65", "#FFAABB", "#7b538c", "#80898f" ))
)
