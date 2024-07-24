require(jtools)
require(lme4)
require(broom)
require(broom.mixed)
require(sandwich)
require(plotly)
require(pracma)

s_surfplot <- function(mod, pal, ztitle) {
  grid <- ggeffects::predict_response(mod, c("PC2_score [-.8:0.65 by=0.05]", "PC1_score [all]"), margin = "empirical") 
  grid_wide <- as.data.frame(grid) %>% 
      select(x, group, predicted) %>% 
      pivot_wider(names_from = x, values_from = predicted) %>% 
      tibble::column_to_rownames("group") %>% 
      as.matrix()
  
  
  
  grid_low <- as.data.frame(grid) %>% 
    select(x, group, conf.low) %>% 
    pivot_wider(names_from = x, values_from = conf.low) %>% 
    tibble::column_to_rownames("group") %>% 
    as.matrix()
  
  
  grid_high <- as.data.frame(grid) %>% 
    select(x, group, conf.high) %>% 
    pivot_wider(names_from = x, values_from = conf.high) %>% 
    tibble::column_to_rownames("group") %>% 
    as.matrix()
  
    intplot <- plot_ly() %>%
      add_surface(
        z = grid_low,
        y = rownames(grid_low),
        x = colnames(grid_low),
        opacity = .25,
        colorscale = list(c(0, 1), c("white", "white")),
        type = "mesh3d",
        showscale = FALSE,
        contours = list(x = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5,
                                 color = "gray"),
                        y = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5,
                                 color = "gray"))
      ) %>%
      add_surface(
        z = grid_wide,
        y = rownames(grid_wide),
        x = colnames(grid_wide),
        opacity = .6,
        colorscale = list(c(0, .35, 1), c(pal[1], pal[5], pal[7])),
        type = "mesh3d",
        showscale = FALSE,
        contours = list(x = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5),
                        y = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5),
                        z = list(show = TRUE,
                                 start = 40,
                                 end = 90,
                                 size = 2,
                                 color = "black"))
      ) %>%
      add_surface(
        z = grid_high,
        y = rownames(grid_high),
        x = colnames(grid_high),
        opacity = .25,
        colorscale = list(c(0, 1), c("white", "white")),
        type = "mesh3d",
        showscale = FALSE,
        contours = list(x = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5,
                                 color = "white"),
                        y = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5,
                                 color = "white"))
      ) %>%
      layout(
        showlegend = FALSE,
        scene =  list(
          yaxis = list(title = "Rhodolith complexity",
                       gridcolor = "white",
                       linecolor = "black",
                       zerolinecolor = "black",
                       zerolinewidth = 2,
                       backgroundcolor = "white",
                       showbackground = TRUE,
                       mirror = TRUE),
          xaxis = list(autorange = "reversed",
                       title = "Bed complexity",
                       gridcolor = "white",
                       linecolor = "black",
                       zerolinecolor = "black",
                       zerolinewidth = 2,
                       backgroundcolor = "white",
                       showbackground = TRUE,
                       mirror = TRUE),
          zaxis = list(title = ztitle,
                       
                       gridcolor = "white",
                       linecolor = "black",
                       backgroundcolor = "white",
                       showbackground = TRUE,
                       mirror = TRUE),
          aspectmode = "cube",
          camera = list(eye = list(
            x = 4, y = 2, z = 1
          ))
        )
      )
    intplot
}

bd_surfplot <- function(mod, pal, ztitle) {
  grid <- ggeffects::predict_response(mod, c("PC2_score [-.8:0.65 by=0.05]", "PC1_score [-.8:0.65 by=0.05]"), margin = "empirical") 
  grid_wide <- as.data.frame(grid) %>% 
    select(x, group, predicted) %>% 
    pivot_wider(names_from = x, values_from = predicted) %>% 
    tibble::column_to_rownames("group") %>% 
    as.matrix()

  grid_low <- as.data.frame(grid) %>% 
    select(x, group, conf.low) %>% 
    pivot_wider(names_from = x, values_from = conf.low) %>% 
    tibble::column_to_rownames("group") %>% 
    as.matrix()
  
  
  grid_high <- as.data.frame(grid) %>% 
    select(x, group, conf.high) %>% 
    pivot_wider(names_from = x, values_from = conf.high) %>% 
    tibble::column_to_rownames("group") %>% 
    as.matrix()

    intplot <- plot_ly() %>%
      add_surface(
        z = grid_low,
        y = rownames(grid_low),
        x = colnames(grid_low),
        opacity = .25,
        colorscale = list(c(0, 1), c("white", "white")),
        type = "mesh3d",
        showscale = FALSE,
        contours = list(x = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5,
                                 color = "gray"),
                        y = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5,
                                 color = "gray"))
      ) %>%
      add_surface(
        z = grid_wide,
        y = rownames(grid_wide),
        x = colnames(grid_wide),
        opacity = .6,
        colorscale = list(c(0, .35, 1), c(pal[1], pal[5], pal[7])),
        type = "mesh3d",
        showscale = FALSE,
        contours = list(x = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5),
                        y = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5),
                        z = list(show = TRUE,
                                 start = 0,
                                 end = 1,
                                 size = .01,
                                 color = "black"))
      ) %>%
      add_surface(
        z = grid_high,
        y = rownames(grid_high),
        x = colnames(grid_high),
        opacity = .25,
        colorscale = list(c(0, 1), c("white", "white")),
        type = "mesh3d",
        showscale = FALSE,
        contours = list(x = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5,
                                 color = "white"),
                        y = list(show = TRUE,
                                 start = -.8,
                                 end = .65,
                                 size = 1.45,
                                 width = 5,
                                 color = "white"))
      ) %>%
      layout(
        showlegend = FALSE,
        scene =  list(
          yaxis = list(title = "Rhodolith complexity",
                       gridcolor = "white",
                       linecolor = "black",
                       zerolinecolor = "black",
                       zerolinewidth = 2,
                       backgroundcolor = "white",
                       showbackground = TRUE,
                       mirror = TRUE),
          xaxis = list(autorange = "reversed",
                       title = "Bed complexity",
                       gridcolor = "white",
                       linecolor = "black",
                       zerolinecolor = "black",
                       zerolinewidth = 2,
                       backgroundcolor = "white",
                       showbackground = TRUE,
                       mirror = TRUE),
          zaxis = list(title = ztitle,
                       gridcolor = "white",
                       linecolor = "black",
                       backgroundcolor = "white",
                       showbackground = TRUE,
                       mirror = TRUE),
          aspectmode = "cube",
          camera = list(eye = list(
            x = 2, y = 2, z = 1.5
          ))
        )
      )
    intplot
}

dens_surfplot <- function(mod, pal, ztitle) {
  grid <- ggeffects::predict_response(mod, c("PC2_score [-.8:0.65 by=0.05]", "PC1_score [-.8:0.65 by=0.05]"), margin = "empirical") 
  grid_wide <- as.data.frame(grid) %>% 
    select(x, group, predicted) %>% 
    pivot_wider(names_from = x, values_from = predicted) %>% 
    tibble::column_to_rownames("group") %>% 
    as.matrix()
  
  grid_low <- as.data.frame(grid) %>% 
    select(x, group, conf.low) %>% 
    pivot_wider(names_from = x, values_from = conf.low) %>% 
    tibble::column_to_rownames("group") %>% 
    as.matrix()
  
  
  grid_high <- as.data.frame(grid) %>% 
    select(x, group, conf.high) %>% 
    pivot_wider(names_from = x, values_from = conf.high) %>% 
    tibble::column_to_rownames("group") %>% 
    as.matrix()
  
  intplot <- plot_ly() %>%
    add_surface(
      z = grid_low,
      y = rownames(grid_low),
      x = colnames(grid_low),
      opacity = .25,
      colorscale = list(c(0, 1), c("white", "white")),
      type = "mesh3d",
      showscale = FALSE,
      contours = list(x = list(show = TRUE,
                               start = -.8,
                               end = .65,
                               size = 1.45,
                               width = 5,
                               color = "gray"),
                      y = list(show = TRUE,
                               start = -.8,
                               end = .65,
                               size = 1.45,
                               width = 5,
                               color = "gray"))
    ) %>%
    add_surface(
      z = grid_wide,
      y = rownames(grid_wide),
      x = colnames(grid_wide),
      opacity = .6,
      colorscale = list(c(0, .35, 1), c(pal[1], pal[5], pal[7])),
      type = "mesh3d",
      showscale = FALSE,
      contours = list(x = list(show = TRUE,
                               start = -.8,
                               end = .65,
                               size = 1.45,
                               width = 5),
                      y = list(show = TRUE,
                               start = -.8,
                               end = .65,
                               size = 1.45,
                               width = 5),
                      z = list(show = TRUE,
                               start = 0,
                               end = 10000,
                               size = 100,
                               color = "black"))
    ) %>%
    add_surface(
      z = grid_high,
      y = rownames(grid_high),
      x = colnames(grid_high),
      opacity = .25,
      colorscale = list(c(0, 1), c("white", "white")),
      type = "mesh3d",
      showscale = FALSE,
      contours = list(x = list(show = TRUE,
                               start = -.8,
                               end = .65,
                               size = 1.45,
                               width = 5,
                               color = "white"),
                      y = list(show = TRUE,
                               start = -.8,
                               end = .65,
                               size = 1.45,
                               width = 5,
                               color = "white"))
    ) %>%
    layout(
      showlegend = FALSE,
      scene =  list(
        yaxis = list(title = "Rhodolith complexity",
                     gridcolor = "white",
                     linecolor = "black",
                     zerolinecolor = "black",
                     zerolinewidth = 2,
                     backgroundcolor = "white",
                     showbackground = TRUE,
                     mirror = TRUE),
        xaxis = list(autorange = "reversed",
                     title = "Bed complexity",
                     gridcolor = "white",
                     linecolor = "black",
                     zerolinecolor = "black",
                     zerolinewidth = 2,
                     backgroundcolor = "white",
                     showbackground = TRUE,
                     mirror = TRUE),
        zaxis = list(title = ztitle,
                     gridcolor = "white",
                     linecolor = "black",
                     backgroundcolor = "white",
                     showbackground = TRUE,
                     mirror = TRUE),
        aspectmode = "cube",
        camera = list(eye = list(
          x = 2, y = 2, z = 1.5
        ))
      )
    )
  intplot
}
