#' fauna.compart
#'
#' @param data 
#' @param compartment string containing the name of the compartment: "Epifauna", "Endofauna" or "Insterstice"
#'
#' @return
#' @export
#'
#' @examples
fauna.compart <- function(data, compartment){
  samples_compart <- data %>%
    filter(Position == compartment) %>% 
    distinct(Site, Year, Point, Season, Replicate) %>%
    group_by(Site, Point, Season, Year) %>%
    summarise(count = n()) %>%
    ungroup()
  
  compart <- data %>% 
    filter(Position == compartment) %>%
    select(Point, Site, Replicate, Year, Season, Date, Species, Position, Abundance) %>% 
    mutate(Year = as.factor(Year)) %>% 
    group_by(Point, Site, Year, Season, Species) %>%
    summarise(Abundance =  sum(Abundance)) %>% 
    ungroup() %>% 
    spread(key = Species, value = Abundance, fill = 0) %>% 
    mutate_if(is.numeric, funs(./(samples_compart$count*0.3))) %>% 
    set_rownames(paste(.$Point, .$Year, sep = "_"))
  
  
  return(compart)
}
