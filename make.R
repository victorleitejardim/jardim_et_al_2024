#' @author Victor Leite Jardim
#' 
#' @date 2022/12/02

## Install Dependencies (listed in DESCRIPTION) ----
remotes::install_deps(upgrade = "never")

## Run Project ----
targets::tar_make()
quarto::quarto_render(input = "index.qmd", output_file = "index.html")


