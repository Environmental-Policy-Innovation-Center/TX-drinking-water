# testing parallax scrolling 
# devtools::install_github("martinctc/parallaxr")
## Load packages
library(parallaxr)

## Character vector of all MD files
all_md_str <- list.files(path = "./results/markdown", full.names = TRUE)

## Loop through each MD file, parse, and return a single tibble
md_tibble <- all_md_str %>%
  purrr::map_dfr(parse_md) # Return a tibble with row-binding

## Output HTML file
css <- readLines("./results/parallax.css") %>% 
  paste(collapse = "\n")
generate_scroll_doc(path = "results/tx_viol_interactive_parallax.html",
                    inputs = md_tibble, 
                    custom_css = css)



