
suppressPackageStartupMessages({
  library(rvest)
  library(stringr)
})

entities <- tibble::tribble(~name, ~url, ~path, ~known_offers,
                            "bds", "https://www.bds-hamburg.de/unser-angebot/wohnungsangebote/", ".immobilielist .listitem a", c("055.1.012", "022.3.054"),
                            "kaifu", "https://kaifu.de/index.php?id=14", ".getRight .kaifu_elem .kaifu_content a", c("106701", "102501", "105002", "105201"),
                            "fuhle", "https://portal.immobilienscout24.de/ergebnisliste/15339103", ".result__list__element__infos--figcaption a", c("121803482", "123905365", "15339103"),
                            "farmsen", "https://www.mgf-farmsen.de/de/vermietungen", ".immobilie .imm_top .imm_text a", c("055.1.012"),
                            "VHW", "https://www.mgf-farmsen.de/de/vermietungen", ".immobilie .imm_top .imm_text a",  c("id-53201.4.1096", "id-53201.12.1050", "id-14203.2014.1083", "id-12215.32", "id-71102.3", "id-53201.12", "id-71104.1010"),
                            "Harabau", "http://harabau.de/vermietung/wohnungen", "#homepage-mietangebote-content #subtitle", c(),
                            "Süderelbe", "https://www.baugen-suederelbe.de/wohnungangebote/", ".page-content", c(),
                            "BVE", "https://www.bve.de/wohnen-beim-bve/wohnungsbestand/wohnungsangebote/", ".contentWrapper span", c(),
                            "Gartenstadt", "https://www.gartenstadt-hamburg.de/angebote", "#main-content", c(),
                            "Hansa", "https://hansa-baugenossenschaft.de/wohnen/unsere-wohnungen", "#c635", c())


check_single <- function(row){
  print(paste("checking", row$name))
  # row contains url, path, known_offers
  nix_da <- read_html(row$url) %>% html_text %>% str_to_lower() %>% 
    str_detect("keine (freien |aktuellen )?(wohnungs|miet)?(angebote|wohnungen)")
  if(nix_da) return()
  offers <- read_html(row$url) %>% html_nodes(row$path) %>% html_attr("href") %>% unique

  if(length(unlist(row$known_offers)) > 0){
    new <- str_subset(offers, str_flatten(fixed(unlist(row$known_offers)), "|"), negate = TRUE)
  }else{
    new <- offers
  }
  
  if (length(new) > 0){
    # open only if > 3 rooms
    text <- read_html(row$url) %>% html_nodes(str_remove(row$path, " a$")) %>% html_text()
    interesting <- str_subset(text, "[1-3][ -]Zimmer", negate=T)
    if(length(interesting) > 0) browseURL(row$url)
    
  }
}

purrr::walk(purrr::transpose(entities), check_single)
