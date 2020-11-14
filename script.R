.libPaths("./renv/library/R-4.0/x86_64-w64-mingw32")
suppressPackageStartupMessages({
  library(rvest)
  library(stringr)
  library(purrr)
})

entities <- tibble::tribble(~name, ~url, ~path, ~known_offers, ~check_keine_angebote_text,
                            "bds", "https://www.bds-hamburg.de/unser-angebot/wohnungsangebote/", ".immobilielist .listitem a", c("055.1.012", "022.3.054"), TRUE,
                            "kaifu", "https://kaifu.de/index.php?id=14", ".getRight .kaifu_elem .kaifu_content a", c("106701", "102501", "105002", "105201"), FALSE,
                            "fuhle", "https://portal.immobilienscout24.de/ergebnisliste/15339103", ".result__list__element__infos--figcaption a", c("121803482", "123905365", "15339103"), TRUE,
                            "farmsen", "https://www.mgf-farmsen.de/de/vermietungen", ".immobilie .imm_top .imm_text a", c("055.1.012"), TRUE,
                            "VHW", "https://www.vhw-hamburg.de/wohnen/aktuelle-angebote.html", "section.searchResults--list a",  c("id-71104.1008.1008", "id-53201.12.1036"), TRUE,
                            "Harabau", "http://harabau.de/vermietung/wohnungen", "#homepage-mietangebote-content #subtitle", c(), TRUE,
                            "Süderelbe", "https://www.baugen-suederelbe.de/wohnungangebote/", ".pm-component__accomodation_link", c(), TRUE,
                            "BVE", "https://www.bve.de/wohnen-beim-bve/wohnungsbestand/wohnungsangebote/", ".contentWrapper span", c(), TRUE,
                            "Gartenstadt", "https://www.gartenstadt-hamburg.de/angebote", "#main-content", c(), TRUE,
                            "Hansa", "https://hansa-baugenossenschaft.de/wohnen/unsere-wohnungen", "#c635", c(), TRUE)

check_single <- function(row, possible_outcomes){
  
  url <- suppressWarnings(try(url(row$url, "rb"), silent = TRUE))
  if ("try-error" %in% class(url)) return(str_squish(geterrmessage()))
  page_html <- try(read_html(url), silent = TRUE)
  if ("try-error" %in% class(page_html)) return(str_squish(geterrmessage()))
  close(url)
  
  # row contains url, path, known_offers
  if(row$check_keine_angebote_text){
    nix_da <- page_html %>% html_text %>% str_to_lower() %>% 
      str_detect("keine (freien |aktuellen )?(wohnungs|miet)?(angebote|wohnungen)")
    if(nix_da){
      return(possible_outcomes$NOTHING)
    } 
  }
  
  offers <- page_html %>% html_nodes(row$path) %>% html_attr("href") %>% unique
  
  if(length(unlist(row$known_offers)) > 0){
    new <- str_subset(offers, str_flatten(fixed(unlist(row$known_offers)), "|"), negate = TRUE)
  }else{
    new <- offers
  }
  
  if (length(new) > 0){
    # open only if > 3 rooms
    text <- page_html %>% html_nodes(str_remove(row$path, " a$")) %>% html_text()
    interesting <- str_subset(text, "[1-3](,[05])?[ -]Zimmer", negate=T)
    if(length(interesting) > 0) return(possible_outcomes$CHECKITOUT) else return(possible_outcomes$ONLY_SMALL)
  }else{
    return(possible_outcomes$NOTHING_NEW)
  }
}

possible_outcomes <- list("CHECKITOUT" = "Es gibt was neues",
                          "NOTHING" = "Nichts da", 
                          "NOTHING_NEW" = "Nichts neues da", 
                          "ONLY_SMALL" = "Nur Mauselöcher")

# The actual search
entities$result <- map_chr(transpose(entities), possibly(check_single, otherwise = "UNKNOWN ERROR"), 
                           possible_outcomes)

# write errors or new offers to desktop always and results to Desktop once a week
write_to_desktop <- function(df, name){
  write.csv2(df, file.path(dirname(path.expand('~')),'Desktop', name), row.names = F, quote = F)
}

errors <- entities[!entities$result %in% unlist(possible_outcomes), c("name", "url", "result")]
to_be_checked <- entities[entities$result == possible_outcomes$CHECKITOUT, c("name", "url", "result")]
if(nrow(errors) > 0) write_to_desktop(errors, "ERRORS.txt")
if(nrow(to_be_checked) > 0) write_to_desktop(to_be_checked, "NEW_OFFERS.txt")
if(as.integer(format(Sys.Date(), "%d")) %% 7 == 0) write_to_desktop(entities[, c("result", "name", "url")], "WEEKLY_RESULTS.txt")
