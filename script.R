# .Rprofile activates renv -> workaround if this script is run from different directory
if(!any(grepl("renv", .libPaths()))){
  this_file = gsub("--file=", "", commandArgs()[grepl("--file", commandArgs())])
  wd <- paste(head(strsplit(this_file, '[/|\\]')[[1]], -1), collapse = .Platform$file.sep)
  source(file.path(wd, "renv/activate.R"))
}

suppressPackageStartupMessages({
  library(rvest)
  library(stringr)
  library(purrr)
  library(dplyr)
})

entities <- tibble::tribble(~name, ~url, ~path, ~known_offers, ~check_keine_angebote_text,
                            "bds", "https://www.bds-hamburg.de/unser-angebot/wohnungsangebote/", ".immobilielist .listitem a", c("055.1.012"), TRUE,
                            "kaifu", "https://kaifu.de/index.php?id=14", ".getRight .kaifu_elem .kaifu_content a", c(), FALSE,
                            "fuhle", "https://portal.immobilienscout24.de/ergebnisliste/15339103", ".result__list__element__infos--figcaption a", c(), TRUE,
                            "farmsen", "https://www.mgf-farmsen.de/de/vermietungen", ".immobilie .imm_top .imm_text a", c(), TRUE,
                            "VHW", "https://www.vhw-hamburg.de/wohnen/aktuelle-angebote.html", "section.searchResults--list a",  c(), TRUE,
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
      str_detect("keine (freien |aktuellen )?(wohnungs|miet)?(angebote|wohnung)")
    if(nix_da){
      return(possible_outcomes$NOTHING)
    } 
  }
  
  offers <- page_html %>% html_nodes(row$path) %>% html_attr("href") %>% unique
  if(length(offers) == 0){
    return(possible_outcomes$NOTHING)
  }
  
  if(length(unlist(row$known_offers)) > 0){
    new <- str_subset(offers, str_flatten(fixed(unlist(row$known_offers)), "|"), negate = TRUE)
  }else{
    new <- offers
  }
  
  if (length(new) > 0){
    # open only if > 3 rooms
    text <- page_html %>% html_nodes(str_remove(row$path, " a$")) %>% html_text()
    interesting <- str_subset(text, "Drei|[1-3]([.,][05])?[ -]Zimmer", negate=T)
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
write_to_desktop <- function(df, name = str_glue("{f}.txt", f = str_to_upper(deparse(substitute(df))))){
  if(nrow(df) > 0){
    fileConn <- file(file.path(dirname(path.expand('~')),'Desktop', name))
    writeLines(knitr::kable(df), fileConn)
    close(fileConn)
  }
}

for_output <- entities %>% select(name, result, url)
errors <- for_output %>% filter(! result %in% unlist(possible_outcomes))
offers <- for_output %>% filter(result == possible_outcomes$CHECKITOUT)
write_to_desktop(errors)
write_to_desktop(offers)
if(as.integer(format(Sys.Date(), "%d")) %% 7 == 0) write_to_desktop(for_output, "WEEKLY_RESULTS.txt")
