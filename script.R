suppressPackageStartupMessages(library(checkpoint))
checkpoint("2020-01-01", checkpointLocation = "/", verbose = FALSE)
library(rvest)


gatekeeper <- function(url){
  webpage <- tryCatch(read_html(url),
                      error = function(e) cat(paste0("Fehler bei '", url, "': ", e)))
  if(is.null(webpage)){
    stop(paste0("Fehler bei '", url, "'"))
  }else{
    return(webpage)
  }
}

# BGS Fuhlsbüttel hat Angebote, falls der Text "keine ... verfügbar" nicht vorkommt
text <- html_nodes(gatekeeper("https://www.bds-hamburg.de/unser-angebot/wohnungsangebote/"), ".immobilielist") %>% html_text() %>% trimws()
if(text != "Zurzeit haben wir leider keine Angebote für Sie. Schauen Sie gern mal wieder vorbei. Wir würden uns freuen."){
  message("BDS hat Angebote!")
  print(text)
  print("Zurzeit haben wir leider keine Angebote für Sie. Schauen Sie gern mal wieder vorbei. Wir würden uns freuen.")
}


# Kaifu hat Angebote, falls in .getRight irgendetwas steht
text <- html_nodes(gatekeeper("https://kaifu.de/index.php?id=14"), ".getRight") %>% html_text() %>% trimws()
if(text != ""){
  message("Kaifu hat Angebote!")
}


# BGS Fuhlsbüttel hat Angebote, falls der Link in result_list_... etwas anderes als das bekante Expose enthält
results <- as_list(html_nodes(gatekeeper("https://portal.immobilienscout24.de/ergebnisliste/15339103"), ".result__list__element__infos--figcaption a"))
links <- sapply(results, function(x) attr(x, "href"))
if (!all(grepl("/expose/15339103/105323205/", links))){
  message("Baugenossenschaft Fuhlsbüttel hat Angebote!")
}

# BGS Fuhlsbüttel hat Angebote, falls der Text "keine ... verfügbar" nicht vorkommt
text <- html_nodes(gatekeeper("http://harabau.de/vermietung/wohnungen"), "#homepage-mietangebote-content #subtitle")  %>% html_text() %>% trimws()
if(text != "Keine aktuellen Mietangebote verfügbar"){
  message("HAMBURG-RAHLSTEDTER BAUGENOSSENSCHAFT hat Angebote!")
}

# BGS Fuhlsbüttel hat Angebote, falls der Text "keine Wohnungsangebote" nicht vorkommt
text <- html_nodes(gatekeeper("https://www.gartenstadt-hamburg.de/angebote/"), "#main")  %>% html_text() %>% trimws()
if(!grepl("Zur Zeit haben wir leider keine Wohnungsangebote", text)){
  message("Gartenstadt Hamburg eG hat Angebote!")
}

# BGS Fuhlsbüttel hat Angebote, falls sie andere Angebote als das bekannte haben
text <- html_nodes(gatekeeper("https://www.mgf-farmsen.de/de/vermietungen"), ".immobilie .imm_top .imm_text h2") %>% html_text() %>% trimws()
if(text != "Gut angebundene 1,5-Zimmer-Wohnung in Hamburg-Farmsen"){
  message("MGF Gartenstadt hat Angebote!")
}
