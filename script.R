suppressPackageStartupMessages(library(rvest))

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
l <- length(html_nodes(gatekeeper("https://www.bds-hamburg.de/unser-angebot/wohnungsangebote/"), ".ce0 p.gross"))
if (l != 2){
  browseURL("https://www.bds-hamburg.de/unser-angebot/wohnungsangebote")
}

# Kaifu hat Angebote, falls in .getRight h2 neue Überschriften, die wir nicht kennen, stehen
offers <- unlist(as_list(html_nodes(gatekeeper("https://kaifu.de/index.php?id=14"), ".getRight .kaifu_elem .kaifu_content h2")))
new <- offers[!grepl("01-106003-66|12-108401-63|01-100801-62|12-105204-95|10-108301-24|01-100801-96", offers)]
if(length(new) > 0){
  browseURL("https://kaifu.de/index.php?id=14")
}


# Baugenossenschaft Fuhlsbüttel hat Angebote, falls der Link in result_list_... etwas anderes als das bekante Expose enthält
res <- as_list(html_nodes(gatekeeper("https://portal.immobilienscout24.de/ergebnisliste/15339103"), ".result__list__element__infos--figcaption a"))
links <- sapply(res, function(x) attr(x, "href"))
if (!all(grepl("/expose/15339103/107200431", links))){
  browseURL("https://portal.immobilienscout24.de/ergebnisliste/15339103")
}

# Harabau hat Angebote, falls der Text "keine ... verfügbar" nicht vorkommt
text <- html_nodes(gatekeeper("http://harabau.de/vermietung/wohnungen"), "#homepage-mietangebote-content #subtitle")  %>% html_text() %>% trimws()
if(text != "Keine aktuellen Mietangebote verfügbar"){
  browseURL("http://harabau.de/vermietung/wohnungen")
}

# Gartenstadt hat Angebote, falls der Text "keine Wohnungsangebote" nicht vorkommt
text <- html_nodes(gatekeeper("https://www.gartenstadt-hamburg.de/angebote/"), "#main")  %>% html_text() %>% trimws()
if(!grepl("Zur Zeit haben wir leider keine Wohnungsangebote", text)){
  browseURL("https://www.gartenstadt-hamburg.de/angebote")
}

# Farmsen hat Angebote, falls sie andere Angebote als das bekannte haben
offers <- sapply(as_list(html_nodes(gatekeeper("https://www.mgf-farmsen.de/de/vermietungen"), ".immobilie .imm_top .imm_text a")), attr, "href")
new <- offers[!grepl("[obj=8|obj=12]", offers)]
if(length(new) > 0){
  browseURL("https://www.mgf-farmsen.de/de/vermietungen")
}
