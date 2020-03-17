suppressPackageStartupMessages(library(rvest))

gatekeeper <- function(url){
  webpage <- tryCatch(read_html(url),
                      error = function(e) cat(paste0("Fehler bei '", url, "': ", e)))
  if(is.null(webpage)){
    stop("Fehler beim Aufrufen der Webseite", call. = F)
  }else{
    return(webpage)
  }
}

# BGS Fuhlsbüttel hat Angebote, falls es mehr als 2 die 2 bekannten Absätze gibt
text_hp <- sapply(html_nodes(gatekeeper("https://www.bds-hamburg.de/unser-angebot/wohnungsangebote/"), ".ce0 p.gross"), html_text)

if (sum(grepl("Sie sind auf der Suche|Aufgrund der vielen", text_hp)) > 2){
  browseURL("https://www.bds-hamburg.de/unser-angebot/wohnungsangebote")
}

# Kaifu hat Angebote, falls in .getRight h2 neue Überschriften, die wir nicht kennen, stehen
offers <- unlist(as_list(html_nodes(gatekeeper("https://kaifu.de/index.php?id=14"), ".getRight .kaifu_elem .kaifu_content h2")))
new <- offers[!grepl("102401", offers) & !grepl("2 Zimmer", offers)]
if(length(new) > 0){
  browseURL("https://kaifu.de/index.php?id=14")
}


# Baugenossenschaft Fuhlsbüttel hat Angebote, falls der Link in result_list_... etwas anderes als das bekante Expose enthält
res <- as_list(html_nodes(gatekeeper("https://portal.immobilienscout24.de/ergebnisliste/15339103"), ".result__list__element__infos--figcaption a"))
links <- sapply(res, function(x) attr(x, "href"))
new <- links[!grepl("116198180|", links)]
if (length(new) > 0){
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
