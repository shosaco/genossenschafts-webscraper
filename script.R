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

# BDS hat Angebote, falls Immoliste mit neuen Angeboten befüllt ist
offers <- html_nodes(gatekeeper("https://www.bds-hamburg.de/unser-angebot/wohnungsangebote/"), ".immobilielist .listitem a") %>% as_list %>% purrr::map_chr(attr, "href") %>% unique
known <- "055.1.012"
new <- offers[!grepl(paste(known, collapse = "|"), offers)]
if (length(new) > 0){
  browseURL("https://www.bds-hamburg.de/unser-angebot/wohnungsangebote")
}

# Kaifu hat Angebote, falls in .getRight h2 neue Überschriften, die wir nicht kennen, stehen
offers <- unlist(as_list(html_nodes(gatekeeper("https://kaifu.de/index.php?id=14"), ".getRight .kaifu_elem .kaifu_content h2")))
new <- offers[!grepl("106701|102501|105002|105201", offers) & !grepl("[1-3] Zimmer", offers)]
if(length(new) > 0){
  browseURL("https://kaifu.de/index.php?id=14")
}


# Baugenossenschaft Fuhlsbüttel hat Angebote, falls der Link in result_list_... etwas anderes als das bekante Expose enthält
res <- as_list(html_nodes(gatekeeper("https://portal.immobilienscout24.de/ergebnisliste/15339103"), ".result__list__element__infos--figcaption a"))
links <- sapply(res, function(x) attr(x, "href"))
new <- links[!grepl("121803482", links)]
if (length(new) > 0){
  browseURL("https://portal.immobilienscout24.de/ergebnisliste/15339103")
}

# Harabau hat Angebote, falls der Text "keine ... verfügbar" nicht vorkommt
text <- html_nodes(gatekeeper("http://harabau.de/vermietung/wohnungen"), "#homepage-mietangebote-content #subtitle")  %>% html_text() %>% trimws()
if(text != "Keine aktuellen Mietangebote verfügbar"){
  browseURL("http://harabau.de/vermietung/wohnungen")
}

# Gartenstadt hat Angebote, falls der Text "keine Wohnungsangebote" nicht vorkommt
text <- html_nodes(gatekeeper("https://www.gartenstadt-hamburg.de/angebote/"), "#main-content")  %>% html_text() %>% trimws()
if(!grepl("keine Angebote zur Verfügung", text)){
  browseURL("https://www.gartenstadt-hamburg.de/angebote")
}

# Farmsen hat Angebote, falls sie andere Angebote als das bekannte haben
offers <- sapply(as_list(html_nodes(gatekeeper("https://www.mgf-farmsen.de/de/vermietungen"), ".immobilie .imm_top .imm_text a")), attr, "href")
new <- offers[!grepl("[obj=8|obj=12]", offers)]
if(length(new) > 0){
  browseURL("https://www.mgf-farmsen.de/de/vermietungen")
}

# BVE
offers <- as_list(html_nodes(gatekeeper("https://www.bve.de/wohnen-beim-bve/wohnungsbestand/wohnungsangebote/"), ".contentWrapper span")) %>% purrr::flatten()
if(length(offers) > 1 | offers[[1]] != "Zur Zeit sind leider keine Wohnungsangebote vorhanden."){
  browseURL("https://www.bve.de/wohnen-beim-bve/wohnungsbestand/wohnungsangebote/")
}

# Hansa
if(!grepl("Leider haben wir", html_nodes(gatekeeper("https://hansa-baugenossenschaft.de/wohnen/unsere-wohnungen"), "#c635") %>% html_text)){
  browseURL("https://hansa-baugenossenschaft.de/wohnen/unsere-wohnungen")
}

# VHW
offers <- html_nodes(gatekeeper("https://www.vhw-hamburg.de/wohnen/aktuelle-angebote.html"), ".searchResults--list--object--content h4 a") %>% as_list %>%  map_chr(attr, "href")
known <- c("id-53201.4.1096", "id-53201.12.1050", "id-14203.2014.1083")
new <- offers[!grepl(paste(known, collapse = "|"), offers)]
if (length(new) > 0){
  browseURL("https://www.vhw-hamburg.de/wohnen/aktuelle-angebote.html")
}

# WV1902
if(length(html_nodes(gatekeeper("https://www.wv1902.de/Wohnungen"), "#Inhalt table")) > 0){
  offers <- html_nodes(gatekeeper("https://www.wv1902.de/Wohnungen"), "#Inhalt table tr td a") %>% html_attr("href") %>% unique
  known <- "/Wohnungen/Bruno_Lauenroth_Weg_6/338"
  new <- offers[!grepl(paste(known, collapse = "|"), offers)]
  if (length(new) > 0) browseURL("https://www.wv1902.de/Wohnungen")
}

# Süderelbe
if(!grepl("keine freien Wohnungen", html_nodes(gatekeeper("https://www.baugen-suederelbe.de/wohnungangebote/"), ".page-content") %>% html_text())){
  browseURL("https://www.baugen-suederelbe.de/wohnungangebote/")
}
