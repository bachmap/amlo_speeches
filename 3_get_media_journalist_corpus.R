
########
#needed packages
########
library(rvest)
library(stringr)
library(dplyr)


#############################
# Scrape journalists Names
#############################

url_journalists <- "https://es.wikipedia.org/wiki/Categor%C3%ADa:Periodistas_de_M%C3%A9xico"

journalists <- url_journalists %>%
  read_html() %>%
  html_elements("div.mw-category div a") %>%
  html_text()

journalists_clean <- journalists %>%
  str_to_lower() %>%
  str_replace_all("[^a-záéíóúñü ]", "") %>%  # nur Buchstaben + Leerzeichen
  str_trim()

# drop other headlines
journalists_filtered <- journalists_clean %>%
  discard(~ .x == "" | str_detect(.x, "periodistas|columnistas|corresponsales|editorialistas|locutores|mujeres|siglo|estado|televisión|rock|lgbt")) %>%
  str_trim() %>%
  unique()

#check results
journalists_filtered

#############################
# Scrape Newspaper Names
#############################

url_media <- "https://es.wikipedia.org/wiki/Categor%C3%ADa:Peri%C3%B3dicos_de_M%C3%A9xico"

media <- url_media %>%
  read_html() %>%
  html_elements("div.mw-category div a") %>%
  html_text()

media_clean <- media %>%
  str_to_lower() %>%
  str_replace_all("[^a-záéíóúñü ]", "") %>%
  str_trim()

# drop other headlines
media_filtered <- media_clean %>%
  discard(~ .x == "" | str_detect(.x, "periódicos|diarios|grupo|anexo|organización|estado|inmigrantes|desaparecidos|digitales")) %>%
  str_trim() %>%
  unique()

media_filtered

#comibine data
media_targets <- unique(c(journalists_filtered, media_filtered)) %>%
  sort()


# manual add from https://en.wikipedia.org/wiki/List_of_newspapers_in_Mexico

manual_additions <- c(
  "elena poniatowska",
  "andrés roemer",
  "ángeles mastretta",
  "lydia cacho",
  "anabel hernández",
  "javier valdez cárdenas",
  "francisco ortiz franco",
  "miroslava breach",
  "regina martínez pérez",
  "tamara de anda",
  "proceso"
) %>% 
  str_to_lower() %>%
  str_replace_all("[^a-záéíóúñü ]", "") %>%
  str_trim()

#comibine data
media_targets2 <- unique(c(journalists_filtered, media_filtered, manual_additions)) %>%
  sort()

write_lines(media_targets2, "/Users/paulbachmann/Nextcloud/Amlo_paper/2_Data/media_targets.txt")

