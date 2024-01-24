library(tidyverse)
library(httr2)
library(rvest)
library(RSelenium)
library(wdman)
library(sf)

# Estrategia -----
#' Se pasa una  url base con los params de la busqueda
#' La web de zonaprop va a generar resultados paginados
#' Inicia en pagina 1 con resultados ordenados de mas reciente a mas viejo
#' Las paginas siguientes son secuenciales de 1 en 1 
#' Devuelve 20 resultados por pagina por lo que las paginas a revisar son
#' paginas = N resultados/20
#' Si se navega a una pagina N+1 mayor a la ultima pagina disponible N, recarga la pagina N
#' De cada pagina se pueden levantar los links a cada propiedad
#' Se arma la lista de links y se visita cada uno
#' Se captura precio, expensas si hay, direccion y caracteristicas resumidas de cada
# (Falta) Se calcula usd/m2, m2/amb, se parsea la direccion en texto y se extraen la coordenadas de la direccion link

# Codigo -----

## Setting selenium ---- 

con_que <- "patio"


# url base
url <- glue::glue("https://www.zonaprop.com.ar/departamentos-ph-venta-capital-federal-con-{con_que}-mas-de-2-ambientes-mas-50-m2-cubiertos-60000-125000-dolar-orden-publicado-descendente.html")

# correr docker
# driver <- rsDriver(port = 4567L, verbose = TRUE, browser = "chrome", chromever = "120.0.6099.225")
system("docker run --name selenium-server -d -p 4445:4444 selenium/standalone-firefox:2.53.1")

firefoxProfile  <- makeFirefoxProfile(list(
  "general.useragent.override" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:121.0) Gecko/20100101 Firefox/121.0"))

# inicializar driver
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4445L,
  browserName = "firefox", 
  extraCapabilities = firefoxProfile
)

# conectar a selenium
remDr$open()


## Root url scraping ---- 


# lista de sources
source_list <- list()

# lista de links de propiedades
property_links <- list()

# lista de resumen de propiedades
property_brief <- list()

# lista de resumen de propiedades
property_days_published <- list()


# navegar root url
remDr$navigate(url = url)

# Sys.sleep(.5)
# titulo x chequeo
remDr$getTitle()


# levanta html de la web
source_list[1] <- remDr$getPageSource()[[1]]

# total de propiedades 
total_resultados <- source_list[[1]] %>% 
  read_html() %>% 
  rvest::html_element(css = ".sc-1oqs0ed-0") %>% 
  rvest::html_text() %>% 
  str_extract(., "^[\\d|[:punct:]]*(?=\\s)") %>%
  gsub("\\D", "", .) %>%
  as.numeric()

# paginas a iterar
total_paginas <- ceiling(total_resultados/20)

# captura links de propiedades
property_links <- append(x = property_links,
                         values = source_list[[1]] %>% 
                           read_html() %>% 
                           rvest::html_elements(css = ".sc-i1odl-0") %>% 
                           rvest::html_attr("data-to-posting"))

property_brief <- append(x = property_brief,
                         values = source_list[[1]] %>% 
                           read_html() %>% 
                           rvest::html_elements(css = ".sc-i1odl-2") %>% 
                           rvest::html_text2() %>% 
                           str_split(., "\\n"))


property_days_published <- append(property_days_published, 
                                  values = source_list[[1]] %>% 
                                    read_html() %>% 
                                    rvest::html_elements(css = ".sc-i1odl-2") %>% 
                                    rvest::html_text2() %>% 
                                    str_extract_all(.,
                                                    "(?<=Publicado hace )\\d{1,3}") %>%
                                    as.numeric())

# paging_index  <- source_list[[1]] %>% 
#   read_html() %>% 
#   rvest::html_elements(css = ".sc-n5babu-1")

## Iteracion urls 2:N ---- 

for (i in 2:total_paginas) {
  
  url_i <- str_replace(url, pattern = "\\.html", glue::glue("-pagina-{i}.html"))
  
  remDr$navigate(url = url_i)
  
  # remDr$screenshot(display = F, file = glue::glue("screens/zonaprop-list-{i}.png"))
  
  
  # levanta html de la web
  source_list[i] <- remDr$getPageSource()[[1]]
  
  # write_file(source_list[[i]], file = glue::glue("sources/zonaprop-list-{i}.html"))
  
  # captura links de propiedades
  property_links <- append(x = property_links,
                           values = source_list[[i]] %>% 
                             read_html() %>% 
                             rvest::html_elements(css = ".sc-i1odl-0") %>% 
                             rvest::html_attr("data-to-posting"))
  
  # captura texto resumen de propiedades
  property_brief <- append(x = property_brief,
                           values = source_list[[i]] %>% 
                             read_html() %>% 
                             rvest::html_elements(css = ".sc-i1odl-2") %>% 
                             rvest::html_text2() %>% 
                             str_split(., "\\n"))
  
  # captura cuantos dias hace que se publico la propiedad
  property_days_published <- append(property_days_published, 
                          values = source_list[[i]] %>% 
                            read_html() %>% 
                            rvest::html_elements(css = ".sc-i1odl-2") %>% 
                            rvest::html_text2() %>% 
                            str_extract_all(.,
                                            "(?<=Publicado hace )\\d{1,3}") %>%
                            as.numeric())
  
  # si detecta que alcanzamos publicaciones mas viejas que nuestro limite para
  if (any(replace_na(unlist(property_days_published), 0) >= 180)) {
    break
  }
  
  # un sleep con distribucion poisson
  # Sys.sleep(rpois(n = 1, 2))
  
  print(glue::glue("pagina {i}: hecho"))
  
}

# guardo sources para control
write_rds(x = source_list,
          "sources/source_list.rds")

# tabla de links para iterar por propiedad
df_prop_resumen <- tibble(link = unlist(property_links),
                          dias_publicado = property_days_published,
                          resumen = property_brief)

# listas para guardar iteraciones
propiedades <- list()
propiedades_source <- list()

# incluir fecha de publicacion ojo con "Publicado hace 1 año"
for (i in 1:length(df_prop_resumen$link)) {
  
  link_i <- glue::glue("https://www.zonaprop.com.ar{df_prop_resumen$link[i]}")
  
  remDr$navigate(url = link_i)
  
  
  propiedades_source[i] <- remDr$getPageSource()[[1]]
  
  # precio
  prop_precio <- propiedades_source[[i]] %>% 
    read_html() %>% 
    rvest::html_elements(css = ".price-value") %>% 
    rvest::html_element("span") %>%
    rvest::html_text2() %>% 
    gsub("\\D", "", .) %>% 
    as.numeric()
  
  # expensas
  prop_expensas <- propiedades_source[[i]] %>% 
    read_html() %>% 
    rvest::html_elements(css = ".price-expenses") %>% 
    rvest::html_text2() %>% 
    gsub("\\D", "", .) %>% 
    as.numeric()
  
  # direccion texto
  prop_dir_txt <- propiedades_source[[i]] %>% 
    read_html() %>% 
    rvest::html_elements(css = ".section-location-property") %>% 
    rvest::html_text2()
  
  # direccion link
  prop_dir_link <- propiedades_source[[i]] %>% 
    read_html() %>% 
    rvest::html_elements(css = ".static-map") %>%
    rvest::html_attr("src")
  
  
  features_names <- propiedades_source[[i]] %>% 
    read_html() %>% 
    rvest::html_element(css = ".section-main-features") %>%
    rvest::html_elements("li") %>% 
    rvest::html_elements("i") %>% rvest::html_attr("class")
  
  features <- propiedades_source[[i]] %>% 
    read_html() %>% 
    rvest::html_element(css = ".section-main-features") %>%
    rvest::html_elements("li") %>% 
    rvest::html_text2()
  
  names(features) <- janitor::make_clean_names(features_names)
  
  features <- as.list(features)
  
  
  propiedades[[i]] <- list(link = link_i,
                           precio = prop_precio,
                           expensas = prop_expensas,
                           direccion_txt = prop_dir_txt,
                           direccion_link = prop_dir_link,
                           caracteristicas = features)
  
  print(glue::glue("propiedad {i}: capturada"))
  
  Sys.sleep(abs(rnorm(n = 1, 1, 1)))
  
}

propiedades_source %>% 
  write_rds("sources/propiedades_source.rds")

propiedades_df <- lapply(propiedades, unlist) %>%
  bind_rows() 

propiedades_df$publicado_desde <- as_vector(property_days_published)

propiedades_df <- propiedades_df %>% 
  filter(!is.na(precio))


propiedades_df <- propiedades_df %>% 
  mutate(precio = as.numeric(precio),
         across(c(caracteristicas.icon_ambiente, caracteristicas.icon_bano,
                  caracteristicas.icon_scubierta, caracteristicas.icon_stotal),
                \(x) as.numeric(gsub("\\D", "", x))),
         precio_m2cub = precio/caracteristicas.icon_scubierta,
         precio_m2total = precio/caracteristicas.icon_stotal,
         m2_amb = caracteristicas.icon_scubierta/caracteristicas.icon_ambiente,
         barrio = str_split_i(direccion_txt, ",", 2),
         lat = direccion_link %>% 
           str_extract_all(., "(?<=markers=).*(?=\\&key)") %>% 
           str_split(., ",") %>% 
           sapply(., function(x) x[1]),
         long = direccion_link %>% 
           str_extract_all(., "(?<=markers=).*(?=\\&key)") %>% 
           str_split(., ",") %>% 
           sapply(., function(x) x[2])) 

propiedades_df_geo <- propiedades_df %>%
  filter(!is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

mapa <- propiedades_df_geo %>% 
  select(-direccion_link) %>% 
  mapview::mapview(zcol = "precio_m2cub")

mapview::mapshot(mapa, url = glue::glue("mapa-{con_que}.html"),
                 file = "html")

propiedades_df_geo %>% 
  write_rds(glue::glue("resultados-geo-{con_que}.rds"))

propiedades_df %>% 
  select(link, publicado_desde,  barrio, direccion_txt,
         matches("caracteristicas"), precio, precio_m2cub,
         precio_m2total, expensas, m2_amb, lat, long) %>%
  write_rds(glue::glue("resultados-{con_que}.rds"))


system("docker stop selenium-server")
system("docker rm selenium-server")
