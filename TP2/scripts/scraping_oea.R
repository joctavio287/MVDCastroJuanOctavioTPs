#| warnings = FALSE
library(tidyverse)  # Manipulación de datos
library(rvest)      # Web scraping
library(httr2)      # Requests HTTP
library(here)       # Manejo de rutas de archivos
library(xml2)       # Manejo de HTML (guardar la página completa x ej)

# Creamos la carpeta /data si la misma no existe
data_dir <- here("TP2", "data")
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
  message("La carpeta 'data' ha sido creada.")
}else {
  message("La carpeta 'data' ya existe.")
}

# URL de la página a scrapear
meses_a_scrapear <- c(
  "1", # Enero
  "2", # Febrero
  "3", # Marzo
  "4"  # Abril
)
# Agrega esta línea justo antes del "for (month in meses_a_scrapear) {"
if (exists("comunicados")) rm(comunicados)
for (month in meses_a_scrapear) {
  pagina_html <- NULL
  url <- paste0(
    "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp?nMes=",
    month,
    "&nAnio=2026"
  )
  # Hacemos un sleep de 3 segundos
  Sys.sleep(3)

  # Realizamos un la lectura con try, si falla mandamos un mensaje
  tryCatch({
    pagina_html <- read_html(url)

    # Guardamos el html
    attr(pagina_html, "fecha_descarga") <- Sys.time()
    file_name <- paste0("comunicados_prensa_", month, "_2026.html")
    file_path <- here("TP2", "data", file_name)
    write_html(pagina_html, file = file_path)
    message(paste("Página del mes", month, "guardada exitosamente en:", file_path))

    # Extraemos los nodos de los títulos de las noticias
    nodos_titulos <- pagina_html |>
      html_elements(".itemmenulink") # Selector para títulos (tag "a" dentro de la class=post-title)

    # Nos quedamos con el texto de cada uno
    titulos <- nodos_titulos |>
      html_text2() |> # Extraemos el texto limpio de cada nodo
      str_trim()  # Limpiamos espacios en blanco

    # Sus urls
    urls <- nodos_titulos |>
      html_attr("href") # Accedemos al atributo href para obtener las urls de cada noticia

    comunicados_i <- tibble(
      titulo = titulos,
      url = paste0("https://www.oas.org/es/centro_noticias/", urls)
    )

    if (!exists("comunicados")) {
      comunicados <- comunicados_i
    } else {
      comunicados <- bind_rows(comunicados, comunicados_i)
    }

    message(paste("Mes", month, "procesado ok."))

  }, error = function(e) {
    message(paste("Error en el mes", month, ":", e$message))
  })
}

# Agregamos una columna de id para cada comunicado
comunicados <- comunicados |>
  mutate(id = row_number()) |>
  select(id, everything())

# Ahora scrapeamos las noticias de cada url
lista_textos <- list()
for (i in 1:nrow(comunicados)) {
  pagina_html <- NULL
  url_actual = comunicados$url[i]
  id_actual = comunicados$id[i]

  # Hacemos un sleep de 3 segundos
  Sys.sleep(3)

  # Realizamos un la lectura con try, si falla mandamos un mensaje
  tryCatch({
    noticia_html <- read_html(url_actual)

    # Guardamos el html
    attr(noticia_html, "fecha_descarga") <- Sys.time()
    file_name <- paste0("noticia_", id_actual, ".html")
    file_path <- here("TP2", "data", file_name)
    write_html(noticia_html, file = file_path)
    message(paste("Noticia guardada exitosamente en:", file_path))

    # Extraemos el texto y actualizamos la tabla
    texto_extraido <- noticia_html |>
      # html_elements("p:nth-child(5)") |>
      html_elements("#rightmaincol") |>
      html_elements("p") |>
      html_text2() |>
      str_trim() |>
      str_c(collapse = " \n ") # Unimos todo en una celda

    # Si el texto está vacío, probamos un selector más general
    if (length(texto_extraido) == 0) texto_extraido <- "Texto no encontrado"

    lista_textos[[i]] <- tibble(
      id = comunicados$id[i],
      cuerpo = texto_extraido
    )

  }, error = function(e) {
    message(paste("Error al scrapear la noticia en URL", url_actual, ":", e$message))
  })
}

# Unimos la tabla de comunicados con la de textos y removemos observaciones con cuerpos faltantes
resultados_texto <- bind_rows(lista_textos)

comunicados <- comunicados |>
  left_join(resultados_texto, by = "id") |>
  select(id, titulo, cuerpo) |>
  filter(!is.na(cuerpo), cuerpo != "Texto no encontrado", cuerpo != "")


# Guardamos la tabla global con la fecha
attr(comunicados, "fecha_descarga") <- Sys.time()
comunicados_path <- here("TP2", "data", paste0("comunicados_prensa_2026.rds"))
saveRDS(comunicados, comunicados_path)
message("Total de comunicados guardados: ", nrow(comunicados))
message(paste("Data frame de comunicados guardado en:", comunicados_path))