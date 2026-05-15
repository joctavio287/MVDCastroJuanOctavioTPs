#| warnings = FALSE
library(tidyverse)
library(tidytext)
library(udpipe)
library(here)

# Creamos la carpeta /output si la misma no existe
data_dir <- here("TP2", "data")
comunicados_path = file.path(data_dir, "comunicados_prensa_2026.rds")
output_dir <- here("TP2", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message("La carpeta 'output' ha sido creada.")
}else {
  message("La carpeta 'output' ya existe.")
}

# Chequeamos que exista la carpeta data y el archivo con los comunicados, si no existe el archivo, le indicamos al usuario que ejecute el script de scraping
if (!dir.exists(data_dir) | !file.exists(comunicados_path)) {
  message("El archivo 'comunicados_prensa_2026.rds' no se encuentra en la carpeta 'data'. Por favor, ejecute el script de scraping para generar este archivo.")
} else {
  comunicados_prensa <- readRDS(comunicados_path)
}

# LIMPIEZA
# Limpiamos el texto de cada comunicado de prensa
comunicados_prensa_clean <- comunicados_prensa |> 
  mutate(
    # Reemplazamos saltos de línea y tabulaciones por espacios
    texto_limpio = str_replace_all(cuerpo, "[\\r\\n\\t]+", " "),  
    # Eliminamos comillas y otros caracteres especiales
    texto_limpio = str_replace_all(texto_limpio, "[\\\"'“”‘’«»`´%()]", ""),  
    # Pasamos todo a minúscula
    texto_limpio = str_to_lower(texto_limpio),
    # Eliminamos espacios múltiples
    texto_limpio = str_squish(texto_limpio)  
  ) |> 
  # Eliminamos la columna original del cuerpo del comunicado
  select(-c(cuerpo)) 


# ESTADISTICAS DESCRIPTIVAS
# Algunas estadisticas descriptivas del corpus de texto
estadisticas <- comunicados_prensa_clean |>
  mutate(n_caracteres = nchar(texto_limpio)) |>
  summarise(
    promedio_caracteres = mean(n_caracteres, na.rm = TRUE),
    mediana_caracteres = median(n_caracteres, na.rm = TRUE),
    min_caracteres = min(n_caracteres, na.rm = TRUE),
    max_caracteres = max(n_caracteres, na.rm = TRUE),
    total_noticias = n()
  ) |> 
  pivot_longer(
    cols=everything(),
    names_to="Métricas", 
    values_to="Valores"
  ) |> 
  # Transformamos la columna final a entero
  mutate(Valores = as.integer(Valores))

message("Estadísticas descriptivas del corpus de texto:\n")
print(estadisticas)


# LEMATIZACIÓN 
if (!dir.exists(here("TP2", "models"))) {
  dir.create(here("TP2", "models"))
  message("La carpeta 'models' ha sido creada.")
} else {
  message("La carpeta 'models' ya existe.")
}

# Descarga y carga el modelo de lematización en español
m_es <- udpipe_download_model(
  language = "spanish", 
  model_dir = here("TP2", "models"),
  overwrite = FALSE)
modelo_es <- udpipe_load_model(m_es$file_model)

# Lematiza el texto completo
noticias_lemas <- udpipe_annotate(
  modelo_es, 
  x = comunicados_prensa_clean$texto_limpio, 
  doc_id = comunicados_prensa_clean$id
) |>
  as.data.frame() |>
  mutate(id=as.integer(doc_id)) |> 
  select(id, lemma, upos) 

# Agregamos los titulares, filtramos sustantivos, verbos y adjetivos, eliminamos la columna upos y pasamos los lemas a minúscula
noticias_lemas <- noticias_lemas |> 
  left_join(
    comunicados_prensa_clean |> select(id, titulo), 
    by = "id"
  ) |> 
  filter(upos %in% c("NOUN", "ADJ", "VERB")) |> 
  select(-upos) |> 
  mutate(lemma=str_to_lower(lemma))


# REMOVEMOS STOPWORDS
# Descargamos stopwords
data("stopwords", package = "stopwords", overwrite=FALSE)
stop_es <- stopwords::stopwords("es")
stop_en <- stopwords::stopwords("en")
stop_words <- tibble(lemma = c(stop_es, stop_en))

numero_de_palabras <- nrow(noticias_lemas)

# Eliminamos las stop words
noticias_lemas <- noticias_lemas |>
  anti_join(stop_words, by = "lemma") |>
  # También eliminamos números y palabras de una menos de 2 letras
  filter(
    !str_detect(lemma, "^\\d+$"), 
    nchar(lemma) > 2               
  )

# Comparamos: antes y después
message("Tokens antes de eliminar stop words:", numero_de_palabras, "\n")
message("Tokens después de eliminar stop words:", nrow(noticias_lemas), "\n")
message("Reducción relativa porcentual:", 
  round(100 * ((numero_de_palabras - nrow(noticias_lemas)) / numero_de_palabras),1), 
  "%\n"
)

# FILTRADO POR CANTIDAD DE PALABRAS
# Identificamos los IDs que tienen 10 o más palabras
ids_validos <- noticias_lemas |>
  group_by(id) |>
  summarise(n_tokens = n()) |>
  filter(n_tokens >= 20) |>
  pull(id)

# Contamos cuántos eliminamos para informar al usuario
n_antes <- length(unique(noticias_lemas$id))
noticias_lemas <- noticias_lemas |> 
  filter(id %in% ids_validos)
n_despues <- length(unique(noticias_lemas$id))

message("Comunicados eliminados por tener menos de 10 palabras: ", n_antes - n_despues)

# Computamos las cantidad de palabras por noticia (valor mínimo, mediana, media, máximo y desviación estándar)
estadisticas_palabras <- noticias_lemas |>
  group_by(id) |>
  summarise(n_palabras = n()) |>
  summarise(
    promedio_palabras = mean(n_palabras, na.rm = TRUE),
    mediana_palabras = median(n_palabras, na.rm = TRUE),
    min_palabras = min(n_palabras, na.rm = TRUE),
    max_palabras = max(n_palabras, na.rm = TRUE),
    sd_palabras = sd(n_palabras, na.rm = TRUE)
  ) |> 
  pivot_longer(
    cols=everything(),
    names_to="Métricas", 
    values_to="Valores"
  ) |> 
  # Transformamos la columna final a entero
  mutate(Valores = as.integer(Valores))



# GUARDAMOS LOS RESULTADOS
attr(noticias_lemas, "estadisticas") <- estadisticas
attr(noticias_lemas, "estadisticas_palabras") <- estadisticas_palabras
attr(noticias_lemas, "fecha_de_ejecucion") <- Sys.Date()
saveRDS(noticias_lemas, file = here("TP2", "output", "processed_text.rds"))
message("El archivo 'processed_text.rds' ha sido guardado en la carpeta 'output'.")

