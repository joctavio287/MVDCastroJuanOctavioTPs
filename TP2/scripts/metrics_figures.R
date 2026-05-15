#| warnings = FALSE
library(tidyverse)
library(tidytext)
library(ggplot2)
library(here)
library(tm)


# Cargamos los datos con los lemas
output_dir <- here("TP2", "output")
lemas_path <- file.path(output_dir, "processed_text.rds")
if (!file.exists(lemas_path)) {
  message("El archivo 'processed_text.rds' no se encuentra en la carpeta 'output'. Correr el script de procesamiento para generar este archivo.")
} else {
  lemas <- readRDS(lemas_path)
}


# Construimos la matriz DTM

# Calculamos la frecuencia de los tokens
frecuencia_tokens <- lemas |> 
  count(id, lemma, name = "n")  |>  
  arrange(id)

# Convertimos el data frame a una Document-Term Matrix (formato ancho)
matriz_dtm <- frecuencia_tokens|> 
  cast_dtm(
    document = id, term = lemma, value = n 
  )

# Inspeccionamos la estructura de la matriz (es gigante)
# matriz_dtm

# Seleccionamos 5 términos de interés relacionados con la OEA y la Carta 
# Democrática Interamericana, que podrían ser relevantes para el análisis

# Democracia: Es el eje central de la Carta Democrática Interamericana.# 
# Derechos: Referido a los Derechos Humanos (CIDH).
# Seguridad: Relacionado con la seguridad hemisférica y multidimensional.
# Desarrollo: Específicamente el desarrollo integral de las naciones.
# Cooperación: La base de la relación diplomática entre los Estados miembros.
terminos_de_interes <- c(
  "democrático", "derecho", "seguridad", "desarrollo", "cooperación"
)
matriz_dtm_de_interes <- matriz_dtm[, colnames(matriz_dtm) %in% terminos_de_interes]
matriz_dtm_de_interes

as.matrix(matriz_dtm_de_interes)[1:20, ]


dtm_df <- as.data.frame(as.matrix(matriz_dtm_de_interes)) |>
  rownames_to_column(var = "id") |>
  pivot_longer(-id, names_to = "lemma", values_to = "n") |> 
  group_by(lemma) |> # Agrupamos por palabra
  summarise(frecuencia_total = sum(n)) # Sumamos todas las apariciones

# Graficamos la frecuencia de los términos de interés
term_freq_plot <- ggplot(dtm_df, aes(x = reorder(lemma, -frecuencia_total), y = frecuencia_total)) +
  # Añadimos un degradado sutil o un color más moderno y bordes redondeados (opcional)
  geom_col(fill = "#2c7fb8", width = 0.7) + 
  
  # Agregamos las etiquetas de datos sobre las barras para evitar que el lector tenga que "adivinar" el valor
  geom_text(aes(label = frecuencia_total), vjust = -0.5, size = 4.5, fontface = "bold", color = "#333333") +
  
  labs(
    title = "Frecuencia de Términos Institucionales",
    subtitle = "Análisis de términos clave en comunicados de la OEA (Enero, Febrero, Marzo, Abril 2026)",
    x = NULL, # Quitamos el título del eje X porque es obvio que son términos
    y = "Cantidad de apariciones",
    caption = "Fuente: comunicados de prensa de la OEA (https://www.oas.org/es/centro_noticias/comunicados_prensa.asp)"
  ) +
  
  # Ajustamos los límites del eje Y para que la etiqueta de la barra más alta no se corte
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  
  theme_minimal(base_size = 14, base_family = "sans") +
  
  theme(
    # Títulos más destacados
    plot.title = element_text(face = "bold", size = 18, color = "#1a1a1a"),
    plot.subtitle = element_text(size = 12, color = "#666666", margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "#999999", margin = margin(t = 20)),
    
    # Limpieza de ejes
    axis.text.x = element_text(angle = 0, size = 12, face = "bold", color = "#333333"), # Si son pocos términos, mejor ángulo 0
    axis.title.y = element_text(size = 11, color = "#666666"),
    
    # Eliminamos líneas innecesarias para un look más "limpio"
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#ebebeb")
  )

# Hacemos el mismo gráfico pero para los 10 términos más frecuentes en cambio
top_10_terms <- as.matrix(matriz_dtm) |> 
  colSums() |> 
  sort(decreasing = TRUE) |> 
  head(7) |> 
  names()

matriz_dtm_top_10 <- matriz_dtm[, colnames(matriz_dtm) %in% top_10_terms]
dtm_top_10_df <- as.data.frame(as.matrix(matriz_dtm_top_10)) |>
  rownames_to_column(var = "id") |>
  pivot_longer(-id, names_to = "lemma", values_to = "n")
top_10_plot <- ggplot(dtm_top_10_df, aes(x = reorder(lemma, -n), y = n)) +
  geom_col(fill = "#2c7fb8", width = 0.7) + 
  labs(
    title = "Frecuencia de los 7 Términos Más Frecuentes",
    subtitle = "Análisis de términos clave en comunicados de la OEA (Enero
, Febrero, Marzo, Abril 2026)",
    x = NULL,
    y = "Cantidad de apariciones",
    caption = "Fuente: comunicados de prensa de la OEA (https://www.o
as.org/es/centro_noticias/comunicados_prensa.asp)"
  ) +
  # Ajustamos los límites del eje Y para que la etiqueta de la barra más alta no se corte
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  
  theme_minimal(base_size = 14, base_family = "sans") +
  
  theme(
    # Títulos más destacados
    plot.title = element_text(face = "bold", size = 18, color = "#1a1a1a"),
    plot.subtitle = element_text(size = 12, color = "#666666", margin = margin(b = 20)),
    plot.caption = element_text(size = 9, color = "#999999", margin = margin(t = 20)),
    
    # Limpieza de ejes
    axis.text.x = element_text(angle = 0, size = 12, face = "bold", color = "#333333"), # Si son pocos términos, mejor ángulo 0
    axis.title.y = element_text(size = 11, color = "#666666"),
    
    # Eliminamos líneas innecesarias para un look más "limpio"
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#ebebeb")
  )


# Guardamos la figura en la carpeta output
output_dir <- here("TP2", "output")
if (!dir.exists(output_dir)) {
  message("La carpeta 'output' NO EXISTE, correr primero el preprocesamiento.")
} 
ggsave(
  filename = "frecuencia_terminos.png",
  plot = term_freq_plot,
  path = output_dir,
  width = 10,
  height = 6,
  dpi = 300
)
ggsave(
  filename = "frecuencia_top_10_terminos.png",
  plot = top_10_plot,
  path = output_dir,
  width = 10,
  height = 6,
  dpi = 300
)