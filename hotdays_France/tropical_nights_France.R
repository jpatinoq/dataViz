# Datos climáticos para explorar el aumento de calor extremo en Francia
# Jorge Patiño
# 20260728

# Historical temperature data over France - Meteo France

# Datasets downloaded from:
# https://www.data.gouv.fr/datasets/donnees-changement-climatique-sim-quotidienne

# Le modèle de surface utilisé permet d’estimer les bilans d’eau et d’énergie en surface sur la
# France sur une grille régulière de 8 km. Cette modélisation est en fait effectuée via deux
# composants :

# SAFRAN (Système d’Analyse Fournissant des Renseignements Atmosphériques à la 
# Neige), qui est un schéma d’analyse qui permet d’obtenir des données pour 7
# paramètres atmosphériques au niveau de la surface et au pas de temps horaire :
# température et humidité à 2 m, vent à 10 m, rayonnements infrarouge et visible,
# précipitations liquides et solides.

# ISBA (Interaction Sol-Biosphère-Atmosphère), qui calcule les bilans d’eau, les bilans
# d’énergie et l’évolution du manteau neigeux.

# SAFRAN
# Le système SAFRAN originalement développé pour la prévision d’avalanche sur le massif
# alpin a été étendu et évalué sur la France métropolitaine. SAFRAN utilise à la fois les
# observations sol et altitude des réseaux français et une ébauche provenant d’un modèle de
# prévision numérique du temps.

# L’analyse SAFRAN est basée sur une méthode d’interpolation optimale à l’échelle de massifs,
# définis à partir d’un découpage de la France en zones climatologiquement homogènes. Les
# données sont dans un premier temps analysées par massif et par tranche d’altitude (par pas
# de 300 m) via une interpolation optimale (au pas de temps 06 h pour température, vent, 
# humidité et au pas de temps 24 h pour les précipitations).

# Les analyses ainsi obtenues sont ensuite interpolées pour obtenir une analyse horaire
# (température, vent, humidité, précipitations, nébulosité) et des données de rayonnement
# (infrarouge et visible) sont obtenues via l’utilisation d’un schéma de transfert radiatif.
# Une interpolation spatiale est enfin réalisée pour projeter ces données sur une grille régulière
# de 8 km. Les coordonnées indiquées de la maille correspondent au centre de la maille.

# LIBRARIES
library(tidyverse)
library(sf)
library(magick)
library(av)
library(mapview)
library(patchwork)

setwd("Docs/Testing/hotdays_France/")

# Inspect SAFRAN-ISBA data (France)
SHP_SIM_FRANCE = st_read("SAFRAN-ISBA/SHP_SIM_FRANCE.shp") %>%
  st_set_crs(4326)

# FUNCION ####
# Ajustar a Tmax = 30 para hot days, Tmin = 20 para tropical nights
# 1. Definir la ruta y listar los archivos que coinciden con el patrón
dir_path = "SAFRAN-ISBA/"
files = list.files(path = dir_path, pattern = "QUOT_SIM2.*", full.names = TRUE)

# 2. Definir la función de procesamiento por archivo
process_sim_file = function(file_path) {
  
  # Leer y seleccionar inicial
  df = read_delim(
    file = file_path,
    delim = ";",
    col_names = TRUE
  ) %>% 
    select(c(LAMBX, LAMBY, DATE, T, TINF_H, TSUP_H))
  
  # Procesamiento de variables y resumen
  df_processed = df %>% 
    mutate(
      Tx30 = if_else(TSUP_H >= 30, 1, 0),
      Tm20 = if_else(TINF_H >= 20, 1, 0),
      Tx30Tm20 = if_else(Tx30 == 1 & Tm20 == 1, 1, 0),
      Year = substr(as.character(DATE), 1, 4)
    ) %>%
    group_by(LAMBX, LAMBY, Year) %>% 
    summarise(
      Tx30 = sum(Tx30),
      Tm20 = sum(Tm20),
      Tx30Tm20 = sum(Tx30Tm20),
      .groups = "drop" # Evita que el dataframe quede agrupado para los siguientes pasos
    ) %>%
    # Filtrar años no deseados inmediatamente para ahorrar memoria en la combinación
    filter(!Year %in% c("1958", "2026"))
  
  return(df_processed)
}

# 3. Ejecutar el proceso sobre todos los archivos y unirlos en un solo dataframe
# map_df automáticamente combina los resultados de la función en una sola tabla
final_df = map_df(files, process_sim_file)


# DATOS PARA VIZ ####
datos = left_join(
  SHP_SIM_FRANCE,
  final_df,
  by = c("lambx" = "LAMBX", "lamby" = "LAMBY")
) %>% st_drop_geometry()

# Valores sobre Paris, Bordeaux, Marseille
# Identificar puntos para extraer valores
mapview(SHP_SIM_FRANCE)
# num_maille: 
# Paris: 1454, 1342, 1563, 1564
# Bordeaux: 7334, 7335
# Marseille: 9206

# seleccionar los valores más altos para 2025 en cada ciudad
pts_paris = c(1454, 1342, 1563, 1564)
pts_bordeaux = c(7334, 7335)
datos_paris_2025 = filter(datos, num_maille %in% pts_paris & Year == "2025")
datos_bordeaux_2025 = filter(datos, num_maille %in% pts_bordeaux & Year == "2025")

# selección: Paris: 1454, Bordeaux: 7335, Marseille: 9206
# datos de ciudades
datos_ciudad = filter(datos, num_maille %in% c(1454, 7335, 9206))
# add city name
datos_ciudad = datos_ciudad %>%
  mutate(
    city = case_when(
      num_maille == 1454 ~ "Paris",
      num_maille == 7335 ~ "Bordeaux",
      num_maille == 9206 ~ "Marseille"
    )
  ) %>% 
  mutate(
    city = factor(city, levels = c("Paris", "Bordeaux", "Marseille"))
  )

# VISUALIZACION ####
# Mapa con plot de barras por ciudad

# Función para plot y guardar un año, mapa + barras ciudad 
plot_year = function(year, var){
  var_name = switch(
    var,
    "Tx30" = "hot days",
    "Tm20" = "tropical nights",
    "Tx30Tm20" = "hot days and tropical nights",
    var
  )
  
  lmax = switch(
    var,
    "Tx30" = 91,
    "Tm20" = 113,
    "Tx30Tm20" = 74,
    var
  )
  
  # mapa
  m = ggplot(filter(datos, Year == year)) + 
    geom_point(aes(col = .data[[var]], x = lon_dg, y = lat_dg), size = 2.75, alpha = 0.9, shape = 15) + 
    scale_color_continuous(
      palette = c(
        # "#001219",
        # "#005f73",
        "#0a9396", 
        "#94d2bd",
        "#e9d8a6",
        "#ee9b00",
        "#ca6702",
        "#bb3e03", 
        "#ae2012",
        "#9b2226"),
      limits = c(0, lmax),
      name = NULL
    ) + 
    # add Paris
    geom_point(x = 2.35, y = 48.85, size = 7, shape = 21, color = "black", fill = NA, stroke = 1.5) + 
    annotate(geom = "text", x = 3, y = 49.1, label = "Paris", size = 10, color = "black", alpha = 1) +
    # add Bordeaux
    geom_point(x = -0.58, y = 44.84, size = 7, shape = 21, color = "black", fill = NA, stroke = 1.5) + 
    annotate(geom = "text", x = 0.5, y = 45.1, label = "Bordeaux", size = 10, color = "black", alpha = 1) +
    # add Marseille
    geom_point(x = 5.37, y = 43.30, size = 7, shape = 21, color = "black", fill = NA, stroke = 1.5) + 
    annotate(geom = "text", x = 6.4, y = 43.55, label = "Marseille", size = 10, color = "black", alpha = 1) +
    theme_void() + 
    labs(
      title = paste0("Annual ", var_name, " (", var, ")"),
      subtitle = year,
      caption = "Source: Meteo France, SAFRAN-ISBA dataset."
    ) + 
    theme(
      plot.title = element_text(size = rel(3), face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = rel(3), hjust = 0.5,  margin = margin(b = 150)),
      plot.caption = element_text(size = rel(1.5)),
      legend.position = "bottom",
      legend.key.height = unit(3, "mm"),
      legend.key.width = unit(30, "mm"),
      legend.text = element_text(size = rel(2)),
      plot.background = element_rect(fill = "white")
    )
  
  # barras de ciudades
  datos_ciudad[[var]][datos_ciudad$Year > year] = NA
  
  p_paris = ggplot(datos_ciudad) + 
    geom_col(aes(x = as.numeric(Year), y = 2, fill = .data[[var]]), width = 0.95) + 
    scale_fill_continuous(
      palette = c(
        # "#001219",
        # "#005f73",
        "#0a9396", 
        "#94d2bd",
        "#e9d8a6",
        "#ee9b00",
        "#ca6702",
        "#bb3e03", 
        "#ae2012",
        "#9b2226"),
      limits = c(0, lmax),
      na.value = "gray90",
      name = NULL
    ) + 
    scale_x_continuous(
      # limits = c(1935, 2025),
      breaks = c(1959, 1980, 2000, 2025), 
      labels = c("1959", "1980", "2000", "2025")
    ) + 
    # annotate(geom = "text", x = 1940, y = 1, label = "Paris", size = 10, color = "black", alpha = 1) +
    theme_minimal() + 
    facet_wrap(~city, nrow = 3, strip.position = "top") + 
    theme(
      plot.title = element_text(size = rel(5), hjust = 0),
      legend.position = "none",
      plot.background = element_rect(fill = "white"),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = rel(1.5)), 
      panel.grid = element_blank(),
      margins = margin(t = 1, b = 1, r = 0, l = 0),
      strip.text = element_text(size = rel(2))
    )
  v = m + inset_element(p_paris, left = 0.1, bottom = 0.77, right = 0.9, top = 0.92, align_to = "full")
  
  ggsave(plot = v, paste0("plots/", var, "-", year, ".png"))
}

# test función
plot_year(year = "1974", var = "Tm20")

# plot todos los años
years = unique(final_df$Year)

for(year in years){
  plot_year(year = year, var = "Tm20")
  cat(year, "saved\n")
}

# Crear video con av ####
# cargar frames
frames = list.files(
  "plots",
  pattern = "\\.png$",
  full.names = TRUE
)

# ordenar
frames = sort(frames)
length(frames)

# crear video mp4
av::av_encode_video(
  input = frames, 
  framerate = 4,
  output = 'plots/tm20-1959-2025-FR.mp4'
  )




 # END