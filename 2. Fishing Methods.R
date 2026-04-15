# ============================================================
# ANÁLISIS ENCUESTAS PESCADORES - RNDN
# ============================================================

# Instalar y cargar librerías
install.packages(c("readxl", "dplyr", "ggplot2", "tidyr", "janitor", "car", "dunn.test"))

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(janitor)
library(car)
library(dunn.test)
library(stringr)

# ============================================================
# 1. CARGAR Y COMBINAR LAS 4 PESTAÑAS
# ============================================================

# AQUI cambia segun tu ubicacion
ruta <- "E:/Kelly/To Read and Work/2025_SPDA/Dorsal de Nazca/Paper Caracterizacion Pesquera/encuesta_RNDN_ALL DATA.xlsx"

# Leer cada pestaña con TODAS las columnas como texto
# "San Andrés" se asigna
pucusana <- read_excel(ruta, sheet = "Pucusana", col_types = "text") %>% mutate(Localidad = "Pucusana")
marcona  <- read_excel(ruta, sheet = "Marcona",  col_types = "text") %>% mutate(Localidad = "Marcona")
andres   <- read_excel(ruta, sheet = "Andres",   col_types = "text") %>% mutate(Localidad = "San Andrés")
lomas    <- read_excel(ruta, sheet = "Lomas",    col_types = "text") %>% mutate(Localidad = "Lomas")

# Combinar las 4 pestañas en un solo dataframe
datos <- bind_rows(pucusana, marcona, andres, lomas)

# Convertir columnas numéricas
columnas_numericas <- c("Edad", "Años_Pesca", "Años_Altura", "Personas_Dependientes",
                        "Numero_hijos", "Num_Embarcaciones", "Num_Tripulantes1", 
                        "Num_Tripulantes2", "Galones_Combustible",
                        "Kg_Lance1", "Kg_Faena1", "Precio_Kg1",
                        "Kg_Lance2", "Kg_Faena2", "Precio_Kg2",
                        "Dias_Faena1", "Dias_Faena2", "Dias_Faena3",
                        "Viajes_Temporada1", "Viajes_Temporada2", "Viajes_Temporada3")

datos <- datos %>%
  mutate(across(all_of(columnas_numericas), as.numeric))

# ============================================================
# 2. LIMPIEZA BÁSICA
# ============================================================

datos <- datos %>% clean_names()

# Definir orden de puertos
datos <- datos %>%
  mutate(localidad = factor(localidad, 
                            levels = c("Pucusana", "Marcona", "San Andrés", "Lomas")))

# Verificar carga correcta
glimpse(datos)
datos %>% count(localidad)
# Pucusana:   42
# Marcona:    33
# San Andrés: 23
# Lomas:      15


# Verificar datos
unique(datos$recurso_1)
unique(datos$recurso_2)
unique(datos$recurso_3)


# ============================================================
#              ANÁLISIS DE RECURSOS DE PESCA DE ALTURA
# ============================================================

# ============================================================
# 1. EXPANDIR RECURSOS - Detecta cada recurso de interés independientemente
# ============================================================

expandir_recursos_altura <- function(df, col_recurso) {
  df %>%
    mutate(
      recurso_raw = {{ col_recurso }},
      # Convertir "NA" texto a NA real
      recurso_raw = ifelse(recurso_raw == "NA", NA, recurso_raw),
      # Detectar cada recurso independientemente
      es_bonito   = str_detect(recurso_raw, regex("bonito", ignore_case = TRUE)),
      es_pota     = str_detect(recurso_raw, regex("pota", ignore_case = TRUE)),
      es_perico   = str_detect(recurso_raw, regex("perico", ignore_case = TRUE)),
      es_ova      = str_detect(recurso_raw, regex("ova", ignore_case = TRUE)),
      es_tiburon  = str_detect(recurso_raw, regex("tollo|espada|tiburón|tiburon", ignore_case = TRUE)),
      es_caballa  = str_detect(recurso_raw, regex("caballa", ignore_case = TRUE)),
      es_jurel    = str_detect(recurso_raw, regex("jurel", ignore_case = TRUE)),
      es_bacalao  = str_detect(recurso_raw, regex("bacalao", ignore_case = TRUE))
    ) %>%
    filter(!is.na(recurso_raw)) %>%
    tidyr::pivot_longer(
      cols      = c(es_bonito, es_pota, es_perico, es_ova, es_tiburon,
                    es_caballa, es_jurel, es_bacalao),
      names_to  = "recurso_flag",
      values_to = "presente"
    ) %>%
    filter(presente == TRUE) %>%
    mutate(recurso = case_when(
      recurso_flag == "es_bonito"  ~ "Bonito",
      recurso_flag == "es_pota"    ~ "Pota",
      recurso_flag == "es_perico"  ~ "Perico",
      recurso_flag == "es_ova"     ~ "Ova de pez volador",
      recurso_flag == "es_tiburon" ~ "Tiburón",
      recurso_flag == "es_caballa" ~ "Caballa",
      recurso_flag == "es_jurel"   ~ "Jurel",
      recurso_flag == "es_bacalao" ~ "Bacalao"
    )) %>%
    select(localidad, recurso)
}

# ============================================================
# 2. APLICAR A RECURSO 1, 2 Y 3
# ============================================================

rec1 <- datos %>%
  filter(!is.na(recurso_1), recurso_1 != "NA") %>%
  expandir_recursos_altura(recurso_1)

rec2 <- datos %>%
  filter(!is.na(recurso_2), recurso_2 != "NA") %>%
  expandir_recursos_altura(recurso_2)

rec3 <- datos %>%
  filter(!is.na(recurso_3), recurso_3 != "NA") %>%
  expandir_recursos_altura(recurso_3)

# Combinar y convertir a factor ordenado
recursos_altura <- bind_rows(rec1, rec2, rec3) %>%
  mutate(recurso = factor(recurso,
                          levels = c("Bonito", "Pota", "Perico",
                                     "Ova de pez volador", "Tiburón",
                                     "Caballa", "Jurel", "Bacalao")))

# Verificar conteos
recursos_altura %>% count(recurso)

# ============================================================
# 3A. ANÁLISIS A: % de pescadores por localidad que reportaron cada recurso
# ============================================================
# El porcentaje de pescadores en cada localidad que reportaron ese recurso 
# (ej. "el 60% de los pescadores de Pucusana reportaron pescar Bonito")

total_por_localidad <- datos %>% count(localidad, name = "total_pescadores")

tabla_A <- recursos_altura %>%
  group_by(localidad, recurso) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(total_por_localidad, by = "localidad") %>%
  mutate(pct = round((n / total_pescadores) * 100, 1)) %>%
  complete(localidad, recurso, fill = list(n = 0, pct = 0)) %>%
  group_by(localidad) %>%
  mutate(total_pescadores = max(total_pescadores, na.rm = TRUE)) %>%
  arrange(localidad, desc(pct))

print(tabla_A)

# See the whole table
tabla_A %>% print(n = Inf)

# ============================================================
# 3B. ANÁLISIS B: % de reportes de cada recurso que vienen de cada localidad
# ============================================================
# Del total de reportes de ese recurso, qué porcentaje viene de cada localidad 
# (ej. "el 40% de todos los reportes de Bonito vienen de Pucusana")

tabla_B <- recursos_altura %>%
  group_by(recurso, localidad) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(recurso) %>%
  mutate(
    total_recurso = sum(n),
    pct = round((n / total_recurso) * 100, 1)
  ) %>%
  arrange(recurso, desc(pct))

print(tabla_B)

# See the whole table
tabla_B %>% print(n = Inf)

# ============================================================
# 4. GRÁFICOS
# ============================================================
# Paleta de colores personalizada
colores <- c(
  "Pucusana"   = "#00bbd6",  # turquesa
  "Marcona"    = "#237194",  # azul oscuro
  "San Andrés" = "#faa32b",  # naranja/dorado
  "Lomas"      = "#8B1A3A"   # vino tinto
)

# Verificar cuántos pescadores reportaron Ova por localidad
recursos_altura %>%
  filter(recurso == "Ova de pez volador") %>%
  count(localidad) %>%
  left_join(total_por_localidad, by = "localidad") %>%
  mutate(pct = round((n / total_pescadores) * 100, 1))

# Gráfico A actualizado
ggplot(tabla_A, aes(x = recurso, y = pct, fill = localidad)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colores) +
  labs(
    title    = "Percentage of fishers reporting each offshore species by locality",
    subtitle = "Denominator = total fishers per locality",
    x        = "Target species",
    y        = "% of fishers",
    fill     = "Locality"
  ) +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray50", size = 9)
  )

# Paleta de gradiente de azules para 8 recursos
colores_recurso <- c(
  "Bonito"             = "#D8EEFA",  # un poco más claro que antes
  "Pota"               = "#9DCAE8",  
  "Perico"             = "#6AADD4",  
  "Ova de pez volador" = "#4A9CCE",  
  "Tiburón"            = "#2E85BE",  
  "Caballa"            = "#1A6A9A",  
  "Jurel"              = "#0D4F76",  
  "Bacalao"            = "#062D45"
)

# Gráfico B
ggplot(tabla_B, aes(x = localidad, y = pct, fill = recurso)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colores_recurso) +
  labs(
    title    = "Distribution of offshore species reports across localities",
    subtitle = "Denominator = total reports per species",
    x        = "Locality",
    y        = "% of reports",
    fill     = "Target species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray50", size = 9)
  )


# ============================================================
#              ANALISIS RECURSO ENTRE LOCALIDADES
# ============================================================
# La proporción de pescadores que reportan este recurso difiere entre localidades?

# Función para correr Fisher por recurso
analisis_por_recurso <- function(recurso_interes) {
  tabla <- datos %>%
    mutate(reporta = ifelse(
      str_detect(coalesce(recurso_1, ""), regex(recurso_interes, ignore_case = TRUE)) |
        str_detect(coalesce(recurso_2, ""), regex(recurso_interes, ignore_case = TRUE)) |
        str_detect(coalesce(recurso_3, ""), regex(recurso_interes, ignore_case = TRUE)),
      "si", "no"
    )) %>%
    count(localidad, reporta) %>%
    tidyr::pivot_wider(names_from = reporta, values_from = n, values_fill = 0) %>%
    tibble::column_to_rownames("localidad")
  
  cat("\n====", recurso_interes, "====\n")
  print(chisq.test(tabla)$expected)
  print(fisher.test(tabla, simulate.p.value = TRUE))
}

# Correr para recursos con suficientes datos
# p < 0.05 = Hay diferencia
# p > 0.05 = NO hay diferencia

analisis_por_recurso("bonito")
# p = 0.001499. SI diferencia

analisis_por_recurso("pota")
# p = 0.01349. SI diferencia

analisis_por_recurso("perico")
# p = 0.003498. SI diferencia

analisis_por_recurso("ova")
# p = 0.0004998. SI diferencia

analisis_por_recurso("tollo|espada|tiburón|tiburon")
# p = 0.04798. SI diferencia

#la probabilidad de que un pescador reporte ese recurso no es la misma en todos los puertos.