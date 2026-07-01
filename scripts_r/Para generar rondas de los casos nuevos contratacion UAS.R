library(dplyr)
library(stringr)

base_completos <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/87_ Plan de contratacion/Data/equipo itinerantes 23062026.xlsx"
)

ejemplo <- readxl::read_xlsx("C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/87_ Plan de contratacion/Data raw/Segunda validacion equipos qx itinerantes.xlsx",
                             sheet = "Ronda 5")

library(dplyr)
library(readxl)
library(stringr)
library(purrr)

# 1. Leer base -------------------------------------------------------------

hoja_ronda_6 <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/Downloads/casos nuevos.xlsx"
)

# 2. Definir puestos requeridos ------------------------------------------

puestos_requeridos <- c(
  "Medicina General",
  "Anestesiologia",
  "Cirugia",
  "Enfermeria quirurgica"
)

# 3. Crear / asegurar puesto_arm -----------------------------------------

hoja_ronda_6 <- hoja_ronda_6 %>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ puesto_arm
    )
  )

# 4. Resumen por cluster --------------------------------------------------

resumen_cluster <- hoja_ronda_6 %>% 
  group_by(estado_ancla, cluster_id) %>% 
  summarise(
    medicina_general = sum(puesto_arm == "Medicina General", na.rm = TRUE),
    anestesiologia = sum(puesto_arm == "Anestesiologia", na.rm = TRUE),
    cirugia = sum(puesto_arm == "Cirugia", na.rm = TRUE),
    enfermeria_quirurgica = sum(puesto_arm == "Enfermeria quirurgica", na.rm = TRUE),
    chofer = sum(puesto_arm == "Chofer", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      medicina_general,
      anestesiologia,
      cirugia,
      enfermeria_quirurgica
    ),
    
    siguiente_equipo = equipo_itinerante + 1,
    
    puestos_faltantes_siguiente_equipo = pmap_chr(
      list(
        medicina_general,
        anestesiologia,
        cirugia,
        enfermeria_quirurgica,
        siguiente_equipo
      ),
      function(mg, an, ci, en, sig) {
        
        faltan <- c(
          if (mg < sig) "Medicina General",
          if (an < sig) "Anestesiologia",
          if (ci < sig) "Cirugia",
          if (en < sig) "Enfermeria quirurgica"
        )
        
        if (length(faltan) == 0) {
          "Equipo completo"
        } else {
          str_c(faltan, collapse = ", ")
        }
      }
    ),
    
    n_faltantes_siguiente_equipo = if_else(
      puestos_faltantes_siguiente_equipo == "Equipo completo",
      0L,
      str_count(puestos_faltantes_siguiente_equipo, ",") + 1L
    )
  )

# 5. Pegar resumen a la base persona por persona --------------------------

hoja_ronda_6_final <- hoja_ronda_6 %>% 
  select(-any_of(c(
    "equipo_itinerante",
    "puestos_faltantes",
    "puestos_faltantes_siguiente_equipo",
    "n_faltantes_siguiente_equipo"
  ))) %>% 
  left_join(
    resumen_cluster %>% 
      select(
        estado_ancla,
        cluster_id,
        equipo_itinerante,
        siguiente_equipo,
        puestos_faltantes_siguiente_equipo,
        n_faltantes_siguiente_equipo
      ),
    by = c("estado_ancla", "cluster_id")
  )

# 6. Filtrar clusters relevantes -----------------------------------------
# Deja:
# - clusters que ya arman al menos 1 equipo
# - clusters que están a 1 persona de armar el siguiente equipo

base_filtrada <- hoja_ronda_6_final %>% 
  filter(
    equipo_itinerante >= 1 |
      n_faltantes_siguiente_equipo == 1
  ) %>% 
  transmute(
    estado_ancla = ancla_entidad,
    clues_ancla,
    nombre_del_ancla,
    nombre,
    curp,
    puesto,
    clave_del_puesto,
    `¿Es equipo itinerante y sigue en pie?` = NA,
    Completo = case_when(
      equipo_itinerante >= 1 ~ "SI",
      equipo_itinerante == 0 & n_faltantes_siguiente_equipo == 1 ~ paste0(
        "NO, falta ",
        puestos_faltantes_siguiente_equipo
      ),
      TRUE ~ NA_character_
    ),
    estatus_uas,
    cluster_id,
    enlace_a_carpeta,
    puesto_arm,
    equipo_itinerante
  )

writexl::write_xlsx(base_filtrada,
                    "C:/Users/Cecilia Pereo/Downloads/ronda 6.xlsx")


