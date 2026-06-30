library(googlesheets4)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(data.table)
library(writexl)
library(httr)
library(reticulate)
library(stringr)

# FUNCIONES ---------------------------------------------------------------
# CURPs limpias 
py_run_string("
import requests
import json
import pandas as pd

requests.packages.urllib3.disable_warnings()

def consultar_curp(curp):
    headers = {
        'user-agent': 'Mozilla/5.0',
        'content-type': 'application/json; charset=utf-8'
    }

    payload = {'curp': curp.strip()}

    try:
        r = requests.post(
            'https://us-central1-os-gobierno-de-nuevo-leon.cloudfunctions.net/nuevoLeon-checkCurp',
            data=json.dumps(payload),
            headers=headers,
            verify=False
        )

        if r.status_code == 200:
            out = r.json()
            out['curp'] = curp.strip()
            return out

        return {'curp': curp.strip(), 'error': 'No encontrada'}

    except Exception as e:
        return {'curp': curp.strip(), 'error': str(e)}


def consultar_curps(curps):
    resultados = [consultar_curp(x) for x in curps]
    return pd.DataFrame(resultados)
")
regex_curp <- "^[A-Z]{4}[0-9]{6}[HM][A-Z]{5}[A-Z0-9]{2}$"

probar_link <- function(url) {
  tryCatch({
    resp <- HEAD(url, timeout(10))
    
    tibble(
      url = url,
      status = status_code(resp),
      funciona = status_code(resp) %in% c(200, 302)
    )
    
  }, error = function(e) {
    tibble(
      url = url,
      status = NA_integer_,
      funciona = FALSE
    )
  })
}
# Catálogo -----------------------------------------------------
catalogo_puestos <- read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Plantilla/catalogos/Catalogo_CNPM_2026_F.xlsx"
) %>% 
  clean_names() %>% 
  select(cnpm = codigo_cnpm_26,
         denominacion_de_puesto)

hbc <- read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/bases_clusters_viejas/cluster_19_carlos_long_simple.xlsx"
)

vector_ancla_cluster <- c(hbc$clues_imb, substr(hbc$nombre_cluster,1,11)) |> unique()
vector_ancla_cluster <- vector_ancla_cluster[!is.na(vector_ancla_cluster)]

team_completos <- base_eq_completos <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/87_ Plan de contratacion/Data/equipo itinerantes 23062026.xlsx"
) %>% 
  clean_names() %>% 
  mutate(
    curp_limpia = curp %>% 
      as.character() %>% 
      str_replace_all("['\"`´“”‘’]", "") %>% 
      str_replace_all("\\s+", "") %>% 
      str_trim() %>% 
      str_to_upper()
  ) %>% 
  distinct(curp_limpia, .keep_all = TRUE) %>% 
  transmute(
    curp_limpia,
    clues = clues_ancla
  )

base_curps_limpios <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/Downloads/equipo itinerantes completos_24_06_2026.xlsx",
  sheet = "base"
) %>%
  select(curp, nombre) %>%
  filter(!is.na(nombre))

base_alex_original <- st_read(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/80_Basico comunitarios dificil acceso/bases/cluster_19_rutas_geo.gpkg"
) 
# Google Sheets -----------------------------------------------------------
gs4_deauth()
gs4_auth(
  email = "lia.pereo@ciencias.unam.mx",
  scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")

url <- "https://docs.google.com/spreadsheets/d/1Axj6z0U1odyFcdDz7Rjdv2jOZgnh9yIxKAXiWzvXKW4/edit?gid=0#gid=0"

df <- read_sheet(ss = url,
                 sheet = "Registros_completos") %>% 
  clean_names() %>% 
  mutate(across(where(is.character),
                ~ .x %>% 
                  str_trim() %>% 
                  str_to_upper()))
# Base madre --------------------------------------------------------------
base_online <- df %>% 
  mutate(
    curp_limpia = curp %>% 
      as.character() %>% 
      str_replace_all("['\"`´“”‘’]", "") %>% 
      str_replace_all("\\s+", "") %>% 
      str_trim() %>% 
      str_to_upper()
  ) %>% 
  anti_join(team_completos, by = "curp_limpia") %>% 
  filter(
    turno %like% "(itinerante)|(ITINERANTE)" |
      (fase == 3 & clues %in% vector_ancla_cluster) 
  ) %>% 
  mutate(cnpm = case_when(
    clave_puesto == "ME002 CIRUGIA GENERAL" ~ "ME002",
    clave_puesto == "OP057 CHOFER PROMOTOR POLIVALENTE" ~ "PA020",
    clave_puesto == "MG001 MEDICINA GENERAL" ~ "MG001",
    clave_puesto == "OP065 AUXILIAR ADMINISTRATIVO (CHOFER)" ~ "PA022",
    clave_puesto == "EN005 ENFERMERA ESPECIALISTA CIRUGIA" ~ "EN005",
    clave_puesto == "CHOFER PROMOTOR POLIVALENTE" ~ "PA020",
    clave_puesto == "CHOFER POLIVALENTE" ~ "PA022",
    clave_puesto == "AUXILIAR ADMINISTRATIVO (CHOFER)" ~ "PA022",
    clave_puesto == "CIRUGIA GENERAL" ~ "ME002",
    clave_puesto == "ANESTESIOLOGIA" ~ "ME001",
    clave_puesto == "ENFERMERA ESPECIALISTA CIRUGIA" ~ "EN005",
    clave_puesto == "MEDICINA GENERAL" ~ "MG001",
    cnpm == "OP057" ~ "PA020",
    cnpm == "OP065" ~ "PA022",
    TRUE ~ cnpm),
    cnpm = if_else(is.na(cnpm), clave_puesto, cnpm)
  ) %>% 
  left_join(catalogo_puestos, by = "cnpm") %>% 
  mutate(
    puesto_final = denominacion_de_puesto
  ) %>% 
  select(estado, clues_ancla = clues, fase, turno, link_carpeta,
         curp, puesto = puesto_final, cnpm, estatus_uas = revision_uas) %>% 
  filter(cnpm %in% c("PA020", "PA022", "ME001", "ME002",
                     "MG001", "EN005", "EN002"),
         is.na(estatus_uas)|estatus_uas == "APROBADO") %>% 
  mutate(prioridad_estatus = case_when(estatus_uas == "APROBADO" ~ 1,
                                       is.na(estatus_uas) | str_trim(
                                         estatus_uas) == "" ~ 2,
                                       TRUE ~ 3)) %>% 
  group_by(curp) %>% 
  arrange(prioridad_estatus, .by_group = TRUE) %>% 
  slice(1) %>%        # se queda con el de mayor prioridad
  ungroup() %>% 
  select(-prioridad_estatus) %>% 
  mutate(clues_ancla = case_when(
    clues_ancla == "PLIMB003706" ~ "PLIMB002516",
    clues_ancla == "SLIMB001950" ~ "SLIMB002930",
    clues_ancla == "SLIMB000195" ~ "SLIMB002930",
    clues_ancla == "SLIMB001554" ~ "SLIMB002930",
    clues_ancla == "BSIMB000503" ~ "BSIMB000754",
    clues_ancla == "TSIMB003483" ~ "TSIMB001260",
    TRUE ~ clues_ancla))

base_alex <-base_alex_original%>% 
  clean_names() %>% 
  st_drop_geometry() %>% 
  data.table() %>% 
  mutate(clues_ancla = str_remove(nombre_cluster, "_\\d+$")) %>% 
  distinct(clues_ancla, nombre_cluster, ancla_entidad,
           ancla_nombre) %>% 
  filter(!is.na(clues_ancla))

clues_no_pertenecen <- base_online %>% 
  clean_names() %>% 
  distinct(clues_ancla) %>% 
  anti_join(base_alex,
            by = c("clues_ancla" = "clues_ancla"))

base_online_1 <- base_online %>% 
  left_join(base_alex %>% distinct(clues_ancla, .keep_all = TRUE),
            by = "clues_ancla") %>%
  mutate(ancla_entidad = if_else(
    is.na(ancla_entidad), estado, ancla_entidad),
    nombre_cluster = if_else(
      is.na(nombre_cluster),
      "Sin match en cluster",
      nombre_cluster)) %>% 
  group_by(ancla_entidad, clues_ancla, nombre_del_ancla = ancla_nombre, 
           curp, puesto, cnpm, estatus_uas, nombre_cluster) %>% 
  mutate(n_duplicado = n()) 

sin_match_cluster <- base_online_1 %>% 
  filter(nombre_cluster == "Sin match en cluster")

base_curps_limpios_join <- base_curps_limpios %>%  #Cuando tenga archivo previo
  mutate(
    curp_limpia = curp %>% 
      as.character() %>% 
      str_replace_all("['\"`´“”‘’]", "") %>% 
      str_replace_all("\\s+", "") %>% 
      str_replace_all("[[:cntrl:]]", "") %>% 
      str_trim() %>% 
      str_to_upper()
  ) %>% 
  filter(!is.na(curp_limpia), curp_limpia != "") %>% 
  distinct(curp_limpia, .keep_all = TRUE) %>% 
  transmute(
    curp_limpia,
    nombre_base = nombre
  )

base_online_1 <- base_online_1 %>% 
  mutate(
    curp_original = curp,
    curp_limpia = curp %>% 
      as.character() %>% 
      str_replace_all("['\"`´“”‘’]", "") %>% 
      str_replace_all("\\s+", "") %>% 
      str_replace_all("[[:cntrl:]]", "") %>% 
      str_trim() %>% 
      str_to_upper(),
    cambio_limpieza = curp_original != curp_limpia,
    curp_vacia = is.na(curp_limpia) | curp_limpia == "",
    longitud_curp = nchar(curp_limpia),
    formato_curp_valido = str_detect(curp_limpia, regex_curp),
    estatus_validacion_curp = case_when(
      curp_vacia ~ "CURP vacía",
      longitud_curp != 18 ~ "Longitud distinta de 18",
      !formato_curp_valido ~ "Formato inválido",
      cambio_limpieza ~ "CURP corregida por limpieza",
      TRUE ~ "CURP válida sin cambios"
    )
  ) %>% 
  left_join(base_curps_limpios_join, by = "curp_limpia") %>% 
  mutate(
    nombre = nombre_base,
    nombre_recuperado_base_previa = !is.na(nombre_base) & nombre_base != ""
  ) %>% 
  select(
    curp_original, curp_limpia, nombre, nombre_recuperado_base_previa,
    cambio_limpieza, curp_vacia, longitud_curp,
    formato_curp_valido, estatus_validacion_curp,
    everything(),
    -nombre_base
  )

vector_curps <- base_online_1 %>% 
  filter(is.na(nombre) | nombre == "" | nombre == "NA NA NA") %>% 
  filter(formato_curp_valido) %>% 
  distinct(curp_limpia) %>% 
  pull(curp_limpia)

resultado_curps_py_it <- py$consultar_curps(vector_curps)

base_limpia_endpoint <- reticulate::py_to_r(resultado_curps_py_it)

if (!"apePat" %in% names(base_limpia_endpoint)) base_limpia_endpoint$apePat <- NA_character_
if (!"apeMat" %in% names(base_limpia_endpoint)) base_limpia_endpoint$apeMat <- NA_character_
if (!"nombres" %in% names(base_limpia_endpoint)) base_limpia_endpoint$nombres <- NA_character_
if (!"error" %in% names(base_limpia_endpoint)) base_limpia_endpoint$error <- NA_character_

base_limpia_endpoint <- base_limpia_endpoint %>% 
  transmute(
    curp_limpia = as.character(curp),
    apePat = as.character(apePat),
    apeMat = as.character(apeMat),
    nombres = as.character(nombres),
    nombre_endpoint = str_squish(paste(nombres, apePat, apeMat)),
    nombre_endpoint = na_if(nombre_endpoint, "NA NA NA"),
    error_endpoint = as.character(error)
  )

base_limpia <- base_online_1 %>% 
  left_join(base_limpia_endpoint, by = "curp_limpia") %>% 
  mutate(
    nombre = coalesce(nombre, nombre_endpoint),
    consulta_endpoint_exitosa = !is.na(nombres) | !is.na(apePat) | !is.na(apeMat),
    estatus_consulta_curp = case_when(
      !formato_curp_valido ~ estatus_validacion_curp,
      nombre_recuperado_base_previa ~ "Nombre recuperado de base previa",
      consulta_endpoint_exitosa ~ "CURP encontrada en endpoint",
      TRUE ~ "CURP válida en formato, no encontrada en endpoint"
    )
  ) %>% 
  select(-nombre_endpoint)
# Validacion de curps --
tabla_validaciones <- base_limpia %>% 
  count(estatus_consulta_curp, sort = TRUE)

curps_invalidas <- base_limpia %>% 
  filter(!formato_curp_valido)

curps_corregidas <- base_limpia %>% 
  filter(cambio_limpieza)

# CURPs válidas que no regresaron nombre/apellidos en el endpoint
sin_datos_curp <- base_limpia %>% 
  filter(
    formato_curp_valido,
    is.na(nombres),
    is.na(apePat),
    is.na(apeMat),
    is.na(nombre)
  ) %>% 
  mutate(
    motivo_eliminacion = "CURP válida pero sin nombre/apellidos en endpoint"
  )

# Quitar de la base principal las que no regresaron datos
base_limpia <- base_limpia %>% 
  filter(
    !(
      formato_curp_valido &
        is.na(nombres) &
        is.na(apePat) &
        is.na(apeMat) &
        is.na(nombre))) %>% 
  transmute(
    estado_ancla = ancla_entidad,
    clues_ancla,
    nombre_del_ancla,
    nombre,
    curp,
    puesto,
    clave_del_puesto = cnpm,
    estatus_uas,
    cluster_id = nombre_cluster,
    enlace_a_carpeta = link_carpeta)

write_xlsx(
  list(
    base_con_nombres = base_limpia,
    resumen_validaciones = tabla_validaciones,
    curps_invalidas = curps_invalidas,
    curps_corregidas = curps_corregidas,
    sin_datos_curp = sin_datos_curp,
    sin_match_cluster = sin_match_cluster
  ),
  "C:/Users/Cecilia Pereo/Downloads/base_eq_itinerantes.xlsx"
)

# Base team qx sin datos carlos -------------------------------------------
base_final <- base_limpia%>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(puesto_arm))

resumen_equipos <- base_final %>% 
  group_by(estado_ancla, cluster_id) %>% 
  summarise(
    anestesiologia = sum(puesto_arm == "Anestesiologia", na.rm = TRUE),
    cirugia = sum(puesto_arm == "Cirugia", na.rm = TRUE),
    medicina_general = sum(puesto_arm == "Medicina General", na.rm = TRUE),
    enfermeria_quirurgica = sum(puesto_arm == "Enfermeria quirurgica", na.rm = TRUE),
    chofer = sum(puesto_arm == "Chofer", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      anestesiologia,
      cirugia,
      medicina_general,
      enfermeria_quirurgica
    )
  ) %>% 
  select(estado_ancla, cluster_id, equipo_itinerante)

base_final <- base_final %>% 
  left_join(
    resumen_equipos,
    by = c("estado_ancla", "cluster_id")
  ) 


base_final %>% 
  distinct(estado_ancla, cluster_id, equipo_itinerante) %>% 
  summarise(total_equipos = sum(equipo_itinerante, na.rm = TRUE))
base_completos <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/87_ Plan de contratacion/Data/equipo itinerantes 23062026.xlsx"
)

val <- inner_join(base_completos, base_final) 

base_final <- anti_join(base_final, base_completos) %>% 
  arrange(ancla_entidad, cnpm,clues_ancla, nombre_del_ancla, curp)

writexl::write_xlsx(base_final,
                    "C:/Users/Cecilia Pereo/Downloads/casos nuevos.xlsx")

val2 <- inner_join(base_final, base_completos)


# Base team qx, con datos Carlos ------------------------------------------
base_completos <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/87_ Plan de contratacion/Data/equipo itinerantes 23062026.xlsx"
) %>% 
  transmute(ancla_entidad = estado_ancla, cnpm = clave_del_puesto,
            nombre_cluster = cluster_id, estado_ancla, clues_ancla,
            nombre_del_ancla, nombre, curp, puesto, clave_del_puesto,
            estatus_uas, cluster_id, enlace_a_carpeta, casos_nuevos = 0)

base_limpia <- base_limpia %>% 
  mutate(casos_nuevos = 1)

val <- inner_join(base_completos,base_limpia)

base_completa_final <- rbind(base_completos, base_limpia) 
val <- inner_join(base_completos,base_completa_final)

base_final_1 <- base_completa_final %>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(puesto_arm))

resumen_equipos <- base_final_1 %>% 
  group_by(estado_ancla, cluster_id) %>% 
  summarise(
    anestesiologia = sum(puesto_arm == "Anestesiologia", na.rm = TRUE),
    cirugia = sum(puesto_arm == "Cirugia", na.rm = TRUE),
    medicina_general = sum(puesto_arm == "Medicina General", na.rm = TRUE),
    enfermeria_quirurgica = sum(puesto_arm == "Enfermeria quirurgica", na.rm = TRUE),
    chofer = sum(puesto_arm == "Chofer", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      anestesiologia,
      cirugia,
      medicina_general,
      enfermeria_quirurgica
    )
  ) %>% 
  select(estado_ancla, cluster_id, equipo_itinerante)

base_final_1 <- base_final_1 %>% 
  left_join(
    resumen_equipos,
    by = c("estado_ancla", "cluster_id")
  ) %>% 
  select(ancla_entidad, nombre_cluster, clues_ancla, nombre_del_ancla,
         cnpm, puesto, curp, nombre, estatus_uas, enlace_a_carpeta,
         puesto_arm, equipo_itinerante, casos_nuevos) %>% 
  arrange(ancla_entidad, cnpm,clues_ancla, nombre_del_ancla, curp)

base_final_val <- inner_join(base_final_1, base_completos) 

writexl::write_xlsx(base_final_1,
                    "C:/Users/Cecilia Pereo/Downloads/casos completos equipos itinerantes.xlsx")

val2 <- inner_join(base_final_1, base_completos)



# Base nueva version ------------------------------------------------------
base_completos <- readxl::read_xlsx(
  "C:/Users/Cecilia Pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/87_ Plan de contratacion/Data/equipo itinerantes 23062026.xlsx"
) %>% 
  transmute(
    ancla_entidad = estado_ancla,
    cnpm = clave_del_puesto,
    nombre_cluster = cluster_id,
    estado_ancla,
    clues_ancla,
    nombre_del_ancla,
    nombre,
    curp,
    puesto,
    clave_del_puesto,
    estatus_uas,
    cluster_id,
    enlace_a_carpeta,
    casos_nuevos = 0
  )

base_limpia <- base_limpia %>% 
  mutate(casos_nuevos = 1)

base_completa_final <- bind_rows(base_completos, base_limpia)

base_final_1 <- base_completa_final %>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(puesto_arm))

resumen_equipos <- base_final_1 %>% 
  group_by(estado_ancla, cluster_id) %>% 
  summarise(
    anestesiologia = sum(puesto_arm == "Anestesiologia", na.rm = TRUE),
    cirugia = sum(puesto_arm == "Cirugia", na.rm = TRUE),
    medicina_general = sum(puesto_arm == "Medicina General", na.rm = TRUE),
    enfermeria_quirurgica = sum(puesto_arm == "Enfermeria quirurgica", na.rm = TRUE),
    chofer = sum(puesto_arm == "Chofer", na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      anestesiologia,
      cirugia,
      medicina_general,
      enfermeria_quirurgica
    ),
    
    puestos_faltantes = pmap_chr(
      list(
        medicina_general,
        anestesiologia,
        cirugia,
        enfermeria_quirurgica,
        equipo_itinerante
      ),
      function(mg, an, ci, en, equipos) {
        
        objetivo <- equipos + 1
        
        faltan <- c(
          if (mg < objetivo) "Medicina General",
          if (an < objetivo) "Anestesiologia",
          if (ci < objetivo) "Cirugia",
          if (en < objetivo) "Enfermeria quirurgica"
        )
        
        if (length(faltan) == 0) {
          "Equipo completo"
        } else {
          paste(faltan, collapse = ", ")
        }
      }
    ),
    
    estado_equipo = case_when(
      equipo_itinerante >= 1 & puestos_faltantes == "Equipo completo" ~ "Equipo completo",
      equipo_itinerante >= 1 ~ paste0(
        equipo_itinerante,
        " equipo(s) completo(s); falta ",
        puestos_faltantes,
        " para otro equipo"
      ),
      equipo_itinerante == 0 ~ paste0(
        "No completo; falta ",
        puestos_faltantes
      )
    )
  ) %>% 
  select(
    estado_ancla,
    cluster_id,
    equipo_itinerante,
    puestos_faltantes,
    estado_equipo
  )

base_final_1 <- base_final_1 %>% 
  select(-any_of(c("equipo_itinerante", "puestos_faltantes", "estado_equipo"))) %>% 
  left_join(
    resumen_equipos,
    by = c("estado_ancla", "cluster_id")
  ) %>% 
  select(
    ancla_entidad,
    nombre_cluster,
    estado_ancla,
    cluster_id,
    clues_ancla,
    nombre_del_ancla,
    cnpm,
    puesto,
    curp,
    nombre,
    estatus_uas,
    enlace_a_carpeta,
    puesto_arm,
    equipo_itinerante,
    puestos_faltantes,
    estado_equipo,
    casos_nuevos
  ) %>% 
  arrange(ancla_entidad, cnpm,clues_ancla, nombre_del_ancla, curp)

writexl::write_xlsx(
  base_final_1,
  "C:/Users/Cecilia Pereo/Downloads/casos completos equipos itinerantes con faltantes.xlsx"
)

# Resumen de base limpia -----------------------------------------
base_limpia_final <- base_limpia %>% 
  mutate(
    puesto_arm = case_when(
      clave_del_puesto == "MG001" ~ "Medicina General",
      clave_del_puesto == "ME001" ~ "Anestesiologia",
      clave_del_puesto == "ME002" ~ "Cirugia",
      clave_del_puesto %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      clave_del_puesto %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(!is.na(puesto_arm)) %>% 
  group_by(estado_ancla, cluster_id, puesto_arm) %>% 
  summarise(
    personas = n_distinct(curp),
    .groups = "drop"
  ) %>% 
  pivot_wider(
    names_from = puesto_arm,
    values_from = personas,
    values_fill = 0
  ) %>% 
  mutate(
    equipo_itinerante = pmin(
      Anestesiologia,
      Cirugia,
      `Medicina General`,
      `Enfermeria quirurgica`,
      na.rm = TRUE
    ),
    anestesiologia_sobrante = Anestesiologia - equipo_itinerante,
    cirugia_sobrante = Cirugia - equipo_itinerante,
    medicina_general_sobrante = `Medicina General` - equipo_itinerante,
    enfermeria_quirurgica_sobrante = `Enfermeria quirurgica` - equipo_itinerante,
    chofer_sobrante = Chofer - equipo_itinerante,
    
    equipo_itinerante_incompleto = if_else(
      anestesiologia_sobrante +
        cirugia_sobrante +
        medicina_general_sobrante +
        enfermeria_quirurgica_sobrante +
        chofer_sobrante > 0,
      1L,
      0L
    ),
    
    puestos_faltantes = pmap_chr(
      list(
        anestesiologia_sobrante,
        cirugia_sobrante,
        medicina_general_sobrante,
        enfermeria_quirurgica_sobrante,
        chofer_sobrante
      ),
      function(anest, cir, med, enf, chof) {
        
        faltan <- c(
          if (anest < 1) "Anestesiologia",
          if (cir < 1) "Cirugia",
          if (med < 1) "Medicina General",
          if (enf < 1) "Enfermeria quirurgica",
          if (chof < 1) "Chofer"
        )
        
        if (length(faltan) == 5) return("")
        if (length(faltan) == 0) return("")
        
        paste(faltan, collapse = ", ")
      }
    )
  ) %>% 
  select(-ends_with("_sobrante")) %>% 
  mutate(estado_ancla = str_to_title(estado_ancla))


resumen_team_qx <- base_limpia_final %>% 
  select(
    estado_ancla,
    cluster_id,
    Anestesiologia,
    Cirugia,
    `Medicina General`,
    `Enfermeria quirurgica`,
    Chofer,
    equipo_itinerante,
    equipo_itinerante_incompleto,
    puestos_faltantes
  ) %>% 
  mutate(
    estado_ancla = str_to_title(estado_ancla)
  )


base_final <- base_limpia %>% 
  select(-any_of(c(
    "Anestesiologia",
    "Cirugia",
    "Medicina General",
    "Enfermeria quirurgica",
    "Chofer",
    "equipo_itinerante",
    "equipo_itinerante_incompleto",
    "puestos_faltantes"
  ))) %>% 
  left_join(
    resumen_team_qx %>% 
      select(-estado_ancla),
    by = "cluster_id"
  ) %>% 
  mutate(
    across(
      c(
        Anestesiologia,
        Cirugia,
        `Medicina General`,
        `Enfermeria quirurgica`,
        Chofer,
        equipo_itinerante,
        equipo_itinerante_incompleto
      ),
      ~ replace_na(.x, 0)
    ),
    puestos_faltantes = replace_na(puestos_faltantes, "")
  ) %>% 
  distinct_all()

revision_links <- base_final %>% 
  group_by(clues_ancla, cnpm, curp) %>% 
  mutate(duplicados = n()) %>% 
  filter(duplicados > 0) %>% 
  distinct(duplicados, enlace_a_carpeta) %>% 
  mutate(
    revision = map(enlace_a_carpeta, probar_link)
  ) %>% 
  unnest(revision)

links_buenos <- revision_links %>% 
  filter(funciona) %>% 
  group_by(duplicados) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(duplicados, enlace_a_carpeta)

base_corregida <- base_final%>% 
  group_by(clues_ancla, cnpm, curp) %>% 
  mutate(duplicados = n()) %>% 
  left_join(
    links_buenos %>% 
      mutate(link_bueno = TRUE),
    by = c("duplicados", "enlace_a_carpeta")
  ) %>% 
  filter(
    duplicados == 0 | link_bueno == TRUE,
    nombre != "NA NA NA",
    cluster_id != "Sin match en cluster"
  )

duplicados_link_feo <- base_final %>% 
  group_by(clues_ancla, cnpm, curp) %>% 
  mutate(duplicados = n()) %>% 
  left_join(
    links_buenos %>% 
      mutate(link_bueno = TRUE),
    by = c("duplicados", "enlace_a_carpeta")
  ) %>% 
  filter(
    duplicados > 0,
    is.na(link_bueno)
  ) %>% 
  mutate(
    motivo_eliminacion = "Duplicado eliminado porque el link no abre"
  ) %>% 
  select(-link_bueno)

observaciones_eliminadas_filtros <- base_online %>% 
  anti_join(
    base_corregida %>% 
      distinct(curp, clues_ancla, clave_del_puesto),
    by = c(
      "curp",
      "clues_ancla",
      "cnpm" = "clave_del_puesto"
    )
  ) %>% 
  mutate(
    motivo_eliminacion = case_when(
      !clues_ancla %in% base_alex$clues_imb ~ "CLUES no pertenece a base_alex / no es ancla",
      TRUE ~ "Se eliminó en filtros posteriores"
    )
  )

observaciones_eliminadas <- bind_rows(
  observaciones_eliminadas_filtros,
  duplicados_link_feo)

base_corregida <- base_corregida %>% 
  select(-link_bueno, -duplicados,
         -puestos_faltantes, -equipo_itinerante_incompleto) %>% 
  mutate(estado_ancla = str_to_title(estado_ancla))

# write_xlsx(
#   list(
#     base_limpia = base_corregida,
#     observaciones_eliminadas = observaciones_eliminadas
#   ),
#   "C:/Users/Cecilia Pereo/Downloads/equipo itinerantes completos.xlsx"
# )

