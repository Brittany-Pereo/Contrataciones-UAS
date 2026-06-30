library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# BASE COMPLETA POR CLUSTER -----------------------------------------------
# 1) Catálogo de puestos requeridos 
puestos_requeridos <- tibble::tribble(
  ~puesto_arm,                 ~codigo_cnpm, ~nombre_del_puesto,
  "Cirugia",                   "ME002",      "Cirujano",
  "Anestesiologia",            "ME001",      "Anestesiólogo",
  "Enfermeria quirurgica",     "EN005",      "Enfermera quirúrgica",
  "Chofer",                    "PA020",      "Chofer",
  "Medicina General",          "MG001",      "Médico general"
)

orden_puestos <- c(
  "Cirugia",
  "Anestesiologia",
  "Enfermeria quirurgica",
  "Chofer",
  "Medicina General"
)

# 2) Base de 75 clusters x 5 puestos = 375 renglones 

clusters_alex <- base_alex %>%
  transmute(
    cluster_id = str_trim(nombre_cluster),
    clues_ancla,
    nombre_del_ancla = ancla_nombre
  ) %>%
  filter(!is.na(cluster_id), cluster_id != "") %>%
  distinct(cluster_id, .keep_all = TRUE) %>%
  mutate(
    orden_cluster = as.integer(str_extract(cluster_id, "\\d+$"))
  ) %>%
  arrange(clues_ancla, orden_cluster, cluster_id)

validacion_clusters_alex <- clusters_alex %>%
  summarise(total_clusters = n())

base_necesidad_375 <- clusters_alex %>%
  tidyr::crossing(puestos_requeridos) %>%
  arrange(
    clues_ancla,
    orden_cluster,
    cluster_id,
    match(puesto_arm, orden_puestos)
  )

validacion_base_necesidad <- base_necesidad_375 %>%
  summarise(total_renglones = n())

# 3) Candidatos aprobados UAS 

candidatos_uas <- base_final_1 %>%
  mutate(
    curp = str_trim(str_to_upper(curp)),
    cluster_id = str_trim(cluster_id),
    cnpm = str_trim(str_to_upper(cnpm)),
    estatus_uas = str_trim(str_to_upper(estatus_uas)),
    puesto_arm = case_when(
      cnpm == "MG001" ~ "Medicina General",
      cnpm == "ME001" ~ "Anestesiologia",
      cnpm == "ME002" ~ "Cirugia",
      cnpm %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      cnpm %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_
    ),
    fuente_candidato = "Base validada UAS"
  ) %>%
  filter(
    !is.na(puesto_arm),
    !is.na(cluster_id),
    cluster_id != "",
    cluster_id != "Sin match en cluster",
    is.na(estatus_uas) | estatus_uas == "" | estatus_uas == "APROBADO"
  ) %>%
  mutate(
    prioridad_estatus = case_when(
      estatus_uas == "APROBADO" ~ 1,
      is.na(estatus_uas) | estatus_uas == "" ~ 2,
      TRUE ~ 3
    ),
    prioridad_cnpm = case_when(
      puesto_arm == "Enfermeria quirurgica" & cnpm == "EN005" ~ 1,
      puesto_arm == "Enfermeria quirurgica" & cnpm == "EN002" ~ 2,
      puesto_arm == "Chofer" & cnpm == "PA020" ~ 1,
      puesto_arm == "Chofer" & cnpm == "PA022" ~ 2,
      TRUE ~ 1
    )
  ) %>%
  arrange(
    cluster_id,
    puesto_arm,
    prioridad_estatus,
    prioridad_cnpm,
    nombre
  ) %>%
  group_by(cluster_id, puesto_arm) %>%
  mutate(slot = row_number()) %>%
  ungroup() %>%
  select(
    cluster_id,
    puesto_arm,
    slot,
    nombre,
    curp,
    enlace_a_carpeta,
    estatus_uas,
    fuente_candidato,
    codigo_cnpm_uas = cnpm,
    nombre_del_puesto_uas = puesto
  )

# 4) Asignar candidatos UAS a la base objetivo 

base_necesidad_375 <- base_necesidad_375 %>%
  group_by(cluster_id, puesto_arm) %>%
  mutate(slot = row_number()) %>%
  ungroup()

base_cluster_375_con_uas <- base_necesidad_375 %>%
  left_join(
    candidatos_uas,
    by = c("cluster_id", "puesto_arm", "slot")
  ) %>%
  mutate(
    ocupado_con_uas = !is.na(curp),
    estatus_ocupacion = case_when(
      ocupado_con_uas ~ "Cubierto con candidato UAS",
      TRUE ~ "Vacante / sin candidato UAS"
    ),
    codigo_cnpm = coalesce(codigo_cnpm_uas, codigo_cnpm),
    nombre_del_puesto = coalesce(nombre_del_puesto_uas, nombre_del_puesto)
  ) %>%
  select(
    cluster_id,
    clues_ancla,
    nombre_del_ancla,
    codigo_cnpm,
    nombre_del_puesto,
    puesto_arm,
    nombre,
    curp,
    enlace_a_carpeta,
    estatus_uas,
    fuente_candidato,
    ocupado_con_uas,
    estatus_ocupacion
  )

# 5) Validaciones rápidas 

validacion_general <- base_cluster_375_con_uas %>%
  summarise(
    total_renglones = n(),
    total_clusters = n_distinct(cluster_id),
    cubiertos = sum(ocupado_con_uas, na.rm = TRUE),
    vacantes = sum(!ocupado_con_uas, na.rm = TRUE)
  )

validacion_por_puesto <- base_cluster_375_con_uas %>%
  count(puesto_arm, estatus_ocupacion)

validacion_renglones_por_cluster <- base_cluster_375_con_uas %>%
  count(cluster_id) %>%
  count(n, name = "clusters_con_ese_numero_de_renglones")

resumen_por_cluster <- base_cluster_375_con_uas %>%
  group_by(cluster_id, clues_ancla, nombre_del_ancla) %>%
  summarise(
    puestos_requeridos = n(),
    puestos_cubiertos = sum(ocupado_con_uas, na.rm = TRUE),
    puestos_vacantes = sum(!ocupado_con_uas, na.rm = TRUE),
    puestos_faltantes = paste(
      nombre_del_puesto[!ocupado_con_uas],
      collapse = ", "
    ),
    .groups = "drop"
  )

# 6) Validación contra base completa UAS 

universo_uas <- candidatos_uas %>%
  distinct(curp, .keep_all = TRUE)

asignados_final <- base_cluster_375_con_uas %>%
  filter(!is.na(curp)) %>%
  distinct(curp)

no_asignados_uas <- universo_uas %>%
  anti_join(asignados_final, by = "curp") %>%
  mutate(
    resultado_validacion = "No asignado en base 375",
    motivo_probable = "Sin espacio disponible en cluster/puesto"
  )

clusters_incompletos <- base_cluster_375_con_uas %>%
  group_by(cluster_id, clues_ancla, nombre_del_ancla) %>%
  summarise(
    puestos_cubiertos = sum(!is.na(curp)),
    puestos_vacantes = sum(is.na(curp)),
    faltantes = paste(
      nombre_del_puesto[is.na(curp)],
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  filter(puestos_vacantes > 0)

asignados_final_detalle <- base_cluster_375_con_uas %>%
  filter(!is.na(curp)) %>%
  distinct(curp, .keep_all = TRUE) %>%
  select(
    curp,
    cluster_id_asignado = cluster_id,
    puesto_arm_asignado = puesto_arm
  )

auditoria_uas <- candidatos_uas %>%
  left_join(asignados_final_detalle, by = "curp") %>%
  mutate(
    resultado = case_when(
      !is.na(cluster_id_asignado) ~ "Asignado en base 375",
      TRUE ~ "No asignado en base 375"
    ),
    motivo_probable = case_when(
      resultado == "Asignado en base 375" ~ "Asignado correctamente",
      TRUE ~ "Sin espacio disponible en cluster/puesto"
    )
  )

clusters_alex_no_en_375 <- clusters_alex %>%
  anti_join(
    base_cluster_375_con_uas %>%
      distinct(cluster_id),
    by = "cluster_id"
  )

clusters_uas_no_en_alex <- candidatos_uas %>%
  filter(
    !is.na(cluster_id),
    cluster_id != "",
    cluster_id != "Sin match en cluster"
  ) %>%
  distinct(cluster_id) %>%
  anti_join(
    clusters_alex %>%
      distinct(cluster_id),
    by = "cluster_id"
  )

# 7) Resúmenes de auditoría 

resumen_no_asignados_uas <- no_asignados_uas %>%
  count(puesto_arm, name = "no_asignados")

resumen_auditoria_uas <- auditoria_uas %>%
  count(resultado, puesto_arm)

# 8) Exportar 

writexl::write_xlsx(
  list(
    base_cluster_375_con_uas = base_cluster_375_con_uas,
    validacion_por_puesto = validacion_por_puesto,
    validacion_renglones_cluster = validacion_renglones_por_cluster,
    resumen_por_cluster = resumen_por_cluster,
    clusters_incompletos = clusters_incompletos,
    no_asignados_uas = no_asignados_uas,
    resumen_no_asignados_uas = resumen_no_asignados_uas,
    auditoria_uas = auditoria_uas
  ),
  "C:/Users/Cecilia Pereo/Downloads/base_375_clusters_uas_validada.xlsx"
)
# BASE COMPLETA POR ESTADO (distribución estatal) -------------------------
# 1) Catálogo de puestos requeridos 

puestos_requeridos <- tibble::tribble(
  ~puesto_arm,                 ~codigo_cnpm, ~nombre_del_puesto,
  "Cirugia",                   "ME002",      "Cirujano",
  "Anestesiologia",            "ME001",      "Anestesiólogo",
  "Enfermeria quirurgica",     "EN005",      "Enfermera quirúrgica",
  "Chofer",                    "PA020",      "Chofer",
  "Medicina General",          "MG001",      "Médico general"
)

orden_puestos <- c(
  "Cirugia",
  "Anestesiologia",
  "Enfermeria quirurgica",
  "Chofer",
  "Medicina General"
)

# 2) Base Alex: mantener clusters pero agregar estado 

clusters_alex <- base_alex %>%
  transmute(
    estado_ancla = str_trim(str_to_upper(ancla_entidad)),
    cluster_id = str_trim(nombre_cluster),
    clues_ancla,
    nombre_del_ancla = ancla_nombre
  ) %>%
  filter(!is.na(cluster_id), cluster_id != "") %>%
  distinct(cluster_id, .keep_all = TRUE) %>%
  mutate(
    orden_cluster = as.integer(str_extract(cluster_id, "\\d+$"))
  ) %>%
  arrange(estado_ancla, clues_ancla, orden_cluster, cluster_id)

# Validación
clusters_alex %>% summarise(total_clusters = n())

# 3) Crear plantilla 75 x 5 = 375

base_necesidad_375 <- clusters_alex %>%
  crossing(puestos_requeridos) %>%
  arrange(
    estado_ancla,
    clues_ancla,
    orden_cluster,
    match(puesto_arm, orden_puestos)
  )

base_necesidad_375 %>%
  summarise(total_renglones = n())

# 4) Candidatos aprobados UAS (bolsa estatal) 

candidatos_uas <- base_final_1 %>%
  mutate(
    estado_ancla = str_trim(str_to_upper(estado_ancla)),
    curp = str_trim(str_to_upper(curp)),
    cnpm = str_trim(str_to_upper(cnpm)),
    estatus_uas = str_trim(str_to_upper(estatus_uas)),
    
    puesto_arm = case_when(
      cnpm == "MG001" ~ "Medicina General",
      cnpm == "ME001" ~ "Anestesiologia",
      cnpm == "ME002" ~ "Cirugia",
      cnpm %in% c("EN002", "EN005") ~ "Enfermeria quirurgica",
      cnpm %in% c("PA022", "PA020") ~ "Chofer",
      TRUE ~ NA_character_
    ),
    
    fuente_candidato = "Base validada UAS"
  ) %>%
  filter(
    !is.na(puesto_arm),
    !is.na(estado_ancla),
    estado_ancla != "",
    is.na(estatus_uas) | estatus_uas == "" | estatus_uas == "APROBADO"
  ) %>%
  mutate(
    prioridad_estatus = case_when(
      estatus_uas == "APROBADO" ~ 1,
      is.na(estatus_uas) | estatus_uas == "" ~ 2,
      TRUE ~ 3
    ),
    
    prioridad_cnpm = case_when(
      puesto_arm == "Enfermeria quirurgica" & cnpm == "EN005" ~ 1,
      puesto_arm == "Enfermeria quirurgica" & cnpm == "EN002" ~ 2,
      puesto_arm == "Chofer" & cnpm == "PA020" ~ 1,
      puesto_arm == "Chofer" & cnpm == "PA022" ~ 2,
      TRUE ~ 1
    )
  ) %>%
  arrange(
    estado_ancla,
    puesto_arm,
    prioridad_estatus,
    prioridad_cnpm,
    nombre
  ) %>%
  group_by(estado_ancla, puesto_arm) %>%
  mutate(slot_estado = row_number()) %>%
  ungroup() %>%
  select(
    estado_ancla,
    puesto_arm,
    slot_estado,
    nombre,
    curp,
    enlace_a_carpeta,
    estatus_uas,
    fuente_candidato,
    codigo_cnpm_uas = cnpm,
    nombre_del_puesto_uas = puesto
  )

# 5) Crear slots por estado 
# Ejemplo:
# Chiapas tiene 3 clusters
# entonces tendrá:
# 3 Cirugía, 3 Anestesia, 3 Enfermería...

base_necesidad_375 <- base_necesidad_375 %>%
  group_by(estado_ancla, puesto_arm) %>%
  arrange(
    estado_ancla,
    puesto_arm,
    cluster_id,
    .by_group = TRUE
  ) %>%
  mutate(
    slot_estado = row_number()
  ) %>%
  ungroup()

# 6) Asignar candidatos desde bolsa estatal 

base_estado_375_con_uas <- base_necesidad_375 %>%
  left_join(
    candidatos_uas,
    by = c("estado_ancla", "puesto_arm", "slot_estado")
  ) %>%
  mutate(
    ocupado_con_uas = !is.na(curp),
    
    estatus_ocupacion = case_when(
      ocupado_con_uas ~ "Cubierto con candidato UAS",
      TRUE ~ "Vacante / sin candidato UAS"
    ),
    
    codigo_cnpm = coalesce(
      codigo_cnpm_uas,
      codigo_cnpm
    ),
    
    nombre_del_puesto = coalesce(
      nombre_del_puesto_uas,
      nombre_del_puesto
    )
  ) %>%
  select(
    estado_ancla,
    cluster_id,
    codigo_cnpm,
    clues_ancla,
    nombre_del_ancla,
    nombre_del_puesto,
    puesto_arm,
    nombre,
    curp,
    enlace_a_carpeta,
    estatus_uas,
    fuente_candidato,
    ocupado_con_uas,
    estatus_ocupacion
  ) %>% 
  arrange(estado_ancla,
          cluster_id,
          codigo_cnpm)

# 7) Validaciones 

# Deben seguir siendo 375 filas
base_estado_375_con_uas %>%
  summarise(
    total_renglones = n(),
    total_clusters = n_distinct(cluster_id)
  )

# Cada cluster sigue teniendo 5 puestos
base_estado_375_con_uas %>%
  count(cluster_id) %>%
  count(n)

# Ver cobertura por estado
resumen_estado <- base_estado_375_con_uas %>%
  group_by(estado_ancla) %>%
  summarise(
    clusters = n_distinct(cluster_id),
    puestos_totales = n(),
    cubiertos = sum(ocupado_con_uas, na.rm = TRUE),
    vacantes = sum(!ocupado_con_uas, na.rm = TRUE),
    .groups = "drop"
  )

# Ejemplo: revisar Chiapas
base_estado_375_con_uas %>%
  filter(estado_ancla == "CHIAPAS") %>%
  count(cluster_id, puesto_arm)

# 8) Exportar 

writexl::write_xlsx(
  list(
    base_estado_375_con_uas = base_estado_375_con_uas,
    resumen_estado = resumen_estado
  ),
  "C:/Users/Cecilia Pereo/Downloads/base_375_distribucion_estatal.xlsx"
)
