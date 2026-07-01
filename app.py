import streamlit as st
import pandas as pd
import numpy as np
from pathlib import Path
from io import BytesIO

st.set_page_config(
    page_title="Plazas UAS por cluster",
    layout="wide"
)

# -------------------------
# Acceso
# -------------------------

password = st.text_input(
    "Contraseña de acceso",
    type="password"
)

if password != "UAS2026":
    st.stop()
# -------------------------
# Cargar base
# -------------------------
@st.cache_data
def cargar_base():

    ruta = "data/base_375_clusters_uas_validada.xlsx"

    df = pd.read_excel(ruta)

    df.columns = (
        df.columns
        .str.strip()
        .str.lower()
        .str.replace(r"\s+", "_", regex=True)
        .str.replace(r"[^\w]", "", regex=True)
    )

    # limpiar texto sin romper links
    for col in df.select_dtypes(include=["object", "string"]).columns:

        # NO tocar links
        if col == "enlace_a_carpeta":

            df[col] = (
                df[col]
                .astype("string")
                .str.strip()
            )

        else:

            df[col] = (
                df[col]
                .astype("string")
                .str.strip()
                .str.upper()
            )

    return df


df = cargar_base()

@st.cache_data
def cargar_base_alex():
    base_alex = pd.read_csv("data/base_alex.csv")

    base_alex.columns = (
        base_alex.columns
        .str.strip()
        .str.lower()
        .str.replace(r"\s+", "_", regex=True)
        .str.replace(r"[^\w]", "", regex=True)
    )

    base_alex["clues_ancla"] = (
        base_alex["clues_ancla"]
        .astype(str)
        .str.strip()
        .str.upper()
    )

    return base_alex


base_alex = cargar_base_alex()

clues_validas = base_alex["clues_ancla"].unique()

# -------------------------
# Cargar base Carlos
# -------------------------

@st.cache_data
def cargar_hbc():

    hbc = pd.read_excel(
        "data/cluster_19_carlos_long_simple.xlsx"
    )

    hbc.columns = (
        hbc.columns
        .str.strip()
        .str.lower()
        .str.replace(r"\s+", "_", regex=True)
        .str.replace(r"[^\w]", "", regex=True)
    )

    return hbc


hbc = cargar_hbc()

vector_ancla_cluster = pd.concat([

    hbc["clues_imb"].astype(str),

    hbc["nombre_cluster"].astype(str).str[:11]

]).dropna().unique()

vector_ancla_cluster = (
    pd.Series(vector_ancla_cluster)
    .astype(str)
    .str.strip()
    .str.upper()
    .unique()
)

# -------------------------
# Leer Google Sheet vivo
# -------------------------

@st.cache_data(ttl=300)
def cargar_base_online():
    sheet_id = "1Axj6z0U1odyFcdDz7Rjdv2jOZgnh9yIxKAXiWzvXKW4"
    gid = "0"

    url_csv = f"https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=csv&gid={gid}"

    base_online = pd.read_csv(url_csv)

    base_online.columns = (
        base_online.columns
        .str.strip()
        .str.lower()
        .str.replace(r"\s+", "_", regex=True)
        .str.replace(r"[^\w]", "", regex=True)
    )

    for col in base_online.select_dtypes(include=["object", "string"]).columns:

        if col == "link_carpeta":

            base_online[col] = (
                base_online[col]
                .astype("string")
                .str.strip()
            )

        else:

            base_online[col] = (
                base_online[col]
                .astype("string")
                .str.strip()
                .str.upper()
            )

    return base_online


base_online_gs = cargar_base_online()

# -------------------------
# Procesar Google Sheet vivo con lógica UAS
# -------------------------

def limpiar_texto(serie):
    return (
        serie
        .fillna("")
        .astype(str)
        .str.strip()
        .str.upper()
    )

def limpiar_curp(serie):
    return (
        serie
        .fillna("")
        .astype(str)
        .str.replace(r"['\"`´“”‘’]", "", regex=True)
        .str.replace(r"\s+", "", regex=True)
        .str.strip()
        .str.upper()
    )

base_online_match = base_online_gs.copy()

# -------------------------
# Filtro equivalente a R
# turno itinerante OR fase 3 en clusters Carlos
# -------------------------

cond_turno = (
    base_online_match["turno"]
    .fillna("")
    .astype(str)
    .str.upper()
    .str.contains("ITINERANTE")
)

cond_fase = (
    (
        pd.to_numeric(
            base_online_match["fase"],
            errors="coerce"
        ) == 3
    )
    &
    (
        base_online_match["clues"]
        .fillna("")
        .astype(str)
        .str.strip()
        .str.upper()
        .isin(vector_ancla_cluster)
    )
)

base_online_match = base_online_match[
    cond_turno | cond_fase
].copy()

base_online_match["curp_limpia"] = limpiar_curp(base_online_match["curp"])

clave_puesto_txt = limpiar_texto(base_online_match["clave_puesto"])
cnpm_txt = limpiar_texto(base_online_match["cnpm"])
revision_txt = limpiar_texto(base_online_match["revision_uas"])

condiciones = [
    clave_puesto_txt.eq("ME002 CIRUGIA GENERAL"),
    clave_puesto_txt.eq("OP057 CHOFER PROMOTOR POLIVALENTE"),
    clave_puesto_txt.eq("MG001 MEDICINA GENERAL"),
    clave_puesto_txt.eq("OP065 AUXILIAR ADMINISTRATIVO (CHOFER)"),
    clave_puesto_txt.eq("EN005 ENFERMERA ESPECIALISTA CIRUGIA"),
    clave_puesto_txt.eq("CHOFER PROMOTOR POLIVALENTE"),
    clave_puesto_txt.eq("CHOFER POLIVALENTE"),
    clave_puesto_txt.eq("AUXILIAR ADMINISTRATIVO (CHOFER)"),
    clave_puesto_txt.eq("CIRUGIA GENERAL"),
    clave_puesto_txt.eq("ANESTESIOLOGIA"),
    clave_puesto_txt.eq("ENFERMERA ESPECIALISTA CIRUGIA"),
    clave_puesto_txt.eq("MEDICINA GENERAL"),
    cnpm_txt.eq("OP057"),
    cnpm_txt.eq("OP065"),
]

valores = [
    "ME002",
    "PA020",
    "MG001",
    "PA022",
    "EN005",
    "PA020",
    "PA022",
    "PA022",
    "ME002",
    "ME001",
    "EN005",
    "MG001",
    "PA020",
    "PA022",
]

base_online_match["codigo_cnpm"] = np.select(
    condiciones,
    valores,
    default=cnpm_txt
)

base_online_match["codigo_cnpm"] = np.where(
    base_online_match["codigo_cnpm"] == "",
    clave_puesto_txt,
    base_online_match["codigo_cnpm"]
)

base_online_match["codigo_cnpm"] = limpiar_texto(base_online_match["codigo_cnpm"])

# Filtrar CNPM válidos y aprobados/blancos
cnpm_validos = ["PA020", "PA022", "ME001", "ME002", "MG001", "EN005", "EN002"]

base_online_match = base_online_match[
    base_online_match["codigo_cnpm"].isin(cnpm_validos)
    &
    (
        revision_txt.eq("")
        | revision_txt.eq("APROBADO")
    )
].copy()

# Prioridad por CURP
revision_txt = limpiar_texto(base_online_match["revision_uas"])

base_online_match["prioridad_estatus"] = np.select(
    [
        revision_txt.eq("APROBADO"),
        revision_txt.eq("")
    ],
    [1, 2],
    default=3
)

base_online_match = (
    base_online_match
    .sort_values(["curp_limpia", "prioridad_estatus"])
    .drop_duplicates(subset=["curp_limpia"], keep="first")
    .drop(columns="prioridad_estatus")
)

# Recodificar CLUES ancla
base_online_match["clues_ancla"] = limpiar_texto(base_online_match["clues"])

reemplazos_clues = {
    "PLIMB003706": "PLIMB002516",
    "SLIMB001950": "SLIMB002930",
    "SLIMB000195": "SLIMB002930",
    "SLIMB001554": "SLIMB002930",
    "BSIMB000503": "BSIMB000754",
    "TSIMB003483": "TSIMB001260",
}

base_online_match["clues_ancla"] = base_online_match["clues_ancla"].replace(
    reemplazos_clues
)

# Filtrar solo universe Alex

base_online_match = base_online_match[
    base_online_match["clues_ancla"].isin(clues_validas)
].copy()

base_online_match["curp"] = base_online_match["curp_limpia"]

# -------------------------
# Detectar casos nuevos
# -------------------------

base_local_match = df[["clues_ancla", "curp", "codigo_cnpm"]].copy()

base_local_match["clues_ancla"] = limpiar_texto(base_local_match["clues_ancla"])
base_local_match["curp"] = limpiar_curp(base_local_match["curp"])
base_local_match["codigo_cnpm"] = limpiar_texto(base_local_match["codigo_cnpm"])

base_local_match = base_local_match.drop_duplicates()
base_local_match["existe_en_base_local"] = True

casos_nuevos = base_online_match.merge(
    base_local_match,
    on=["clues_ancla", "curp", "codigo_cnpm"],
    how="left"
)

casos_nuevos = casos_nuevos[
    casos_nuevos["existe_en_base_local"].isna()
].copy()

casos_nuevos["estatus_ocupacion"] = "Caso nuevo"

# -------------------------
# Separar casos nuevos vs candidatos en espera
# considerando puestos equivalentes
# -------------------------

def puesto_equivalente(codigo):
    codigo = str(codigo).strip().upper()

    if codigo in ["EN002", "EN005"]:
        return "ENFERMERIA_QX"

    if codigo in ["PA020", "PA022"]:
        return "CHOFER"

    return codigo


plazas_local = df.copy()

plazas_local["clues_ancla"] = limpiar_texto(plazas_local["clues_ancla"])
plazas_local["codigo_cnpm"] = limpiar_texto(plazas_local["codigo_cnpm"])
plazas_local["puesto_equivalente"] = plazas_local["codigo_cnpm"].apply(puesto_equivalente)

plazas_local["plaza_ocupada_local"] = (
    plazas_local["ocupado_con_uas"].fillna(False).astype(bool)
)

plazas_local = (
    plazas_local
    .groupby(["clues_ancla", "puesto_equivalente"], dropna=False)
    .agg(
        plaza_ocupada_local=("plaza_ocupada_local", "max")
    )
    .reset_index()
)

casos_nuevos["clues_ancla"] = limpiar_texto(casos_nuevos["clues_ancla"])
casos_nuevos["codigo_cnpm"] = limpiar_texto(casos_nuevos["codigo_cnpm"])
casos_nuevos["puesto_equivalente"] = casos_nuevos["codigo_cnpm"].apply(puesto_equivalente)

casos_clasificados = casos_nuevos.merge(
    plazas_local,
    on=["clues_ancla", "puesto_equivalente"],
    how="left"
)

casos_clasificados["plaza_ocupada_local"] = (
    casos_clasificados["plaza_ocupada_local"]
    .fillna(False)
    .astype(bool)
)

candidatos_en_espera = casos_clasificados[
    casos_clasificados["plaza_ocupada_local"]
].copy()

candidatos_en_espera["estatus_ocupacion"] = "Candidato en espera"

casos_nuevos = casos_clasificados[
    ~casos_clasificados["plaza_ocupada_local"]
].copy()

casos_nuevos["estatus_ocupacion"] = "Caso nuevo"
# -------------------------
# Estilos institucionales
# -------------------------

st.markdown(
    """
    <style>
    :root {
        --vino: #611232;
        --dorado: #BC955C;
        --verde: #235B4E;
        --gris-fondo: #F7F4EF;
        --gris-borde: #E6DED4;
    }

    /* márgenes generales */
    .block-container {
        padding-top: 1rem;
        padding-left: 3rem;
        padding-right: 3rem;
    }

    /* barra institucional */
    .institucional-bar {
        background: linear-gradient(90deg, #611232, #235B4E);
        height: 10px;
        border-radius: 0px 0px 10px 10px;
        margin-bottom: 2rem;
    }

    /* títulos */
    h1 {
        color: #611232;
        font-weight: 800;
        margin-bottom: 0.2rem;
    }

    h2, h3 {
        color: #611232;
        font-weight: 700;
    }

    /* métricas */
    [data-testid="stMetric"] {
        background-color: white;
        border: 1px solid #E6DED4;
        border-top: 5px solid #BC955C;
        border-radius: 14px;
        padding: 16px;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.04);
    }

    [data-testid="stMetricLabel"] {
        color: #611232;
        font-weight: 700;
    }

    [data-testid="stMetricValue"] {
        color: #235B4E;
        font-weight: 800;
    }

    /* botones */
    div.stDownloadButton > button {
        background-color: #611232;
        color: white;
        border-radius: 10px;
        border: none;
        font-weight: 700;
    }

    div.stDownloadButton > button:hover {
        background-color: #235B4E;
        color: white;
    }

    /* tablas */
    [data-testid="stDataFrame"] {
        border: 1px solid #E6DED4;
        border-radius: 10px;
    }

    </style>
    """,
    unsafe_allow_html=True
)

st.markdown(
    '<div class="institucional-bar"></div>',
    unsafe_allow_html=True
)

# -------------------------
# Encabezado
# -------------------------

logo_path = Path("assets/bienestar.png")

col_logo, col_titulo = st.columns([1.1, 8])

with col_logo:
    if logo_path.exists():
        st.image(
            str(logo_path),
            width=110
        )

with col_titulo:
    st.title("Seguimiento de plazas UAS por cluster")

# -------------------------
# Filtros
# -------------------------

st.markdown(
    """
    <div style="
        background-color:#F7F4EF;
        border-left:6px solid #611232;
        border-radius:12px;
        padding:12px 18px;
        margin-bottom:16px;
    ">
        <b style="color:#611232; font-size:22px;">
            Consulta de plazas
        </b><br>
        <span style="color:#555;">
            Filtra por cluster, puesto o estatus de ocupación.
        </span>
    </div>
    """,
    unsafe_allow_html=True
)

f1, f2, f3 = st.columns(3)

with f1:
    cluster_sel = st.multiselect(
        "Cluster ID",
        sorted(df["cluster_id"].dropna().unique())
    )

with f2:
    puesto_sel = st.multiselect(
        "Puesto",
        sorted(df["puesto_arm"].dropna().unique())
    )

with f3:
    estatus_sel = st.multiselect(
        "Estatus ocupación",
        sorted(df["estatus_ocupacion"].dropna().unique())
    )
base_filtrada = df.copy()

if cluster_sel:
    base_filtrada = base_filtrada[
        base_filtrada["cluster_id"].isin(cluster_sel)
    ]

if puesto_sel:
    base_filtrada = base_filtrada[
        base_filtrada["puesto_arm"].isin(puesto_sel)
    ]

if estatus_sel:
    base_filtrada = base_filtrada[
        base_filtrada["estatus_ocupacion"].isin(estatus_sel)
    ]


# -------------------------
# Indicadores
# -------------------------

total_plazas = len(base_filtrada)
cubiertas = base_filtrada["ocupado_con_uas"].sum()
vacantes = total_plazas - cubiertas
pct_cobertura = cubiertas / total_plazas if total_plazas > 0 else 0

c1, c2, c3, c4 = st.columns(4)

c1.metric("Plazas totales", total_plazas)
c2.metric("Plazas cubiertas", int(cubiertas))
c3.metric("Plazas pendientes", int(vacantes))
c4.metric("% cobertura", f"{pct_cobertura:.1%}")

# -------------------------
# Resumen
# -------------------------

st.subheader("Resumen de plazas faltantes por cluster")

resumen_cluster = (
    base_filtrada
    .assign(vacante=lambda x: ~x["ocupado_con_uas"].astype(bool))
    .groupby(["cluster_id", "nombre_del_ancla"], dropna=False)
    .agg(
        plazas_totales=("cluster_id", "size"),
        plazas_cubiertas=("ocupado_con_uas", "sum"),
        plazas_faltantes=("vacante", "sum")
    )
    .reset_index()
)

resumen_cluster["pct_cobertura"] = (
    resumen_cluster["plazas_cubiertas"] /
    resumen_cluster["plazas_totales"]
)

# Colorear renglones según cobertura
def colorear_renglon(row):
    pct = row["pct_cobertura"]

    if pct >= 0.80:
        color = "#D8F3DC"   # verde suave
    elif pct >= 0.40:
        color = "#FFF3CD"   # amarillo suave
    else:
        color = "#F8D7DA"   # rojo suave

    return [f"background-color: {color}"] * len(row)


resumen_cluster_ordenado = resumen_cluster.sort_values(
    ["plazas_faltantes", "pct_cobertura"],
    ascending=[False, True]
)

tabla_resumen = (
    resumen_cluster_ordenado
    .style
    .apply(colorear_renglon, axis=1)
    .format({
        "pct_cobertura": "{:.1%}",
        "plazas_totales": "{:.0f}",
        "plazas_cubiertas": "{:.0f}",
        "plazas_faltantes": "{:.0f}"
    })
)

st.dataframe(
    tabla_resumen,
    use_container_width=True
)
# -------------------------
# Tabla editable
# -------------------------
st.subheader("Base de plazas por cluster")

columnas_editables = [
    "nombre",
    "curp",
    "enlace_a_carpeta",
    "estatus_uas",
    "fuente_candidato",
    "estatus_ocupacion"
]

columnas_mostrar = [
    "cluster_id",
    "clues_ancla",
    "nombre_del_ancla",
    "codigo_cnpm",
    "nombre_del_puesto",
    "puesto_arm",
    "nombre",
    "curp",
    "enlace_a_carpeta",
    "estatus_uas",
    "fuente_candidato",
    "estatus_ocupacion"
]

base_edicion = base_filtrada[columnas_mostrar].copy()

base_editada = st.data_editor(
    base_edicion,
    use_container_width=True,
    num_rows="dynamic",
    disabled=[
        col for col in columnas_mostrar
        if col not in columnas_editables
    ],
    column_config={
        "enlace_a_carpeta": st.column_config.LinkColumn(
            "Carpeta",
            display_text="📁 Abrir carpeta"
        )
    }
)
# -------------------------
# Seguimiento Google Sheet
# -------------------------

st.markdown("---")
st.subheader("Seguimiento de candidatos detectados en Google Sheet")

# columnas relevantes
columnas_seguimiento = [
    "estado",
    "clues_ancla",
    "codigo_cnpm",
    "clave_puesto",
    "curp",
    "revision_uas",
    "link_carpeta"
]

# solo columnas que existan
columnas_seguimiento = [
    c for c in columnas_seguimiento
    if c in casos_nuevos.columns
]


# Tabs
tab1, tab2 = st.tabs(
    [
        "🆕 Nuevos candidatos",
        "⏳ Candidatos en espera"
    ]
)

# -------------------------
# Tab casos nuevos
# -------------------------

with tab1:

    c1, c2 = st.columns([1, 6])

    with c1:
        st.metric(
            "Detectados",
            len(casos_nuevos)
        )

    with c2:
        st.caption(
            "Candidatos identificados en Google Sheet que no existen en la base actual."
        )

    st.dataframe(
        casos_nuevos[
            columnas_seguimiento
        ],
        use_container_width=True,
        column_config={
            "link_carpeta": st.column_config.LinkColumn(
                "Carpeta",
                display_text="📁 Abrir carpeta"
            )
        }
    )


# -------------------------
# Tab candidatos en espera
# -------------------------

with tab2:

    c1, c2 = st.columns([1, 6])

    with c1:
        st.metric(
            "En espera",
            len(candidatos_en_espera)
        )

    with c2:
        st.caption(
            "Candidatos detectados para plazas que actualmente ya cuentan con cobertura."
        )

    st.dataframe(
        candidatos_en_espera[
            columnas_seguimiento
        ],
        use_container_width=True,
        column_config={
            "link_carpeta": st.column_config.LinkColumn(
                "Carpeta",
                display_text="📁 Abrir carpeta"
            )
        }
    )
# -------------------------
# Descargar
# -------------------------

def convertir_excel(base, resumen):

    output = BytesIO()

    with pd.ExcelWriter(
        output,
        engine="openpyxl"
    ) as writer:

        base.to_excel(
            writer,
            index=False,
            sheet_name="base_editada"
        )

        resumen.to_excel(
            writer,
            index=False,
            sheet_name="resumen_cluster"
        )

    return output.getvalue()


st.download_button(
    label="Descargar Excel editado",
    data=convertir_excel(
        base_editada,
        resumen_cluster
    ),
    file_name="base_uas_editada.xlsx",
    mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)

def convertir_casos_nuevos_excel(casos, espera):
    output = BytesIO()

    with pd.ExcelWriter(output, engine="openpyxl") as writer:
        casos.to_excel(writer, index=False, sheet_name="casos_nuevos")
        espera.to_excel(writer, index=False, sheet_name="candidatos_en_espera")

    return output.getvalue()

st.download_button(
    label="Descargar seguimiento Google Sheet",
    data=convertir_casos_nuevos_excel(
        casos_nuevos,
        candidatos_en_espera
    ),
    file_name="seguimiento_google_sheet.xlsx",
    mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
)