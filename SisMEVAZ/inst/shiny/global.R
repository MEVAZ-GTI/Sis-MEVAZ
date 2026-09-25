# =====================================================================
# MEVAZ - interface Shiny
# global.R  -  carregado antes de ui.R e server.R
#
# Fase 0 (esqueleto): pacotes, deteccao da pasta 'dados', paleta FUNCEME
# e carga de alguns dados leves para a carta. Os datasets pesados de
# drenagem e a logica geoespacial ficam em SisMEVAZ/inst/shiny/R/.
# =====================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(sf)
  library(openxlsx)
  library(shinyjs)
  library(DT)
  library(plotly)
})

sf::sf_use_s2(FALSE)

# ---------------------------------------------------------------------
# Localizacao da pasta 'dados' (espelha a deteccao do app Python)
# ---------------------------------------------------------------------
find_base_dir <- function() {
  configured_dir <- Sys.getenv("SISMEVAZ_BASE_DIR", unset = "")
  if (nzchar(configured_dir) && dir.exists(file.path(configured_dir, "dados"))) {
    return(normalizePath(configured_dir, winslash = "/"))
  }

  app_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  candidates <- unique(c(app_dir, dirname(app_dir), dirname(dirname(app_dir))))

  # 1) pasta 'dados' ao lado do projeto
  for (d in candidates) {
    if (dir.exists(file.path(d, "dados"))) {
      return(normalizePath(d, winslash = "/"))
    }
  }
  stop("Pasta 'dados' nao encontrada na raiz do projeto.")
}

BASE_DIR        <- find_base_dir()
DADOS_FIXOS     <- file.path(BASE_DIR, "dados", "Dados_fixos")
DADOS_DASH      <- file.path(DADOS_FIXOS, "Dados_dash")
PLU_ETP         <- file.path(BASE_DIR, "dados", "Plu_ETP")
REDE_RES_CONS   <- file.path(BASE_DIR, "dados", "RededeReservatorios_consolidada")
REDE_RES_TEST   <- file.path(BASE_DIR, "dados", "RededeReservatorios_em_teste")
# Arquivos intermediarios geoespaciais precisam existir em disco porque o
# WhiteboxTools trabalha com caminhos de entrada/saida. Eles nao fazem parte
# dos resultados persistentes da simulacao, portanto ficam em um diretorio
# temporario exclusivo deste processo Shiny e sao removidos ao encerra-lo.
WORK_SHAPES_DIR <- tempfile("sismevaz_workspace_")
dir.create(WORK_SHAPES_DIR, showWarnings = FALSE, recursive = TRUE)
shiny::onStop(function() {
  unlink(WORK_SHAPES_DIR, recursive = TRUE, force = TRUE)
})

work_shape_path <- function(filename) file.path(WORK_SHAPES_DIR, filename)

# ---------------------------------------------------------------------
# Paleta institucional FUNCEME
# ---------------------------------------------------------------------
FUNCEME_GREEN      <- "#00A651"
FUNCEME_GREEN_DARK <- "#00793C"
FUNCEME_TEAL       <- "#12B0A0"
FUNCEME_ORANGE     <- "#F7941D"

CENTER_LAT <- -5.2
CENTER_LON <- -39.5

# ---------------------------------------------------------------------
# Dados carregados pela interface
# ---------------------------------------------------------------------
BASINS_SF <- sf::st_read(
  file.path(DADOS_DASH, "regioeshidrograficas_ce", "regioeshidrograficas_ce.shp"),
  quiet = TRUE
)
BASINS_SF <- sf::st_transform(BASINS_SF, 4326)

# Drenagem (Rios_mSimpl) - ja projetada em SIRGAS2000/UTM24S, mesma do Python.
DRAINAGE_SF <- sf::st_read(
  file.path(DADOS_FIXOS, "Carac_Fisiograficas", "Drenagem", "Rios_mSimpl_1_100000_SRH.shp"),
  quiet = TRUE
)

# Vetor de rios ANADEM (sem CRS, igual ao Python) - rede usada por
# find_nearest_point() para o encaixe (snap) do clique/ponto digitado, e
# desenhada na carta (resolucao total) para guiar o usuario a clicar sobre um
# canal real em vez de um ponto arbitrario fora da drenagem.
#
# NOTA: chegou-se a simplificar esta camada (st_simplify, ~111m) para reduzir
# o payload enviado ao navegador (605 mil vertices -> ~74 mil), mas em zooms
# proximos isso cortava visivelmente as curvas do canal - o proprio ponto
# desta camada e guiar um clique preciso, entao a precisao total e mais
# importante que o tempo de carregamento. Revertido para resolucao total.
VETOR_RIOS_SF <- sf::st_read(
  file.path(DADOS_DASH, "vetor_rios_10000_10__anadem", "vetor_rios_10000_10__anadem_v1_24M_recortado_CE.shp"),
  quiet = TRUE
)
VETOR_RIOS_LL <- sf::st_set_crs(VETOR_RIOS_SF, 4326)

STATIONS_DF <- openxlsx::read.xlsx(file.path(PLU_ETP, "Estacoes_Evaporacao.xlsx"))

# Tabela de atributos base (regiao hidrografica por id_sagreh) e exutorios.
# NOTA: este arquivo especifico nao tem um stylesheet XLSX padrao (mesmo aviso
# aparece com openpyxl no Python) e o openxlsx::read.xlsx falha silenciosamente
# em ler os nomes das 5 primeiras colunas (retorna X1..X5). O readxl usa outro
# parser e le corretamente; os demais arquivos .xlsx do projeto sao lidos bem
# por openxlsx e continuam usando-o.
TABELA_ATRIBUTOS_BASE <- as.data.frame(readxl::read_excel(
  file.path(DADOS_DASH, "Tabela_atributos_acudes.xlsx")
))
EXUT_ACUDES_SF <- sf::st_read(file.path(DADOS_DASH, "exut_acudes", "exut_acudes.shp"), quiet = TRUE)

# Rede consolidada (todas as colunas - usada por ensure_simulation_modreg_input,
# pela geracao de Dados_Novos e pela Opcao A).
tabela_reservatorios_consolidada <- openxlsx::read.xlsx(
  file.path(REDE_RES_CONS, "input", "Dados_Reservatorios.xlsx")
)
load_consolidated_reservoirs <- function() {
  openxlsx::read.xlsx(file.path(REDE_RES_CONS, "input", "Dados_Reservatorios.xlsx"))
}

EXISTING_RESERVOIR_IDS <- sort(unique(stats::na.omit(
  suppressWarnings(as.integer(tabela_reservatorios_consolidada$ID))
)))

message("SisMEVAZ/Shiny: BASE_DIR = ", BASE_DIR)
message("SisMEVAZ/Shiny: ", length(EXISTING_RESERVOIR_IDS), " reservatorios consolidados carregados.")

# Helpers geoespaciais/logicos portados do Python (Fase 1)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# Limpa entradas de uma simulacao anterior a cada inicializacao do app
# (equivalente ao "if __name__ == '__main__': clear_previous_simulation_data()"
# do Python). Roda uma vez, quando o processo R arranca.
clear_previous_simulation_data()

# ---------------------------------------------------------------------
# Funcoes auxiliares de UI (Fase 2). Precisam estar em global.R (nao em
# ui.R) para ficarem visiveis em server.R - o Shiny carrega ui.R num
# ambiente isolado, so para extrair o objeto 'ui'.
# ---------------------------------------------------------------------

# ---- Modal do CAV (Cota-Area-Volume) ----
cav_modal_ui <- function() {
  shiny::modalDialog(
    title = "Insira Dados de Cota, Área e Volume",
    size = "l",
    easyClose = FALSE,
    shiny::fileInput("upload_cav_data", "Arraste e solte ou selecione um arquivo",
                     accept = c(".csv", ".xlsx", ".xls")),
    shiny::tags$hr(),
    shiny::tags$p("Ou insira manualmente os dados:"),
    shiny::fluidRow(
      shiny::column(4, shiny::numericInput("cota_input", "Cota (m)", value = NA)),
      shiny::column(4, shiny::numericInput("area_input", "Área (km²)", value = NA)),
      shiny::column(4, shiny::numericInput("volume_input", "Volume (m³)", value = NA))
    ),
    shiny::uiOutput("input_feedback"),
    shiny::fluidRow(
      shiny::column(12, shiny::numericInput("input_cota_sang", "Cota do Sangrador (m)", value = NA))
    ),
    DT::DTOutput("cav_table"),
    shiny::uiOutput("save_feedback"),
    shiny::div(
      id = "cav_graph_container", style = "display:none; margin-top:15px;",
      plotly::plotlyOutput("cav_graph")
    ),
    footer = shiny::tagList(
      shiny::actionButton("add_data_button", "Adicionar Dados", class = "btn-primary"),
      shiny::actionButton("ver_grafico_button", "Ver Gráfico", class = "btn-primary"),
      shiny::actionButton("confirm_save_button", "Confirmar e Salvar", class = "btn-success"),
      shiny::modalButton("Fechar")
    )
  )
}

# ---- Modal generico de "processando..." (Trace a Bacia / Cascata / ponto / simulacao) ----
processing_modal_ui <- function(title_text) {
  shiny::modalDialog(
    title = "Processando",
    easyClose = FALSE,
    footer = NULL,
    shiny::div(
      style = "text-align:center;",
      shiny::tags$div(class = "spinner-border", role = "status", style = "width:3rem;height:3rem;"),
      shiny::tags$h4(title_text, style = "margin-top:14px;"),
      shiny::tags$p("Por favor, aguarde...")
    )
  )
}

# ---- Uma etapa do assistente: caixa branca com liseira verde ----
step_box <- function(step_id, title_text, ...) {
  shiny::conditionalPanel(
    condition = sprintf("output.wizard_step_js == '%s'", step_id),
    shiny::div(class = "step-box", shiny::h5(title_text), ...)
  )
}

half_button_row <- function(back_id, back_label, next_id, next_label) {
  shiny::div(style = "display:flex; gap:8px;",
     shiny::actionButton(back_id, back_label, class = "btn-outline-primary rounded-pill w-100"),
     shiny::actionButton(next_id, next_label, class = "btn-primary rounded-pill w-100")
  )
}
