# =====================================================================
# MEVAZ - versao Shiny
# ui.R  -  Assistente de 7 etapas + tema FUNCEME.
#          O server.R controla validacoes, processamento e navegacao.
#
# NOTA: as funcoes auxiliares de UI (cav_modal_ui, processing_modal_ui,
# step_box, half_button_row) ficam em global.R, e nao aqui - o Shiny carrega
# ui.R num ambiente isolado (so para extrair o objeto 'ui'), entao funcoes
# definidas aqui nao ficam visiveis para o server.R (ex.: showModal(cav_modal_ui())).
# =====================================================================

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(
    version = 5,
    primary = FUNCEME_GREEN,
    danger = "#dc3545",
    "border-radius" = "0.5rem"
  ),
  title = "MEVAZ",
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "funceme_theme.css?v=20261015-reservoir-sheet"),
    tags$script(src = "simulation_timer.js", defer = NA)
  ),

  # ---- Cabecalho institucional FUNCEME ----
  tags$div(
    class = "funceme-header",
    tags$div(class = "funceme-logo-badge",
             tags$img(src = "logo_funceme.png", alt = "FUNCEME e Governo do Estado do Ceará")),
    tags$div(
      class = "funceme-header-text",
      tags$p("MEVAZ", class = "funceme-header-title"),
      tags$p("Metodologia para o Cálculo de Vazões — Secretaria dos Recursos Hídricos",
             class = "funceme-header-subtitle"),
      tags$p("Sis-MEVAZ é um assistente de inclusão de novos reservatórios na rede de açudes do Ceará e simulação de seus impactos hidrológicos",
             class = "funceme-header-objective")
    )
  ),

  # Saidas ocultas usadas so para pilotar os conditionalPanel (sempre ativas).
  tags$div(
    style = "display:none;",
    textOutput("wizard_step_js"),
    textOutput("wizard_branch_js")
  ),

  tags$div(
    class = "mevaz-body",
    tags$div(
      class = "mevaz-sidebar",

      # ---- Barra e rotulo de progresso ----
      div(class = "wizard-progress-bar",
         div(id = "wizard_progress_fill", class = "wizard-progress-fill", style = "width: 0%;")
      ),
      uiOutput("wizard_progress_label"),

      # ===== ETAPA 1: Identificacao =====
      step_box("1", "Etapa 1 — Identificação do novo açude",
        div(class = "mevaz-field",
           div(class = "mevaz-code-label",
             tags$label("Código do Novo Reservatório:", class = "mevaz-label", `for` = "input_cod"),
             tags$span(class = "mevaz-code-help",
               tags$button("ⓘ", type = "button", class = "mevaz-info-button",
                 `aria-label` = "Informação sobre código válido", `aria-describedby` = "codigo_help"),
               tags$span(id = "codigo_help", class = "mevaz-help-text", role = "tooltip",
                 "Código válido é um número inteiro ainda não utilizado na rede consolidada de reservatórios. Não use letras, símbolos ou casas decimais.")
             )
           ),
           numericInput("input_cod", NULL, value = NA, width = "100%"),
           actionButton("validate_codigo_button", "Validar Código", class = "btn-outline-primary rounded-pill w-100"),
           uiOutput("codigo_validation_feedback")
        ),
        div(class = "mevaz-field",
           tags$label("Nome do Novo Reservatório:", class = "mevaz-label"),
           textInput("input_nomeac", NULL, value = "", width = "100%")
        ),
        uiOutput("step1_feedback"),
        actionButton("btn_step1_next", "Validar e Avançar →", class = "btn-primary rounded-pill w-100")
      ),

      # ===== ETAPA 2: Localizacao do ponto =====
      step_box("2", "Etapa 2 — Localização do novo açude",
        radioButtons("location_method", "Como deseja localizar o novo açude?",
          choices = c("Selecionar no mapa" = "map", "Informar coordenadas" = "coordinates"),
          selected = "map", width = "100%"),
        conditionalPanel(condition = "input.location_method == 'map'",
          div(class = "alert alert-info", "Clique no mapa no local desejado para o barramento. Amplie o zoom para facilitar a seleção.")),
        tags$small("O ponto é sempre encaixado no canal de drenagem (linhas azuis) mais próximo do local informado.",
                  class = "mevaz-hint", style = "display:block; text-align:center; margin-bottom:8px;"),
        conditionalPanel(condition = "input.location_method == 'coordinates'",
        tags$p("Informe latitude e longitude em graus decimais e clique em ‘Localizar ponto’. Use valores negativos para sul e oeste.", class = "mevaz-hint"),
        div(class = "mevaz-field",
           tags$label("Latitude:", class = "mevaz-label"),
           htmltools::tagQuery(numericInput("input_lat", NULL, value = NA, width = "100%"))$
             find("input")$addAttrs(placeholder = "Ex.: -5.123456", step = "any")$allTags()
        ),
        div(class = "mevaz-field",
           tags$label("Longitude:", class = "mevaz-label"),
           htmltools::tagQuery(numericInput("input_lon", NULL, value = NA, width = "100%"))$
             find("input")$addAttrs(placeholder = "Ex.: -39.123456", step = "any")$allTags()
        ),
        actionButton("submit_button", "Localizar ponto", class = "btn-outline-primary rounded-pill w-100 mb-2")
        ),
        uiOutput("location_summary"),
        half_button_row("btn_step2_back", "← Voltar", "btn_step2_next", "Avançar →")
      ),

      # ===== ETAPA 3: Obter a bacia =====
      step_box("3", "Etapa 3 — Bacia do novo açude",
        radioButtons("basin_method", "Como deseja obter a bacia?",
          choices = c("Traçar automaticamente" = "B", "Importar shapefile" = "A"), selected = "B"),
        conditionalPanel(condition = "input.basin_method == 'B'",
          tags$p("Trace a bacia a partir do ponto de barramento escolhido.", class = "mevaz-hint"),
          actionButton("run_r_script", "Traçar bacia", class = "btn-outline-primary rounded-pill w-100 mb-2")
        ),
        conditionalPanel(condition = "input.basin_method == 'A'",
          fileInput("upload_shapefile_a", "Arquivo da bacia",
            multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg"),
            buttonLabel = "Procurar...", placeholder = "Nenhum arquivo selecionado"),
          tags$p("Selecione os arquivos .shp, .shx, .dbf e .prj da mesma bacia.", class = "mevaz-hint"),
          actionButton("import_basin_button", "Importar e conferir bacia", class = "btn-outline-primary rounded-pill w-100 mb-2")
        ),
        uiOutput("basin_feedback"),
        uiOutput("basin_summary"),
        actionButton("reset_input_b", "Resetar mapa e dados", class = "btn-outline-danger rounded-pill w-100 mevaz-reset-btn"),
        half_button_row("btn_step3_back", "← Voltar", "btn_step3_next", "Avançar →")
      ),

      # ===== ETAPA 4: Modelo de regionalizacao =====
      step_box("4", "Etapa 4 — Modelo de regionalização",
        tags$p("Escolha como estimar as afluências do novo açude a partir das características da bacia.", class = "mevaz-hint"),
        radioButtons("input_mod_reg", "Modelo de regionalização:",
          choices = c("KNN — Similaridade entre bacias" = "KNN", "ML — Modelo linear" = "ML", "Multimodelo — Combinação de KNN\u00a0e\u00a0ML" = "Multimodelo"), selected = "KNN", width = "100%"),
        div(class = "mevaz-model-explanations",
          tags$p(tags$strong("KNN: "), "Transfere os parâmetros do modelo hidrológico da estação de referência com características fisiográficas mais semelhantes às da bacia."),
          tags$p(tags$strong("ML: "), "Estima os parâmetros do modelo hidrológico por relações lineares com as características da bacia e a precipitação média."),
          tags$p(tags$strong("Multimodelo: "), "Utiliza a média das séries de afluência estimadas por KNN e ML.")
        ),
        half_button_row("btn_step4_back", "← Voltar", "btn_step4_next", "Confirmar e avançar →")
      ),

      # ===== ETAPA 5: Preparacao da rede =====
      step_box("5", "Etapa 5 — Preparação da rede e dos dados",
        conditionalPanel(condition = "output.wizard_branch_js == 'B'",
          tags$p("Prepare a cascata e os dados dos reservatórios envolvidos usando a bacia traçada e o modelo escolhido.", class = "mevaz-hint"),
          actionButton("traca_cascata", "Preparar rede e dados", class = "btn-primary rounded-pill w-100 mb-2")
        ),
        conditionalPanel(condition = "output.wizard_branch_js == 'A'",
          tags$p("Informe o reservatório a jusante para preparar os dados da bacia importada.", class = "mevaz-hint"),
          numericInput("input_id_jusante", "Código do reservatório a jusante:", value = NA, width = "100%"),
          tags$p("Use 0 quando não houver reservatório a jusante.", class = "mevaz-hint"),
          actionButton("btn_gerar_dados", "Preparar rede e dados", class = "btn-primary rounded-pill w-100 mb-2")
        ),
        uiOutput("network_feedback"),
        half_button_row("btn_step5_back", "← Voltar", "btn_step5_next", "Avançar →")
      ),

      # ===== ETAPA 6: CAV =====
      step_box("6", "Etapa 6 — CAV e cota do sangradouro",
        tags$p("Selecione o arquivo da curva Cota-Área-Volume e informe a cota do sangradouro do novo reservatório.", style = "text-align:center;"),
        actionButton("cav_button", "Importar arquivo CAV", class = "btn-outline-primary rounded-pill w-100 mb-2"),
        half_button_row("btn_step6_back", "← Voltar", "btn_step6_next", "Avançar →")
      ),

      # ===== ETAPA 7: Simulacao =====
      step_box("7", "Etapa 7 — Simulação e resultados",
        actionButton("open_simula_button", "Executar Simulação MEVAZ", class = "btn-primary rounded-pill w-100 mb-2"),
        actionButton("open_results_button", "Resultados", class = "btn-outline-primary rounded-pill w-100 mb-2"),
        actionButton("btn_step7_back", "← Voltar", class = "btn-outline-primary rounded-pill w-100")
      ),

      uiOutput("wizard_requirements"),
      uiOutput("reservoir_sheet"),
      uiOutput("wizard_stage_status"),
      actionButton("reset_button", "Resetar tudo", class = "btn-outline-danger rounded-pill w-100 mevaz-reset-btn"),

      uiOutput("ponto_bloqueado_feedback")
    ),

    # ---- Carta (leaflet) ----
    tags$div(class = "mevaz-map", leaflet::leafletOutput("map", height = "100%"))
  )
)
