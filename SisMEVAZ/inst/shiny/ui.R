# =====================================================================
# MEVAZ - versao Shiny
# ui.R  -  Fase 2: wizard completo de 6 etapas + tema FUNCEME.
#          A reatividade real (validacoes, chamadas aos helpers R da Fase 1,
#          janelas de carregamento) entra na Fase 3. Aqui ha apenas a
#          navegacao minima entre etapas, para permitir testar o layout.
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
    tags$link(rel = "stylesheet", type = "text/css", href = "funceme_theme.css")
  ),

  # ---- Cabecalho institucional FUNCEME ----
  tags$div(
    class = "funceme-header",
    tags$div(class = "funceme-logo-badge", tags$img(src = "logo_funceme.png")),
    tags$div(
      class = "funceme-header-text",
      tags$p("MEVAZ", class = "funceme-header-title"),
      tags$p("Metodologia para o Cálculo de Vazões — Secretaria dos Recursos Hídricos",
             class = "funceme-header-subtitle")
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
         div(id = "wizard_progress_fill", class = "wizard-progress-fill", style = "width: 16.7%;")
      ),
      uiOutput("wizard_progress_label"),

      # ===== ETAPA 1: Dados basicos =====
      step_box("1", "Etapa 1 — Dados básicos do novo reservatório",
        div(class = "mevaz-field",
           tags$label("Código do Novo Reservatório:", class = "mevaz-label"),
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

      # ===== ETAPA 2: Escolha do metodo =====
      step_box("2", "Etapa 2 — Escolha o método",
        tags$p("Como deseja definir a bacia do novo reservatório?", style = "text-align:center;"),
        actionButton("btn_branch_a", "A — Importar meus shapefiles", class = "btn-outline-primary rounded-pill w-100 mb-2"),
        actionButton("btn_branch_b", "B — Traçar bacia e cascata automaticamente", class = "btn-outline-primary rounded-pill w-100 mb-2"),
        actionButton("btn_step2_back", "← Voltar", class = "btn-outline-secondary rounded-pill w-100")
      ),

      # ===== ETAPA 3: Localizacao do ponto =====
      step_box("3", "Etapa 3 — Localização do novo açude",
        tags$h6("Clique no mapa o local do novo açude ou digite as coordenadas e clique em 'Desenhe o Ponto'",
               style = "text-align:center;"),
        tags$small("O ponto é sempre encaixado no canal de drenagem (linhas azuis) mais próximo do clique.",
                  class = "mevaz-hint", style = "display:block; text-align:center; margin-bottom:8px;"),
        div(class = "mevaz-field",
           tags$label("Latitude:", class = "mevaz-label"),
           numericInput("input_lat", NULL, value = NA, width = "100%")
        ),
        div(class = "mevaz-field",
           tags$label("Longitude:", class = "mevaz-label"),
           numericInput("input_lon", NULL, value = NA, width = "100%")
        ),
        actionButton("submit_button", "Desenhe o Ponto", class = "btn-outline-primary rounded-pill w-100 mb-2"),
        half_button_row("btn_step3_back", "← Voltar", "btn_step3_next", "Avançar →")
      ),

      # ===== ETAPA 4: Bacia e dados (depende do metodo) =====
      step_box("4", "Etapa 4 — Bacia e dados do reservatório",
        div(class = "mevaz-field",
           tags$label("Modelo de Regionalização:", class = "mevaz-label"),
           selectInput("input_mod_reg", NULL,
                      choices = c("KNN", "ML", "Multimodelo"), selected = "KNN", width = "100%")
        ),

        # --- Metodo B: tracar automaticamente ---
        conditionalPanel(
          condition = "output.wizard_branch_js == 'B'",
          tags$p("Método B: traçado automático.", class = "mevaz-hint"),
          actionButton("run_r_script", "Trace a Bacia", class = "btn-outline-primary rounded-pill w-100 mb-2"),
          actionButton("traca_cascata", "Trace a Cascata", class = "btn-outline-primary rounded-pill w-100 mb-2"),
          actionButton("reset_input_b", "Resetar mapa e dados", class = "btn-outline-danger rounded-pill w-100 mevaz-reset-btn"),
          tags$small("Limpa o mapa e apaga os shapefiles e Dados_Novos_Reservatorios da pasta input.",
                    class = "mevaz-hint")
        ),

        # --- Metodo A: importar shapefiles ---
        conditionalPanel(
          condition = "output.wizard_branch_js == 'A'",
          tags$p("Método A: importe o shapefile da bacia do novo reservatório.", class = "mevaz-hint"),
          fileInput("upload_shapefile_a", "Importar shapefile da bacia",
                   multiple = TRUE, accept = c(".shp", ".shx", ".dbf", ".prj", ".cpg"),
                   buttonLabel = "Procurar...", placeholder = "Nenhum arquivo selecionado"),
          tags$small("Selecione todos os arquivos do shapefile (.shp, .shx, .dbf, .prj).",
                    class = "mevaz-hint"),
          div(class = "mevaz-field", style = "margin-top:10px;",
             tags$label("ID do reservatório a jusante:", class = "mevaz-label"),
             numericInput("input_id_jusante", NULL, value = NA, width = "100%")
          ),
          actionButton("btn_gerar_dados", "Gerar dados e avançar →", class = "btn-primary rounded-pill w-100"),
          uiOutput("step4a_feedback")
        ),

        tags$div(style = "margin-top:10px;"),
        half_button_row("btn_step4_back", "← Voltar", "btn_step4_next", "Avançar →")
      ),

      # ===== ETAPA 5: CAV =====
      step_box("5", "Etapa 5 — CAV (Cota, Área, Volume)",
        tags$p("Informe a curva Cota-Área-Volume e a Cota do Sangrador do novo reservatório.",
              style = "text-align:center;"),
        actionButton("cav_button", "Abrir CAV", class = "btn-outline-primary rounded-pill w-100 mb-2"),
        half_button_row("btn_step5_back", "← Voltar", "btn_step5_next", "Avançar →")
      ),

      # ===== ETAPA 6: Simulacao =====
      step_box("6", "Etapa 6 — Executar a simulação MEVAZ",
        actionButton("open_simula_button", "Executar Simulação MEVAZ", class = "btn-primary rounded-pill w-100 mb-2"),
        div(style = "display:flex; gap:8px;",
           actionButton("btn_step6_back", "← Voltar", class = "btn-outline-primary rounded-pill w-100"),
           actionButton("reset_button", "Resetar tudo", class = "btn-outline-danger rounded-pill w-100 mevaz-reset-btn")
        )
      ),

      # Painel de status sempre visivel (independente da etapa), equivalente
      # aos divs 'clicked-coordinates'/'nearest-point-info' do Python: mostra
      # o ponto selecionado e o resultado das ultimas acoes de tracado.
      uiOutput("point_feedback")
    ),

    # ---- Carta (leaflet) ----
    tags$div(class = "mevaz-map", leaflet::leafletOutput("map", height = "100%"))
  )
)
