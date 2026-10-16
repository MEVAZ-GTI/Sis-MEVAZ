# =====================================================================
# MEVAZ - versao Shiny
# server.R  -  Fase 3: reatividade completa, usando os helpers validados na
#              helpers em SisMEVAZ/inst/shiny/R/ e o layout de ui.R.
# =====================================================================

server <- function(input, output, session) {

  # =====================================================================
  # Estado do assistente (Fase 2)
  # =====================================================================
  wizard_step <- reactiveVal(1)
  wizard_branch <- reactiveVal("B")
  ponto_bloqueado <- reactiveVal(FALSE)
  etapas_concluidas <- reactiveVal(rep(FALSE, 7))
  bacia_tracada <- reactiveVal(FALSE)
  results_snapshot <- reactiveVal(NULL)
  confirmed_cav <- reactiveVal(NULL)
  observe({
    shinyjs::toggleState("open_results_button", condition = etapas_concluidas()[7])
  })

  marcar_etapas <- function(etapas, concluida) {
    estado <- isolate(etapas_concluidas())
    estado[etapas] <- concluida
    etapas_concluidas(estado)
  }

  # Alteracoes nas entradas tornam pendentes os resultados que dependem delas.
  observeEvent(list(input$input_cod, input$input_nomeac), {
    marcar_etapas(c(1, 3:7), FALSE)
    bacia_tracada(FALSE)
    basin_feedback_msg(NULL)
    network_feedback_msg(NULL)
  }, ignoreInit = TRUE, priority = 100)
  observeEvent(input$input_mod_reg, {
    marcar_etapas(4:7, FALSE)
    network_feedback_msg(NULL)
  }, ignoreInit = TRUE, priority = 100)
  observeEvent(input$input_id_jusante, {
    if (identical(wizard_branch(), "A")) {
      marcar_etapas(5:7, FALSE)
      network_feedback_msg(NULL)
    }
  }, ignoreInit = TRUE, priority = 100)
  observeEvent(input$upload_shapefile_a, {
    if (identical(wizard_branch(), "A")) {
      marcar_etapas(3:7, FALSE)
      bacia_tracada(FALSE)
      imported_basin(NULL)
      basin_feedback_msg(NULL)
      network_feedback_msg(NULL)
    }
  }, ignoreInit = TRUE, priority = 100)

  observe({
    shinyjs::toggleState("location_method", condition = !ponto_bloqueado())
    for (id in c("input_lat", "input_lon")) {
      shinyjs::toggleState(id, condition = !ponto_bloqueado() && etapas_concluidas()[1] &&
        wizard_step() == 2 && identical(input$location_method, "coordinates"))
    }
  })

  output$ponto_bloqueado_feedback <- renderUI({
    if (ponto_bloqueado() && wizard_step() %in% c(2, 3)) {
      div(class = "alert alert-info", role = "status",
          "Ponto de barramento escolhido. Para alterar a localização, use o botão de reset da interface.")
    }
  })

  output$wizard_step_js <- renderText(as.character(wizard_step()))
  outputOptions(output, "wizard_step_js", suspendWhenHidden = FALSE)

  output$wizard_branch_js <- renderText(if (is.null(wizard_branch())) "" else wizard_branch())
  outputOptions(output, "wizard_branch_js", suspendWhenHidden = FALSE)

  step_labels <- c("1" = "Identificação", "2" = "Localização", "3" = "Bacia",
                   "4" = "Regionalização", "5" = "Rede e dados", "6" = "CAV", "7" = "Simulação")
  output$wizard_stage_status <- renderUI({
    concluidas <- etapas_concluidas()
    tags$div(class = "wizard-status", role = "region", `aria-label` = "Andamento das etapas",
      tags$p(sprintf("%d de 7 etapas concluídas", sum(concluidas)), class = "wizard-status-summary"),
      tags$ol(class = "wizard-status-list", lapply(seq_len(7), function(i) {
        status <- if (concluidas[i]) "Concluída" else "Pendente"
        tags$li(class = paste("wizard-status-item", if (concluidas[i]) "is-complete" else "is-pending",
                             if (wizard_step() == i) "is-current" else ""),
          `aria-current` = if (wizard_step() == i) "step" else NULL,
          tags$span(class = "wizard-status-number", if (concluidas[i]) "✓" else as.character(i), `aria-hidden` = "true"),
          tags$div(class = "wizard-status-content",
            tags$div(class = "wizard-status-heading", tags$span(step_labels[[as.character(i)]]),
                     tags$span(class = "wizard-status-badge", status))
          )
        )
      }))
    )
  })
  output$wizard_progress_label <- renderUI({
    s <- wizard_step()
    tags$div(
      sprintf("Etapa %d/7 — %s", s, step_labels[[as.character(s)]]),
      style = sprintf(
        "text-align:center; font-weight:700; margin-bottom:12px; color:%s; text-transform:uppercase; letter-spacing:0.5px; font-size:13px;",
        FUNCEME_ORANGE
      )
    )
  })

  observe({
    pct <- sum(etapas_concluidas()) / 7 * 100
    shinyjs::runjs(sprintf("document.getElementById('wizard_progress_fill').style.width = '%.1f%%';", pct))
  })

  observe({
    shinyjs::toggleState("basin_method", condition = !ponto_bloqueado())
  })
  observeEvent(input$basin_method, {
    if (!input$basin_method %in% c("A", "B")) return()
    if (ponto_bloqueado()) {
      updateRadioButtons(session, "basin_method", selected = wizard_branch())
      return()
    }
    if (!identical(wizard_branch(), input$basin_method)) {
      marcar_etapas(3:7, FALSE)
      bacia_tracada(FALSE)
      imported_basin(NULL)
      basin_feedback_msg(NULL)
      network_feedback_msg(NULL)
    }
    wizard_branch(input$basin_method)
  })
  observeEvent(input$btn_step2_back, wizard_step(1))
  observeEvent(input$btn_step3_back, wizard_step(2))
  observeEvent(input$btn_step2_next, {
    if (permitir_acao("btn_step2_next")) wizard_step(3)
  })
  observeEvent(input$btn_step3_next, {
    if (permitir_acao("btn_step3_next")) wizard_step(4)
  })
  observeEvent(input$btn_step4_back, wizard_step(3))
  observeEvent(input$btn_step4_next, {
    if (!permitir_acao("btn_step4_next")) return()
    marcar_etapas(4, TRUE)
    wizard_step(5)
  })
  observeEvent(input$btn_step5_back, wizard_step(4))
  observeEvent(input$btn_step5_next, {
    if (permitir_acao("btn_step5_next")) wizard_step(6)
  })
  observeEvent(input$btn_step6_back, wizard_step(5))
  observeEvent(input$btn_step6_next, {
    if (permitir_acao("btn_step6_next")) wizard_step(7)
  })
  observeEvent(input$btn_step7_back, wizard_step(6))

  # =====================================================================
  # Estado de dados do reservatorio (equivalente ao analysis-results-store
  # e as variaveis globais tabela/geo_df do Python)
  # =====================================================================
  analysis_results <- reactiveValues(
    station_code = NULL, basin_name = NULL, drainage_name = NULL,
    selected_point = NULL,  # list(lon=, lat=)
    snap_distance = NULL,
    containing_sub_basin_area_sq_km = NULL, intersection_area_sq_km = NULL
  )
  observeEvent(wizard_step(), {
    proxy <- leaflet::leafletProxy("map")
    if (wizard_step() == 2) {
      leaflet::showGroup(proxy, "original_point")
    } else {
      leaflet::hideGroup(proxy, "original_point")
    }
  })
  imported_basin <- reactiveVal(NULL)
  basin_feedback_msg <- reactiveVal(NULL)
  network_feedback_msg <- reactiveVal(NULL)
  output$basin_feedback <- renderUI(if (!etapas_concluidas()[3]) basin_feedback_msg())
  output$location_summary <- renderUI({
    req(etapas_concluidas()[2])
    div(class = "mevaz-step-summary", role = "status",
      tags$p("Localização selecionada", class = "mevaz-summary-title"),
      tags$dl(
        tags$dt("Rio/curso d’água:"), tags$dd(summary_name(analysis_results$drainage_name)),
        tags$dt("Região hidrográfica:"), tags$dd(summary_name(analysis_results$basin_name))
      ),
      if (length(analysis_results$snap_distance) == 1L && analysis_results$snap_distance > 1000) {
        tags$p(sprintf("Atenção: o ponto foi encaixado a %.0f m do local informado. Confira sua localização no mapa.", analysis_results$snap_distance), class = "mevaz-summary-warning")
      }
    )
  })
  output$basin_summary <- renderUI({
    req(etapas_concluidas()[3], bacia_tracada())
    div(class = "mevaz-step-summary", role = "status",
      tags$p("Bacia obtida", class = "mevaz-summary-title"),
      tags$dl(
        tags$dt("Área total:"), tags$dd(summary_area(analysis_results$containing_sub_basin_area_sq_km)),
        tags$dt("Área incremental:"), tags$dd(summary_area(analysis_results$intersection_area_sq_km))
      ),
      if (identical(wizard_branch(), "A")) tags$p("Na importação, o resumo apresenta a área do shapefile; a área incremental não é calculada nesta etapa.", class = "mevaz-summary-note")
    )
  })
  output$network_feedback <- renderUI(if (!etapas_concluidas()[5]) network_feedback_msg())
  output$reservoir_sheet <- renderUI({
    req(wizard_step() %in% 5:7)
    confirmed <- etapas_concluidas()
    rows <- list()
    add_row <- function(label, value) {
      rows[[length(rows) + 1L]] <<- tags$tr(tags$th(label, scope = "row"), tags$td(value))
    }
    if (confirmed[1]) {
      add_row("Nome", input$input_nomeac)
      add_row("Código", as.character(input$input_cod))
    }
    if (all(confirmed[1:2])) {
      add_row("Rio/curso d’água", summary_name(analysis_results$drainage_name))
      add_row("Região hidrográfica", summary_name(analysis_results$basin_name))
    }
    if (all(confirmed[1:3])) {
      add_row("Área total da bacia", summary_area(analysis_results$containing_sub_basin_area_sq_km))
      add_row("Área incremental", summary_area(analysis_results$intersection_area_sq_km))
    }
    if (all(confirmed[1:4])) add_row("Regionalização", input$input_mod_reg)
    add_row("Rede e dados", if (all(confirmed[1:5])) "Preparados" else "Preparação pendente")
    cav <- confirmed_cav()
    if (all(confirmed[1:6]) && !is.null(cav)) {
      add_row("Cota do sangradouro", paste0(results_number(cav$cota, 3), " m"))
      add_row("Capacidade do açude", paste0(results_number(cav$capacity_hm3, 3), " hm³"))
    }
    if (all(confirmed)) {
      add_row("Simulação", "Concluída")
      current <- tryCatch(read_results_snapshot(REDE_RES_TEST), error = function(e) NULL)
      if (!is.null(current)) {
        reservoir <- current$general[current$general$ID == as.character(input$input_cod), , drop = FALSE]
        if (nrow(reservoir) == 1L) {
          for (q in c("Q90", "Q95", "Q98")) add_row(q, paste0(results_number(reservoir[[q]]), " m³/s"))
        }
      }
    } else if (wizard_step() == 7) {
      add_row("Simulação", if (all(confirmed[1:6])) "Pronta para executar" else "Aguardando dados confirmados")
    }
    div(class = "mevaz-step-summary mevaz-reservoir-sheet", role = "region", `aria-label` = "Ficha do reservatório",
      tags$p("Ficha do reservatório", class = "mevaz-summary-title"),
      tags$table(tags$tbody(rows))
    )
  })
  session$onSessionEnded(function() {
    for (path in import_directories) unlink(path, recursive = TRUE)
  })
  import_directories <- character()
  tabela_cas_state <- reactiveVal(NULL)  # Tabela_atributos_acudes_Nova (data.frame)
  geo_sf_state <- reactiveVal(NULL)      # new_exut_acudes (sf)
  # TRUE quando o ponto nao esta dentro de nenhuma sub-bacia consolidada, ou
  # seja, o novo reservatorio nao tem jusante (ID_ACJUS = 0). Nesse caso a
  # cascata e legitimamente vazia e o Dados_Novos so tem o novo reservatorio.
  standalone_state <- reactiveVal(FALSE)
  cav_data <- reactiveVal(data.frame(COTA = numeric(0), AREA_KM2 = numeric(0), VOLUME_M3 = numeric(0)))
  observeEvent(list(cav_data(), input$input_cota_sang), {
    marcar_etapas(6:7, FALSE)
  }, ignoreInit = TRUE, priority = 100)
  observeEvent(list(input$input_lat, input$input_lon), {
    if (!ponto_bloqueado() && identical(input$location_method, "coordinates")) marcar_etapas(2:7, FALSE)
  }, ignoreInit = TRUE, priority = 100)

  inteiro_valido <- function(valor) {
    is.numeric(valor) && length(valor) == 1 && is.finite(valor) && abs(valor) <= .Machine$integer.max && valor == trunc(valor)
  }
  coordenadas_validas <- function() {
    lat <- input$input_lat; lon <- input$input_lon
    is.numeric(lat) && length(lat) == 1 && is.finite(lat) && abs(lat) <= 90 &&
      is.numeric(lon) && length(lon) == 1 && is.finite(lon) && abs(lon) <= 180
  }

  # Uma unica regra por acao controla tanto o botao quanto a validacao no servidor.
  requisitos_acoes <- reactive({
    e <- etapas_concluidas()
    cadastro <- if (!inteiro_valido(input$input_cod)) "Informe um código válido." else
      if (input$input_cod %in% EXISTING_RESERVOIR_IDS) "Escolha um código que não exista na rede consolidada." else
      if (is.null(input$input_nomeac) || !nzchar(trimws(input$input_nomeac))) "Informe o nome do reservatório." else NULL
    localizacao <- if (!e[1]) "Valide a identificação do novo açude na etapa 1." else NULL
    metodo <- if (!is.null(localizacao)) localizacao else
      if (!e[2]) "Escolha o ponto de barramento no mapa ou desenhe o ponto pelas coordenadas." else NULL
    automatico <- if (!is.null(metodo)) metodo else
      if (!identical(wizard_branch(), "B")) "Escolha o traçado automático na etapa 3." else NULL
    importacao <- if (!is.null(metodo)) metodo else
      if (!identical(wizard_branch(), "A")) "Escolha a importação de shapefile na etapa 3." else NULL
    if (is.null(importacao)) {
      upload <- input$upload_shapefile_a
      nomes <- if (is.null(upload)) character() else tolower(upload$name)
      bases <- tools::file_path_sans_ext(nomes[grepl("\\.shp$", nomes)])
      completo <- length(bases) == 1 && all(paste0(bases, c(".shp", ".shx", ".dbf", ".prj")) %in% nomes)
      if (!completo) importacao <- "Selecione os arquivos .shp, .shx, .dbf e .prj de uma única bacia."
    }
    bacia <- if (!is.null(metodo)) metodo else
      if (!e[3] || !bacia_tracada()) "Trace ou importe e confira a bacia na etapa 3." else NULL
    modelo <- if (!is.null(bacia)) bacia else
      if (is.null(input$input_mod_reg) || !input$input_mod_reg %in% c("KNN", "ML", "Multimodelo")) "Escolha um modelo de regionalização na etapa 4." else NULL
    preparacao <- if (!is.null(modelo)) modelo else
      if (!e[4]) "Confirme o modelo de regionalização na etapa 4." else NULL
    cascata <- if (!is.null(preparacao)) preparacao else
      if (!identical(wizard_branch(), "B")) "Escolha o traçado automático na etapa 3." else NULL
    gerar_importacao <- if (!is.null(preparacao)) preparacao else
      if (!identical(wizard_branch(), "A") || is.null(imported_basin())) "Importe e confira a bacia na etapa 3." else
      if (!inteiro_valido(input$input_id_jusante) || !input$input_id_jusante %in% c(0, EXISTING_RESERVOIR_IDS)) {
        "Informe um código a jusante da rede consolidada, ou 0 quando não houver reservatório a jusante."
      } else NULL
    dados <- if (!is.null(preparacao)) preparacao else
      if (!e[5]) "Prepare a rede e os dados na etapa 5 antes de continuar." else NULL
    salvar <- dados
    if (is.null(salvar)) {
      df <- cav_data()
      salvar <- if (!nrow(df)) "Selecione um arquivo contendo os dados de Cota, Área e Volume." else
        tryCatch({ ensure_spillway_cota_in_cav(df, input$input_cota_sang); NULL },
                 error = function(e) conditionMessage(e))
    }
    simulacao <- if (!is.null(dados)) dados else
      if (!e[6]) "Confirme e salve o CAV com a cota do sangrador antes de continuar." else NULL
    list(validate_codigo_button = if (!inteiro_valido(input$input_cod)) "Informe um código válido." else NULL,
         btn_step1_next = cadastro,
         submit_button = if (!is.null(localizacao)) localizacao else
           if (wizard_step() != 2 || !identical(input$location_method, "coordinates")) "Escolha ‘Informar coordenadas’ na etapa 2 para localizar o ponto." else
           if (ponto_bloqueado()) "Use o reset para alterar o ponto de barramento." else
             if (!coordenadas_validas()) "Informe latitude e longitude válidas para desenhar o ponto." else NULL,
         btn_step2_next = metodo, btn_step3_next = bacia, import_basin_button = importacao,
         run_r_script = automatico, traca_cascata = cascata, btn_gerar_dados = gerar_importacao,
         btn_step4_next = modelo, btn_step5_next = dados, cav_button = dados, confirm_save_button = salvar,
         ver_grafico_button = if (!is.null(dados)) dados else
           if (!nrow(cav_data())) "Selecione um arquivo CAV para visualizar o gráfico." else NULL,
         btn_step6_next = simulacao, open_simula_button = simulacao)
  })

  permitir_acao <- function(id) {
    falta <- requisitos_acoes()[[id]]
    if (is.null(falta)) return(TRUE)
    showNotification(falta, type = "warning", duration = 8)
    FALSE
  }
  observe({
    regras <- requisitos_acoes()
    for (id in names(regras)) shinyjs::toggleState(id, condition = is.null(regras[[id]]))
  })
  output$wizard_requirements <- renderUI({
    regras <- requisitos_acoes()
    ids <- switch(as.character(wizard_step()),
      "1" = "btn_step1_next", "2" = c(if (identical(input$location_method, "coordinates")) "submit_button", "btn_step2_next"),
      "3" = c(if (identical(wizard_branch(), "B")) "run_r_script" else "import_basin_button", "btn_step3_next"),
      "4" = "btn_step4_next",
      "5" = c(if (identical(wizard_branch(), "B")) "traca_cascata" else "btn_gerar_dados", "btn_step5_next"),
      "6" = c("cav_button", "btn_step6_next"), "7" = "open_simula_button")
    mensagens <- unique(unlist(regras[ids], use.names = FALSE))
    if (wizard_step() == 2 && (etapas_concluidas()[2] || ponto_bloqueado())) mensagens <- regras$btn_step2_next
    if (length(mensagens)) div(class = "alert alert-info", role = "status",
                              tags$strong("Para continuar:"), tags$ul(lapply(mensagens, tags$li)))
  })
  output$cav_requirements <- renderUI({
    falta <- requisitos_acoes()$confirm_save_button
    if (!is.null(falta)) div(class = "alert alert-info", role = "status", falta)
  })
  outputOptions(output, "cav_requirements", suspendWhenHidden = FALSE)

  # Painel de status sempre visivel (equivalente ao 'nearest-point-info' do
  # Python): mostra o ponto usado na selecao e o resultado do ultimo tracado.
  point_feedback_msg <- reactiveVal(NULL)
  output$point_feedback <- renderUI(point_feedback_msg())
  outputOptions(output, "point_feedback", suspendWhenHidden = FALSE)

  status_div <- function(text, color = "#333") {
    div(style = sprintf("text-align:center; font-weight:700; color:%s; margin-top:6px;", color), text)
  }

  reset_reservoir_state <- function() {
    marcar_etapas(2:7, FALSE)
    bacia_tracada(FALSE)
    imported_basin(NULL)
    basin_feedback_msg(NULL)
    network_feedback_msg(NULL)
    ponto_bloqueado(FALSE)
    updateRadioButtons(session, "location_method", selected = "map")
    tabela_cas_state(NULL)
    geo_sf_state(NULL)
    standalone_state(FALSE)
    cav_data(data.frame(COTA = numeric(0), AREA_KM2 = numeric(0), VOLUME_M3 = numeric(0)))
    confirmed_cav(NULL)
    analysis_results$station_code <- NULL
    analysis_results$basin_name <- NULL
    analysis_results$drainage_name <- NULL
    analysis_results$selected_point <- NULL
    analysis_results$snap_distance <- NULL
    analysis_results$containing_sub_basin_area_sq_km <- NULL
    analysis_results$intersection_area_sq_km <- NULL
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("point_markers") |>
      leaflet::clearGroup("original_point") |>
      leaflet::clearGroup("bacia") |>
      leaflet::clearGroup("cascata")
  }

  # =====================================================================
  # Etapa 1 — Dados basicos
  # =====================================================================
  observeEvent(input$validate_codigo_button, {
    if (!permitir_acao("validate_codigo_button")) return()
    cod <- input$input_cod
    output$codigo_validation_feedback <- renderUI({
      if (is.null(cod) || is.na(cod)) {
        div(class = "alert alert-warning", "Informe um código para validar.")
      } else {
        cod_int <- suppressWarnings(as.integer(cod))
        if (is.na(cod_int)) {
          div(class = "alert alert-danger", "Código inválido. Use apenas números inteiros.")
        } else if (cod_int %in% EXISTING_RESERVOIR_IDS) {
          div(class = "alert alert-danger",
             sprintf("O código %d já existe na rede consolidada. Escolha outro código.", cod_int))
        } else {
          div(class = "alert alert-success", sprintf("Código %d disponível para uso.", cod_int))
        }
      }
    })
  })

  observeEvent(input$btn_step1_next, {
    if (!permitir_acao("btn_step1_next")) return()
    cod <- input$input_cod
    nome <- input$input_nomeac
    show_err <- function(msg) output$step1_feedback <- renderUI(div(class = "alert alert-warning", msg))

    if (is.null(cod) || is.na(cod)) return(show_err("Informe o código do reservatório."))
    cod_int <- suppressWarnings(as.integer(cod))
    if (is.na(cod_int)) return(show_err("Código inválido. Use apenas números inteiros."))
    if (cod_int %in% EXISTING_RESERVOIR_IDS) {
      return(show_err(sprintf("O código %d já existe na rede consolidada. Escolha outro.", cod_int)))
    }
    if (is.null(nome) || trimws(nome) == "") return(show_err("Informe o nome do reservatório."))

    output$step1_feedback <- renderUI(NULL)
    marcar_etapas(1, TRUE)
    wizard_step(2)
  })

  # =====================================================================
  # Etapa 2 — Localizacao do ponto
  # =====================================================================
  run_point_analysis <- function(lon, lat) {
    res <- comprehensive_point_analysis(lon, lat)
    analysis_results$station_code <- res$station_code
    analysis_results$basin_name <- res$basin_name
    analysis_results$drainage_name <- res$drainage_name
    analysis_results$selected_point <- list(lon = lon, lat = lat)
    marcar_etapas(2, TRUE)
    marcar_etapas(3:7, FALSE)
    bacia_tracada(FALSE)
    res
  }

  # snap_and_show: logica compartilhada entre "Desenhe o Ponto" (coordenadas
  # digitadas) e o clique direto no mapa. Encaixa o ponto bruto no canal mais
  # proximo, roda a analise, e desenha na carta: ponto bruto (azul), ponto
  # encaixado (vermelho) e uma LINHA TRACEJADA ligando os dois - assim o
  # encaixe fica sempre explicito (o usuario ve exatamente para onde o ponto
  # foi movido e a que distancia), em vez de precisar adivinhar a partir de um
  # segundo reticulado de rios sobreposto ao fundo. Avisa se o clique caiu
  # longe de qualquer canal (encaixe > 1 km sugere clique fora da drenagem).
  snap_and_show <- function(raw_lon, raw_lat, raw_label) {
    np <- find_nearest_point(raw_lon, raw_lat)
    snap_lon <- unname(np["lon"]); snap_lat <- unname(np["lat"])
    snap_dist <- unname(np["distance"])
    run_point_analysis(snap_lon, snap_lat)
    analysis_results$snap_distance <- snap_dist

    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("point_markers") |>
      leaflet::clearGroup("original_point") |>
      leaflet::addPolylines(lng = c(raw_lon, snap_lon), lat = c(raw_lat, snap_lat),
                            color = "#FF0000", weight = 2, opacity = 0.9,
                            dashArray = "4,6", group = "original_point") |>
      leaflet::addCircleMarkers(lng = raw_lon, lat = raw_lat, radius = 6, color = "#0000FF",
                                fillColor = "#0000FF", fillOpacity = 1, weight = 1,
                                group = "original_point") |>
      leaflet::addCircleMarkers(lng = snap_lon, lat = snap_lat, radius = 7, color = "#FF0000",
                                fillColor = "#FF0000", fillOpacity = 1, weight = 1,
                                group = "point_markers") |>
      leaflet::setView(lng = raw_lon, lat = raw_lat, zoom = 13)

    far <- snap_dist > 1000
    if (far) {
      showNotification(sprintf(
        "Atenção: o ponto ficou a %.0f m do canal mais próximo. Amplie o zoom e clique sobre uma linha azul da rede de drenagem.",
        snap_dist), type = "warning", duration = 9)
    }
    point_feedback_msg(tagList(
      status_div(sprintf("%s: Latitude: %.6f, Longitude: %.6f", raw_label, raw_lat, raw_lon), "#0000FF"),
      status_div(sprintf("Encaixado no canal (a %.0f m): Latitude: %.6f, Longitude: %.6f",
                         snap_dist, snap_lat, snap_lon), if (far) "#B8860B" else "#FF0000")
    ))
    np
  }

  observeEvent(input$submit_button, {
    if (!permitir_acao("submit_button")) return()
    if (ponto_bloqueado()) return()
    lat_v <- input$input_lat; lon_v <- input$input_lon
    if (is.null(lat_v) || is.null(lon_v) || is.na(lat_v) || is.na(lon_v)) {
      showNotification("Favor digitar os valores de latitude e longitude.", type = "warning")
      return()
    }
    withProgress(message = "Selecionando o ponto...", {
      snap_and_show(lon_v, lat_v, "Ponto digitado")
    })
  })

  observeEvent(input$map_click, {
    if (ponto_bloqueado() || !etapas_concluidas()[1] || wizard_step() != 2 ||
        !identical(input$location_method, "map")) return()
    click <- input$map_click
    req(click)
    withProgress(message = "Selecionando o ponto...", {
      # Encaixa (snap) o clique no canal de drenagem mais proximo - o clique
      # bruto quase nunca cai exatamente sobre um canal, e um ponto fora da
      # drenagem gera uma bacia degenerada no tracado. Mesmo encaixe usado
      # em 'Desenhe o Ponto' (find_nearest_point), aplicado tambem ao clique
      # direto no mapa (o Python original nao fazia isso).
      snap_and_show(click$lng, click$lat, "Ponto clicado")
    })
  })

  # =====================================================================
  # Etapas 3 e 5 (B) — tracar bacia, depois preparar a rede
  # =====================================================================
  observeEvent(input$run_r_script, {
    if (!permitir_acao("run_r_script")) return()
    cod_value <- input$input_cod
    nomeac_value <- input$input_nomeac
    cod_int <- suppressWarnings(as.integer(cod_value))
    if (is.na(cod_int) || cod_int %in% EXISTING_RESERVOIR_IDS) {
      showNotification("Código do reservatório inválido ou já existente. Volte à etapa 1.", type = "error")
      return()
    }

    sel <- analysis_results$selected_point
    if (is.null(sel)) {
      lat_v <- input$input_lat; lon_v <- input$input_lon
      if (is.null(lat_v) || is.null(lon_v) || is.na(lat_v) || is.na(lon_v)) {
        showNotification("Selecione um ponto no mapa (ou informe latitude/longitude) antes de traçar a bacia.",
                         type = "error", duration = 8)
        return()
      }
      sel <- list(lon = lon_v, lat = lat_v)
    }

    withProgress(message = "Traçando a bacia...", {
      result <- tryCatch({
        marcar_etapas(3:7, FALSE)
        bacia_tracada(FALSE)
        basin_feedback_msg(NULL)
        network_feedback_msg(NULL)
        clear_previous_simulation_data()
        bacia_sf <- trace_bacia_direct(sel$lon, sel$lat)
        ov <- select_containing_and_overlay(bacia_sf, sel$lon, sel$lat)
        # ov nunca e NULL: quando nenhuma sub-bacia consolidada contem o ponto,
        # devolve uma estrutura standalone (novo reservatorio sem jusante),
        # em vez de erro - ver bacia_helpers.R.
        pr <- process_bacia_trace(ov, cod_int, nomeac_value, sel$lon, sel$lat,
                                  TABELA_ATRIBUTOS_BASE, EXUT_ACUDES_SF, sf::st_crs(bacia_sf),
                                  regiao_fallback = analysis_results$basin_name,
                                  rio_barrado_fallback = analysis_results$drainage_name)
        list(pr = pr, bacia_sf = bacia_sf, intersection_geom = ov$intersection_geom,
             standalone = isTRUE(ov$standalone))
      }, error = function(e) e)

      if (inherits(result, "error")) {
        showNotification(paste("Erro ao traçar a bacia:", conditionMessage(result)), type = "error", duration = 10)
        point_feedback_msg(tagList(
          status_div(sprintf("Ponto usado: Latitude: %.6f, Longitude: %.6f", sel$lat, sel$lon), "#333"),
          status_div(paste("Erro ao traçar a bacia:", conditionMessage(result)), "#FF0000")
        ))
        return()
      }

      tabela_cas_state(result$pr$tabela)
      geo_sf_state(result$pr$geo_sf)
      standalone_state(isTRUE(result$standalone))
      ponto_bloqueado(TRUE)
      bacia_tracada(TRUE)
      marcar_etapas(3, TRUE)
      basin_feedback_msg(div(class = "alert alert-success", "Bacia traçada. Avance para escolher o modelo de regionalização."))
      analysis_results$containing_sub_basin_area_sq_km <- result$pr$bacia_area
      analysis_results$intersection_area_sq_km <- result$pr$intersection_area

      # Desenha a bacia tracada (verde) e, por cima, a bacia INCREMENTAL em
      # amarelo fluorescente (intersecao da bacia com o sub-bacia que contem o
      # ponto) - identico ao Python: bacia rgba(0,255,0,0.2) + intersecao
      # rgba(255,255,0,0.2). E a area que o novo reservatorio "recorta" do
      # sub-bacia a jusante.
      bacia_ll <- sf::st_transform(result$bacia_sf, 4326)
      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup("bacia") |>
        leaflet::addPolygons(data = bacia_ll, color = "green", weight = 2,
                             fillOpacity = 0.2, fillColor = "green", group = "bacia")

      # Desenha sempre a bacia incremental em amarelo - tambem no caso
      # standalone, onde ela e a bacia tracada MENOS as sub-bacias dos
      # reservatorios a montante contidas nela (so coincide com o verde
      # quando nao ha nenhum montante dentro da bacia).
      inter_geom <- result$intersection_geom
      inter_ll <- tryCatch({
        if (is.null(inter_geom) ||
            length(inter_geom) == 0 || all(sf::st_is_empty(inter_geom))) {
          NULL
        } else {
          poly <- suppressWarnings(sf::st_collection_extract(inter_geom, "POLYGON"))
          sf::st_transform(poly, 4326)
        }
      }, error = function(e) NULL)

      if (!is.null(inter_ll) && length(inter_ll) > 0) {
        proxy <- proxy |>
          leaflet::addPolygons(data = inter_ll, color = "#CCCC00", weight = 2,
                               fillOpacity = 0.45, fillColor = "#FFFF00", group = "bacia")
      }

      if (isTRUE(result$standalone)) {
        showNotification(
          "Bacia traçada com sucesso! O novo reservatório não tem reservatório a jusante na rede consolidada.",
          type = "message", duration = 9)
        point_feedback_msg(tagList(
          status_div("Bacia traçada com SUCESSO! Sem reservatório a jusante (ID_ACJUS = 0).", "#0000FF"),
          status_div(sprintf("Bacia total: %.2f km² | incremental (amarelo, descontados os montantes): %.2f km²",
                             result$pr$bacia_area, result$pr$intersection_area), "#B8860B")
        ))
      } else {
        showNotification("Bacia traçada com sucesso!", type = "message")
        point_feedback_msg(status_div("Bacia traçada com SUCESSO! (verde = bacia, amarelo = bacia incremental)", "#0000FF"))
      }
    })
  })

  observeEvent(input$traca_cascata, {
    if (!permitir_acao("traca_cascata")) return()
    cod_value <- suppressWarnings(as.integer(input$input_cod))
    nomeac_value <- input$input_nomeac
    mod_reg_value <- input$input_mod_reg

    tabela_cas <- tabela_cas_state()
    if (is.null(tabela_cas)) {
      showNotification("Trace a Bacia antes de traçar a cascata.", type = "error")
      return()
    }

    withProgress(message = "Traçando a cascata...", {
      marcar_etapas(5:7, FALSE)
      network_feedback_msg(NULL)
      result <- tryCatch({
        reservoir_table <- load_consolidated_reservoirs()
        analysis <- list(
          station_code = analysis_results$station_code,
          basin_name = analysis_results$basin_name,
          drainage_name = analysis_results$drainage_name,
          containing_sub_basin_area_sq_km = analysis_results$containing_sub_basin_area_sq_km,
          intersection_area_sq_km = analysis_results$intersection_area_sq_km
        )
        sel <- analysis_results$selected_point
        build_dados_novos(cod_value, nomeac_value, NA, mod_reg_value,
                          sel$lon, sel$lat, tabela_cas, reservoir_table, analysis,
                          standalone = isTRUE(standalone_state()))
      }, error = function(e) e)

      if (inherits(result, "error")) {
        network_feedback_msg(div(class = "alert alert-danger", conditionMessage(result)))
        showNotification(paste("Erro ao traçar a cascata:", conditionMessage(result)), type = "error", duration = 10)
        point_feedback_msg(status_div(paste("Erro ao traçar a cascata:", conditionMessage(result)), "#FF0000"))
        return()
      }

      network_feedback_msg(div(class = "alert alert-success", "Rede e dados preparados. Avance para informar a CAV."))
      casc <- result$cascata
      marcar_etapas(5, TRUE)
      proxy <- leaflet::leafletProxy("map") |> leaflet::clearGroup("cascata")
      if (nrow(casc) > 0) {
        for (i in seq_len(nrow(casc))) {
          proxy <- proxy |>
            leaflet::addPolylines(lng = c(casc$lonfrom[i], casc$lonto[i]),
                                  lat = c(casc$latfrom[i], casc$latto[i]),
                                  color = "red", weight = 2, group = "cascata")
        }
      }

      if (nrow(casc) == 0) {
        showNotification(
          "Dados gerados. Sem cascata a traçar: o reservatório não tem jusante nem montante na rede consolidada.",
          type = "message", duration = 9)
        point_feedback_msg(status_div(
          "Dados gerados com SUCESSO! Sem cascata (reservatório isolado, ID_ACJUS = 0).", "#0000FF"))
      } else {
        showNotification("Cascata traçada com sucesso!", type = "message")
        point_feedback_msg(status_div("Cascata traçada com SUCESSO!", "#0000FF"))
      }
    })
  })

  observeEvent(input$reset_input_b, {
    clear_previous_simulation_data()
    reset_reservoir_state()
    wizard_step(2)
    showNotification("Mapa e dados de teste limpos.", type = "message")
    point_feedback_msg(status_div("Mapa restaurado e dados de teste (shapefiles e Dados_Novos_Reservatorios) apagados.", "#333"))
  })

  # =====================================================================
  # Etapas 3 e 5 (A) — importar bacia, depois preparar os dados
  # =====================================================================
  observeEvent(input$import_basin_button, {
    if (!permitir_acao("import_basin_button")) return()
    marcar_etapas(3:7, FALSE)
    bacia_tracada(FALSE)
    imported_basin(NULL)
    upload <- input$upload_shapefile_a
    result <- tryCatch({
      imported <- prepare_uploaded_basin(upload)
      import_directories <<- c(import_directories, imported$directory)
      imported
    }, error = function(e) e)
    if (inherits(result, "error")) {
      basin_feedback_msg(div(class = "alert alert-danger", conditionMessage(result)))
      return()
    }
    imported_basin(result)
    analysis_results$containing_sub_basin_area_sq_km <- result$area_km2
    analysis_results$intersection_area_sq_km <- NULL
    bacia_tracada(TRUE)
    marcar_etapas(3, TRUE)
    ponto_bloqueado(TRUE)
    basin_feedback_msg(div(class = "alert alert-success", "Bacia importada e conferida. Avance para escolher o modelo de regionalização."))
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("bacia") |>
      leaflet::addPolygons(data = result$geometry, color = "green", weight = 2,
        fillOpacity = 0.2, fillColor = "green", group = "bacia")
  })

  observeEvent(input$btn_gerar_dados, {
    if (!permitir_acao("btn_gerar_dados")) return()
    imported <- imported_basin()
    analysis <- list(station_code = analysis_results$station_code,
      basin_name = analysis_results$basin_name, drainage_name = analysis_results$drainage_name,
      selected_point = analysis_results$selected_point)
    withProgress(message = "Preparando a rede e os dados...", {
      marcar_etapas(5:7, FALSE)
      clear_previous_simulation_data()
      result <- tryCatch(generate_dados_novos_option_a(imported$path,
        input$input_cod, input$input_nomeac, input$input_mod_reg, input$input_id_jusante,
        analysis, input$input_lat, input$input_lon), error = function(e) e)
    })
    if (inherits(result, "error")) {
      network_feedback_msg(div(class = "alert alert-danger", conditionMessage(result)))
      return()
    }
    marcar_etapas(5, TRUE)
    network_feedback_msg(div(class = "alert alert-success", "Rede e dados preparados. Avance para informar a CAV."))
  })

  # =====================================================================
  # Etapa 6 — CAV
  # =====================================================================
  observeEvent(input$cav_button, {
    if (permitir_acao("cav_button")) showModal(cav_modal_ui())
  })

  output$cav_table <- DT::renderDT({
    DT::datatable(
      cav_data(), editable = FALSE, rownames = FALSE,
      colnames = c("Cota (m)" = "COTA", "Área (km²)" = "AREA_KM2", "Volume (m³)" = "VOLUME_M3")
    )
  }, server = FALSE)
  outputOptions(output, "cav_table", suspendWhenHidden = FALSE)

  # input_feedback/save_feedback/cav_graph precisam ser registrados uma unica
  # vez no nivel superior do server (nao dentro de observeEvent) - caso
  # contrario outputOptions() falha ("nao esta na lista de output objects")
  # porque o binding ainda nao existe na primeira execucao do app, e o
  # output ficaria "recalculating" para sempre dentro do modal (Shiny so
  # detecta visibilidade de elementos que ja existiam no DOM na 1a renderizacao).
  input_feedback_msg <- reactiveVal(NULL)
  output$input_feedback <- renderUI(input_feedback_msg())
  outputOptions(output, "input_feedback", suspendWhenHidden = FALSE)

  save_feedback_msg <- reactiveVal(NULL)
  output$save_feedback <- renderUI(save_feedback_msg())
  outputOptions(output, "save_feedback", suspendWhenHidden = FALSE)

  output$cav_graph <- plotly::renderPlotly({
    df <- cav_data()
    req(nrow(df) > 0)
    cota_sang <- input$input_cota_sang
    linhas <- list()
    rotulos <- list()
    if (length(cota_sang) == 1L && is.finite(cota_sang)) {
      linhas <- list(list(
        type = "line", xref = "paper", x0 = 0, x1 = 1,
        yref = "y", y0 = cota_sang, y1 = cota_sang,
        line = list(color = "#c0392b", width = 2, dash = "dash")
      ))
      rotulos <- list(list(
        xref = "paper", x = 1, xanchor = "right",
        yref = "y", y = cota_sang, yanchor = "bottom",
        text = sprintf("Cota do sangradouro: %g m", cota_sang),
        showarrow = FALSE, font = list(color = "#c0392b"),
        bgcolor = "rgba(255,255,255,0.85)"
      ))
    }
    plotly::plot_ly(df, x = ~I(VOLUME_M3 / 1e6), y = ~COTA, type = "scatter", mode = "lines+markers", name = "Volume",
      hovertemplate = "Volume: %{x:.3f} hm³<br>Cota: %{y:.3f} m<extra></extra>") |>
      plotly::layout(xaxis = list(title = "Volume (hm³)"), yaxis = list(title = "Cota (m)"),
                     shapes = linhas, annotations = rotulos)
  })
  outputOptions(output, "cav_graph", suspendWhenHidden = FALSE)

  observeEvent(input$upload_cav_data, {
    if (!permitir_acao("cav_button")) return()
    f <- input$upload_cav_data
    req(f)
    cav_data(data.frame(COTA = numeric(0), AREA_KM2 = numeric(0), VOLUME_M3 = numeric(0)))
    save_feedback_msg(NULL)
    input_feedback_msg(NULL)
    ext <- tolower(tools::file_ext(f$name))
    df <- tryCatch({
      if (ext == "csv") utils::read.csv(f$datapath, stringsAsFactors = FALSE)
      else if (ext %in% c("xlsx", "xls")) as.data.frame(readxl::read_excel(f$datapath))
      else stop("Formato nao suportado")
    }, error = function(e) NULL)

    needed <- c("COTA", "AREA_KM2", "VOLUME_M3")
    if (is.null(df) || !all(needed %in% names(df))) {
      input_feedback_msg(div(class = "alert alert-danger", "Não foi possível importar o arquivo. Verifique o formato e as colunas COTA, AREA_KM2, VOLUME_M3."))
      return()
    }
    df <- df[, needed, drop = FALSE]
    if (!nrow(df) || !all(vapply(df, is.numeric, logical(1))) ||
        !all(is.finite(as.matrix(df)))) {
      input_feedback_msg(div(class = "alert alert-danger", "O arquivo CAV deve conter linhas com valores numéricos preenchidos nas três colunas."))
      return()
    }
    cav_data(df)
    input_feedback_msg(div(class = "alert alert-success", sprintf("Arquivo CAV importado: %d linhas. Confira a cota do sangradouro e confirme o salvamento.", nrow(df))))
  })

  observeEvent(input$ver_grafico_button, {
    if (!permitir_acao("ver_grafico_button")) return()
    df <- cav_data()
    if (nrow(df) == 0) return()
    shinyjs::show("cav_graph_container")
  })

  observeEvent(input$confirm_save_button, {
    if (!permitir_acao("confirm_save_button")) return()
    marcar_etapas(6:7, FALSE)
    cod <- input$input_cod
    cota_sang <- input$input_cota_sang
    df <- cav_data()
    if (nrow(df) == 0) {
      save_feedback_msg(div(class = "alert alert-warning", "Nenhum dado para salvar!"))
      return()
    }
    result <- tryCatch({
      capacity <- cav_capacity_hm3(df, cota_sang)
      save_cav(cod, cota_sang, df)
      list(cota = cota_sang, capacity_hm3 = capacity)
    }, error = function(e) e)
    if (inherits(result, "error")) {
      save_feedback_msg(div(class = "alert alert-danger", conditionMessage(result)))
    } else {
      confirmed_cav(result)
      marcar_etapas(6, TRUE)
      save_feedback_msg(div(class = "alert alert-success", "Dados salvos com sucesso!"))
    }
  })

  # =====================================================================
  # Etapa 7 — Executar Simulacao / Resetar tudo
  # =====================================================================
  observeEvent(input$open_simula_button, {
    if (!permitir_acao("open_simula_button")) return()
    marcar_etapas(7, FALSE)
    withProgress(message = "Executando Simulação MEVAZ...",
                 detail = "Aguarde a conclusão. Mantenha esta janela aberta.", {
      result <- tryCatch({
        ok_modreg <- ensure_simulation_modreg_input()
        if (!ok_modreg$ok) stop(ok_modreg$message)
        prune_orphan_test_shapes()
        run_simulation(file.path(BASE_DIR, "dados"))
        "OK"
      }, error = function(e) e)
    })

    if (inherits(result, "error")) {
      showModal(modalDialog(
        title = "Erro na Simulação",
        conditionMessage(result),
        easyClose = TRUE
      ))
    } else {
      sintese_path <- file.path(REDE_RES_TEST, "output", "sintese", "Sintese.xlsx")
      marcar_etapas(7, file.exists(sintese_path))
      showModal(modalDialog(
        title = "Fim da Simulação",
        if (file.exists(sintese_path)) {
          tagList(
            "Simulação concluída com sucesso.",
            tags$br(), tags$br(),
            downloadButton("download_sintese", "Baixar Sintese.xlsx")
          )
        } else {
          "Simulação concluída, mas Sintese.xlsx não foi encontrado."
        },
        footer = tagList(
          modalButton("Fechar"),
          if (file.exists(sintese_path)) actionButton("open_results_button_modal", "Resultados", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    }
  })

  open_results <- function() {
    req(etapas_concluidas()[7])
    loaded <- tryCatch({
      after <- read_results_snapshot(REDE_RES_TEST)
      before <- tryCatch(read_results_snapshot(REDE_RES_CONS), error = function(e) NULL)
      series <- tryCatch(read_results_series(REDE_RES_TEST), error = function(e) NULL)
      list(after = after, before = before, new_id = as.character(input$input_cod), series = series)
    }, error = function(e) e)
    if (inherits(loaded, "error")) {
      showModal(modalDialog(title = "Resultados", "Não foi possível carregar os resultados da simulação.",
        tags$p(conditionMessage(loaded)), footer = modalButton("Fechar")))
      return()
    }
    results_snapshot(loaded)
    showModal(modalDialog(title = "Resultados da simulação MEVAZ", size = "l",
      div(class = "mevaz-results-panel",
        tabsetPanel(
          tabPanel("Novo reservatório", uiOutput("results_new_reservoir")),
          tabPanel("Impactos na rede",
            tags$p("Reservatórios existentes com mudança na vazão regularizada em relação à rede consolidada, para a garantia selecionada."),
            selectInput("results_guarantee", "Garantia de atendimento", choices = c("90% — Q90" = "Q90", "95% — Q95" = "Q95", "98% — Q98" = "Q98")),
            tags$p("As vazões usam o método escolhido para cada reservatório. Confira os métodos antes e depois na tabela. Reservatórios sem resultado anterior disponível não entram na comparação; percentuais com vazão anterior zero não são calculados."),
            plotly::plotlyOutput("results_impact_plot", height = "360px"),
            DT::DTOutput("results_impact_table")
          ),
          tabPanel("Séries da simulação",
            tags$p("Resultados mensais do cenário com o novo reservatório, usando o método de regionalização escolhido para cada reservatório."),
            uiOutput("results_series_controls"),
            uiOutput("results_series_description"),
            tags$p("Selecione o período ou use a faixa abaixo dos gráficos para aproximar um intervalo. Passe o mouse para consultar valores; clique na legenda para ocultar ou mostrar curvas."),
            uiOutput("results_series_charts")
          )
        )
      ),
      footer = tagList(downloadButton("download_results_sintese", "Baixar Sintese.xlsx"), modalButton("Fechar")),
      easyClose = TRUE
    ))
  }
  observeEvent(input$open_results_button, open_results())
  observeEvent(input$open_results_button_modal, open_results())

  output$results_new_reservoir <- renderUI({
    req(etapas_concluidas()[7])
    result <- results_snapshot()
    req(result)
    row <- result$after$general[result$after$general$ID == result$new_id, , drop = FALSE]
    if (nrow(row) != 1L) return(div(class = "alert alert-warning", "O novo reservatório não foi encontrado na síntese."))
    tagList(
      tags$h4(row$Reservatorio),
      tags$p(sprintf("Código: %s • Método de regionalização: %s", row$ID, row$Metodo)),
      tags$p(sprintf("Capacidade: %s hm³ • Bacia hidrográfica: %s km²",
        results_number(row$Capacidade_hm3, 2), results_number(row$Bacia_Hidrografica_km2, 2))),
      div(class = "mevaz-results-cards", lapply(c("Q90", "Q95", "Q98"), function(q) {
        div(class = "mevaz-result-card", tags$h5(q),
          div(class = "mevaz-result-value", results_number(row[[q]]), tags$small(" m³/s")),
          tags$p(sprintf("Garantia de atendimento de %s%%", substring(q, 2))))
      })),
      tags$p("Vazão regularizada: retirada calculada pelo modelo para o nível de garantia indicado. O novo reservatório não possui resultado anterior para comparação.")
    )
  })

  results_comparison <- reactive({
    req(etapas_concluidas()[7])
    result <- results_snapshot()
    req(result)
    validate(need(!is.null(result$before), "Os resultados da rede consolidada não estão disponíveis para comparação."))
    guarantee <- input$results_guarantee
    if (is.null(guarantee)) guarantee <- "Q90"
    req(guarantee %in% c("Q90", "Q95", "Q98"))
    compare_results(result$after, result$before, result$new_id, guarantee)
  })

  output$results_impact_table <- DT::renderDT({
    df <- results_comparison()
    validate(need(nrow(df) > 0, "Nenhum reservatório apresentou mudança na vazão regularizada para a garantia selecionada."))
    DT::datatable(df, rownames = FALSE,
      colnames = c("Código", "Reservatório", "Método anterior", "Método simulado", "Antes (m³/s)", "Depois (m³/s)", "Diferença (m³/s)", "Diferença (%)"),
      options = list(pageLength = 10, scrollX = TRUE,
        language = list(search = "Buscar:", lengthMenu = "Mostrar _MENU_ reservatórios", info = "_START_ a _END_ de _TOTAL_ reservatórios", infoEmpty = "Sem reservatórios", zeroRecords = "Nenhum reservatório encontrado", paginate = list(previous = "Anterior", "next" = "Próxima")))) |>
      DT::formatRound(c("Antes", "Depois", "Diferenca"), digits = 3, mark = ".", dec.mark = ",") |>
      DT::formatRound("Percentual", digits = 2, mark = ".", dec.mark = ",")
  })

  output$results_impact_plot <- plotly::renderPlotly({
    df <- results_comparison()
    df <- df[is.finite(df$Diferenca), , drop = FALSE]
    validate(need(nrow(df) > 0, "Nenhum reservatório apresentou mudança na vazão regularizada para a garantia selecionada."))
    df <- df[order(abs(df$Diferenca), decreasing = TRUE), , drop = FALSE]
    # Mantem o grafico legivel; a tabela oferece todos os reservatorios.
    df <- head(df, 15)
    labels <- paste0(df$Reservatorio, " (", df$Codigo, ")")
    plotly::plot_ly(x = df$Diferenca, y = labels, type = "bar", orientation = "h",
      marker = list(color = ifelse(df$Diferenca < 0, "#b33a3a", "#00793C")),
      text = paste0(labels, "<br>Antes: ", round(df$Antes, 3), " m³/s<br>Depois: ", round(df$Depois, 3), " m³/s"),
      hoverinfo = "text+x") |>
      plotly::layout(title = "Variação da vazão regularizada — até 15 maiores diferenças",
        xaxis = list(title = "Diferença (m³/s)", zeroline = TRUE),
        yaxis = list(title = "", autorange = "reversed", automargin = TRUE), margin = list(l = 160))
  })

  output$results_series_controls <- renderUI({
    req(etapas_concluidas()[7])
    result <- results_snapshot()
    req(result)
    if (is.null(result$series)) return(div(class = "alert alert-warning", "As séries temporais desta simulação não estão disponíveis."))
    ids <- results_connected_ids(result$after$reservoirs, result$new_id)
    available <- unique(unlist(lapply(result$series, names), use.names = FALSE))
    reservoirs <- result$after$general
    reservoirs <- reservoirs[reservoirs$ID %in% intersect(ids, available), , drop = FALSE]
    if (!nrow(reservoirs)) return(div(class = "alert alert-warning", "Não há séries disponíveis para os reservatórios desta rede."))
    reservoirs <- reservoirs[order(reservoirs$ID != result$new_id, reservoirs$Reservatorio), , drop = FALSE]
    selected <- reservoirs$ID[1]
    dates <- as.Date(unlist(lapply(result$series$Q90[intersect(reservoirs$ID, names(result$series$Q90))], function(df) as.numeric(as.Date(df$Datas)))), origin = "1970-01-01")
    dates <- dates[!is.na(dates)]
    if (!length(dates)) return(div(class = "alert alert-warning", "As séries não possuem datas disponíveis."))
    period <- range(dates)
    choices <- stats::setNames(reservoirs$ID, paste0(reservoirs$Reservatorio, " (", reservoirs$ID, ")", ifelse(reservoirs$ID == result$new_id, " — Novo", "")))
    tagList(
      fluidRow(
        column(8, selectInput("results_series_reservoir", "Reservatório", choices = choices, selected = selected)),
        column(4, selectInput("results_series_guarantee", "Garantia de atendimento", choices = c("90% — Q90" = "Q90", "95% — Q95" = "Q95", "98% — Q98" = "Q98")))
      ),
      dateRangeInput("results_series_period", "Período", start = period[1], end = period[2], min = period[1], max = period[2], language = "pt-BR", format = "dd/mm/yyyy", separator = " até ", width = "100%"),
      checkboxGroupInput("results_series_variables", "Séries exibidas",
        choices = c("Volume armazenado" = "Vol_fimmes_hm3", "Afluência" = "Aflu_m3s", "Retirada atendida" = "Ret_m3s", "Vertimento" = "Ver_m3s"),
        selected = c("Vol_fimmes_hm3", "Aflu_m3s", "Ret_m3s", "Ver_m3s"), inline = TRUE)
    )
  })

  results_series_data <- reactive({
    req(etapas_concluidas()[7])
    result <- results_snapshot()
    req(result)
    validate(need(!is.null(result$series), "As séries temporais desta simulação não estão disponíveis."))
    req(input$results_series_reservoir, input$results_series_guarantee, input$results_series_period)
    df <- tryCatch(prepare_results_series(result$series, input$results_series_reservoir,
      input$results_series_guarantee, input$results_series_period), error = function(e) e)
    validate(need(!inherits(df, "error"), if (inherits(df, "error")) conditionMessage(df) else ""))
    df
  })

  output$results_series_description <- renderUI({
    df <- results_series_data()
    result <- results_snapshot()
    reservoir <- result$after$general[result$after$general$ID == input$results_series_reservoir, , drop = FALSE]
    req(nrow(reservoir) == 1L)
    tags$p(sprintf("Método: %s • %d registros mensais • %s a %s. O volume é o armazenado ao fim do mês; a retirada é a efetivamente atendida na simulação.",
      reservoir$Metodo, nrow(df), format(min(df$Datas), "%m/%Y"), format(max(df$Datas), "%m/%Y")))
  })

  output$results_series_charts <- renderUI({
    req(etapas_concluidas()[7])
    result <- results_snapshot()
    req(result, result$series)
    variables <- input$results_series_variables
    if (!length(variables)) return(div(class = "alert alert-info", "Selecione pelo menos uma série para visualizar."))
    tagList(
      if ("Vol_fimmes_hm3" %in% variables) tagList(tags$h4("Volume armazenado"), plotly::plotlyOutput("results_series_volume", height = "350px")),
      if (any(variables %in% c("Aflu_m3s", "Ret_m3s", "Ver_m3s"))) tagList(tags$h4("Vazões mensais"), plotly::plotlyOutput("results_series_flows", height = "400px"))
    )
  })

  output$results_series_volume <- plotly::renderPlotly({
    req("Vol_fimmes_hm3" %in% input$results_series_variables)
    results_time_plot(results_series_data(), "Vol_fimmes_hm3", volume = TRUE)
  })
  output$results_series_flows <- plotly::renderPlotly({
    variables <- intersect(input$results_series_variables, c("Aflu_m3s", "Ret_m3s", "Ver_m3s"))
    req(length(variables) > 0)
    results_time_plot(results_series_data(), variables)
  })

  output$download_results_sintese <- downloadHandler(
    filename = function() "Sintese.xlsx",
    content = function(file) {
      req(etapas_concluidas()[7])
      file.copy(file.path(REDE_RES_TEST, "output", "sintese", "Sintese.xlsx"), file)
    }
  )

  output$download_sintese <- downloadHandler(
    filename = function() "Sintese.xlsx",
    content = function(file) {
      req(etapas_concluidas()[7])
      file.copy(file.path(REDE_RES_TEST, "output", "sintese", "Sintese.xlsx"), file)
    }
  )

  observeEvent(input$reset_button, {
    marcar_etapas(1:7, FALSE)
    wizard_step(1)
    wizard_branch("B")
    updateRadioButtons(session, "basin_method", selected = "B")
    clear_previous_simulation_data()
    reset_reservoir_state()
    point_feedback_msg(NULL)
  })

  # =====================================================================
  # Carta e informacoes gerais
  # =====================================================================
  output$info_reservatorios <- renderText({
    paste0(length(EXISTING_RESERVOIR_IDS), " reservatórios já existem na rede consolidada.")
  })

  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
      # OpenStreetMap como unico fundo; o controle abaixo permite alternar
      # apenas as camadas de drenagem e regioes hidrograficas.
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") |>
      leaflet::setView(lng = CENTER_LON, lat = CENTER_LAT, zoom = 7) |>
      leaflet::addPolygons(
        data = BASINS_SF,
        weight = 1,
        color = FUNCEME_GREEN,
        fillColor = FUNCEME_GREEN,
        fillOpacity = 0.05,
        group = "Regiões hidrográficas"
      ) |>
      # Rede de drenagem ANADEM (o mesmo alvo de find_nearest_point()): so
      # aparece a partir do zoom 9 - no zoom estadual (7) seria uma pelota
      # ilegivel de milhares de linhas. O encaixe do ponto e sempre no
      # servidor, em resolucao total, independente do que esta desenhado.
      leaflet::addPolylines(
        data = VETOR_RIOS_LL,
        color = "#1E90FF",
        weight = 1.2,
        opacity = 0.75,
        group = "Rede de drenagem"
      ) |>
      leaflet::addMapPane("named-rivers", zIndex = 450) |>
      leaflet::addPolylines(
        data = NAMED_DRAINAGE_LL,
        color = "#008a87", weight = 2, opacity = 0.75,
        options = leaflet::pathOptions(pane = "named-rivers"),
        label = ~as.character(river_name),
        labelOptions = leaflet::labelOptions(direction = "auto"),
        group = "Rios com nome cadastrado"
      ) |>
      leaflet::addLayersControl(
        overlayGroups = c("Rede de drenagem", "Rios com nome cadastrado", "Regiões hidrográficas"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::groupOptions("Rede de drenagem", zoomLevels = 9:18) |>
      leaflet::groupOptions("Rios com nome cadastrado", zoomLevels = 9:18)
  })
}
