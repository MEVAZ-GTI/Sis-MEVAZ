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
  wizard_branch <- reactiveVal(NULL)

  output$wizard_step_js <- renderText(as.character(wizard_step()))
  outputOptions(output, "wizard_step_js", suspendWhenHidden = FALSE)

  output$wizard_branch_js <- renderText(if (is.null(wizard_branch())) "" else wizard_branch())
  outputOptions(output, "wizard_branch_js", suspendWhenHidden = FALSE)

  step_labels <- c("1" = "Dados básicos", "2" = "Método", "3" = "Localização",
                   "4" = "Bacia e dados", "5" = "CAV", "6" = "Simulação")
  output$wizard_progress_label <- renderUI({
    s <- wizard_step()
    tags$div(
      sprintf("Etapa %d/6 — %s", s, step_labels[[as.character(s)]]),
      style = sprintf(
        "text-align:center; font-weight:700; margin-bottom:12px; color:%s; text-transform:uppercase; letter-spacing:0.5px; font-size:13px;",
        FUNCEME_ORANGE
      )
    )
  })

  observe({
    pct <- wizard_step() / 6 * 100
    shinyjs::runjs(sprintf("document.getElementById('wizard_progress_fill').style.width = '%.1f%%';", pct))
  })

  observeEvent(input$btn_branch_a, { wizard_branch("A"); wizard_step(3) })
  observeEvent(input$btn_branch_b, { wizard_branch("B"); wizard_step(3) })
  observeEvent(input$btn_step2_back, wizard_step(1))
  observeEvent(input$btn_step3_back, wizard_step(2))
  observeEvent(input$btn_step3_next, wizard_step(4))
  observeEvent(input$btn_step4_back, wizard_step(3))
  observeEvent(input$btn_step4_next, wizard_step(5))
  observeEvent(input$btn_step5_back, wizard_step(4))
  observeEvent(input$btn_step5_next, wizard_step(6))
  observeEvent(input$btn_step6_back, wizard_step(5))

  # =====================================================================
  # Estado de dados do reservatorio (equivalente ao analysis-results-store
  # e as variaveis globais tabela/geo_df do Python)
  # =====================================================================
  analysis_results <- reactiveValues(
    station_code = NULL, basin_name = NULL, drainage_name = NULL,
    selected_point = NULL,  # list(lon=, lat=)
    containing_sub_basin_area_sq_km = NULL, intersection_area_sq_km = NULL
  )
  tabela_cas_state <- reactiveVal(NULL)  # Tabela_atributos_acudes_Nova (data.frame)
  geo_sf_state <- reactiveVal(NULL)      # new_exut_acudes (sf)
  # TRUE quando o ponto nao esta dentro de nenhuma sub-bacia consolidada, ou
  # seja, o novo reservatorio nao tem jusante (ID_ACJUS = 0). Nesse caso a
  # cascata e legitimamente vazia e o Dados_Novos so tem o novo reservatorio.
  standalone_state <- reactiveVal(FALSE)
  cav_data <- reactiveVal(data.frame(COTA = numeric(0), AREA_KM2 = numeric(0), VOLUME_M3 = numeric(0)))

  # Painel de status sempre visivel (equivalente ao 'nearest-point-info' do
  # Python): mostra o ponto usado na selecao e o resultado do ultimo tracado.
  point_feedback_msg <- reactiveVal(NULL)
  output$point_feedback <- renderUI(point_feedback_msg())
  outputOptions(output, "point_feedback", suspendWhenHidden = FALSE)

  status_div <- function(text, color = "#333") {
    div(style = sprintf("text-align:center; font-weight:700; color:%s; margin-top:6px;", color), text)
  }

  reset_reservoir_state <- function() {
    tabela_cas_state(NULL)
    geo_sf_state(NULL)
    standalone_state(FALSE)
    cav_data(data.frame(COTA = numeric(0), AREA_KM2 = numeric(0), VOLUME_M3 = numeric(0)))
    analysis_results$station_code <- NULL
    analysis_results$basin_name <- NULL
    analysis_results$drainage_name <- NULL
    analysis_results$selected_point <- NULL
    analysis_results$containing_sub_basin_area_sq_km <- NULL
    analysis_results$intersection_area_sq_km <- NULL
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("point_markers") |>
      leaflet::clearGroup("bacia") |>
      leaflet::clearGroup("cascata")
  }

  # =====================================================================
  # Etapa 1 — Dados basicos
  # =====================================================================
  observeEvent(input$validate_codigo_button, {
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
    wizard_step(2)
  })

  # =====================================================================
  # Etapa 3 — Localizacao do ponto
  # =====================================================================
  run_point_analysis <- function(lon, lat) {
    res <- comprehensive_point_analysis(lon, lat)
    analysis_results$station_code <- res$station_code
    analysis_results$basin_name <- res$basin_name
    analysis_results$drainage_name <- res$drainage_name
    analysis_results$selected_point <- list(lon = lon, lat = lat)
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

    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("point_markers") |>
      leaflet::addPolylines(lng = c(raw_lon, snap_lon), lat = c(raw_lat, snap_lat),
                            color = "#FF0000", weight = 2, opacity = 0.9,
                            dashArray = "4,6", group = "point_markers") |>
      leaflet::addCircleMarkers(lng = raw_lon, lat = raw_lat, radius = 6, color = "#0000FF",
                                fillColor = "#0000FF", fillOpacity = 1, weight = 1,
                                group = "point_markers") |>
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
  # Etapa 4 (B) — Trace a Bacia / Trace a Cascata / Resetar
  # =====================================================================
  observeEvent(input$run_r_script, {
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
    cod_value <- suppressWarnings(as.integer(input$input_cod))
    nomeac_value <- input$input_nomeac
    mod_reg_value <- input$input_mod_reg

    tabela_cas <- tabela_cas_state()
    if (is.null(tabela_cas)) {
      showNotification("Trace a Bacia antes de traçar a cascata.", type = "error")
      return()
    }

    withProgress(message = "Traçando a cascata...", {
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
        showNotification(paste("Erro ao traçar a cascata:", conditionMessage(result)), type = "error", duration = 10)
        point_feedback_msg(status_div(paste("Erro ao traçar a cascata:", conditionMessage(result)), "#FF0000"))
        return()
      }

      casc <- result$cascata
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
    showNotification("Mapa e dados de teste limpos.", type = "message")
    point_feedback_msg(status_div("Mapa restaurado e dados de teste (shapefiles e Dados_Novos_Reservatorios) apagados.", "#333"))
  })

  # =====================================================================
  # Etapa 4 (A) — Importar shapefile proprio
  # =====================================================================
  observeEvent(input$btn_gerar_dados, {
    cod_value <- input$input_cod
    nomeac_value <- input$input_nomeac
    mod_reg_value <- input$input_mod_reg
    id_jusante <- input$input_id_jusante
    upload <- input$upload_shapefile_a

    show_err <- function(msg) output$step4a_feedback <- renderUI(div(class = "alert alert-danger", msg))

    if (is.null(upload)) {
      return(show_err("Importe o shapefile da bacia (.shp, .shx, .dbf, .prj) antes de gerar os dados."))
    }
    shp_idx <- which(grepl("\\.shp$", upload$name, ignore.case = TRUE))
    if (length(shp_idx) == 0) {
      return(show_err("Nenhum arquivo .shp encontrado. Selecione todos os arquivos do shapefile."))
    }

    # fileInput grava com nomes temporarios; copiamos preservando os nomes
    # originais lado a lado, para que sf::st_read encontre .dbf/.prj/.shx.
    tmp_dir <- tempfile("mevaz_upload_")
    dir.create(tmp_dir)
    for (i in seq_len(nrow(upload))) {
      file.copy(upload$datapath[i], file.path(tmp_dir, upload$name[i]))
    }
    shp_path <- file.path(tmp_dir, upload$name[shp_idx[1]])

    analysis <- list(
      station_code = analysis_results$station_code,
      basin_name = analysis_results$basin_name,
      drainage_name = analysis_results$drainage_name,
      selected_point = analysis_results$selected_point
    )

    withProgress(message = "Gerando dados...", {
      draw_sf <- tryCatch(sf::st_transform(sf::st_read(shp_path, quiet = TRUE), 4326), error = function(e) NULL)

      result <- tryCatch(
        generate_dados_novos_option_a(shp_path, cod_value, nomeac_value, mod_reg_value, id_jusante,
                                      analysis, input$input_lat, input$input_lon),
        error = function(e) e
      )
      unlink(tmp_dir, recursive = TRUE)

      if (inherits(result, "error")) {
        show_err(conditionMessage(result))
        return()
      }

      output$step4a_feedback <- renderUI(div(
        class = "alert alert-success",
        "Shapefile importado e dados gerados com sucesso! Informe agora o CAV (com a Cota do Sangrador)."
      ))

      if (!is.null(draw_sf)) {
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup("bacia") |>
          leaflet::addPolygons(data = draw_sf, color = "green", weight = 2,
                               fillOpacity = 0.2, fillColor = "green", group = "bacia")
      }

      wizard_step(5)
    })
  })

  # =====================================================================
  # Etapa 5 — CAV
  # =====================================================================
  observeEvent(input$cav_button, showModal(cav_modal_ui()))

  output$cav_table <- DT::renderDT({
    DT::datatable(
      cav_data(), editable = TRUE, rownames = FALSE,
      colnames = c("Cota (m)" = "COTA", "Área (km²)" = "AREA_KM2", "Volume (m³)" = "VOLUME_M3")
    )
  }, server = FALSE)
  outputOptions(output, "cav_table", suspendWhenHidden = FALSE)

  observeEvent(input$cav_table_cell_edit, {
    info <- input$cav_table_cell_edit
    df <- cav_data()
    df[info$row, info$col + 1] <- DT::coerceValue(info$value, df[info$row, info$col + 1])
    cav_data(df)
  })

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
    plotly::plot_ly(df, x = ~AREA_KM2, y = ~COTA, type = "scatter", mode = "lines+markers", name = "Área") |>
      plotly::layout(xaxis = list(title = "Área (km²)"), yaxis = list(title = "Cota (m)"))
  })
  outputOptions(output, "cav_graph", suspendWhenHidden = FALSE)

  observeEvent(input$add_data_button, {
    cota <- input$cota_input; area <- input$area_input; volume <- input$volume_input
    if (is.null(cota) || is.na(cota) || is.null(area) || is.na(area) || is.null(volume) || is.na(volume)) {
      input_feedback_msg(div(class = "alert alert-warning", "Por favor, preencha todos os campos"))
      return()
    }
    df <- cav_data()
    df <- rbind(df, data.frame(COTA = cota, AREA_KM2 = area, VOLUME_M3 = volume))
    cav_data(df)
    input_feedback_msg(NULL)
  })

  observeEvent(input$upload_cav_data, {
    f <- input$upload_cav_data
    req(f)
    ext <- tolower(tools::file_ext(f$name))
    df <- tryCatch({
      if (ext == "csv") utils::read.csv(f$datapath, stringsAsFactors = FALSE)
      else if (ext %in% c("xlsx", "xls")) openxlsx::read.xlsx(f$datapath)
      else stop("Formato nao suportado")
    }, error = function(e) NULL)

    needed <- c("COTA", "AREA_KM2", "VOLUME_M3")
    if (is.null(df) || !all(needed %in% names(df))) {
      showNotification("O arquivo precisa ter as colunas COTA, AREA_KM2, VOLUME_M3.", type = "error")
      return()
    }
    cav_data(df[, needed, drop = FALSE])
  })

  observeEvent(input$ver_grafico_button, {
    df <- cav_data()
    if (nrow(df) == 0) return()
    shinyjs::show("cav_graph_container")
  })

  observeEvent(input$confirm_save_button, {
    cod <- input$input_cod
    cota_sang <- input$input_cota_sang
    df <- cav_data()
    if (nrow(df) == 0) {
      save_feedback_msg(div(class = "alert alert-warning", "Nenhum dado para salvar!"))
      return()
    }
    result <- tryCatch({ save_cav(cod, cota_sang, df); "OK" }, error = function(e) e)
    if (inherits(result, "error")) {
      save_feedback_msg(div(class = "alert alert-danger", conditionMessage(result)))
    } else {
      save_feedback_msg(div(class = "alert alert-success", "Dados salvos com sucesso!"))
    }
  })

  # =====================================================================
  # Etapa 6 — Executar Simulacao / Resetar tudo
  # =====================================================================
  observeEvent(input$open_simula_button, {
    withProgress(message = "Executando Simulação MEVAZ...", {
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
        easyClose = TRUE
      ))
    }
  })

  output$download_sintese <- downloadHandler(
    filename = function() "Sintese.xlsx",
    content = function(file) {
      file.copy(file.path(REDE_RES_TEST, "output", "sintese", "Sintese.xlsx"), file)
    }
  )

  observeEvent(input$reset_button, {
    wizard_step(1)
    wizard_branch(NULL)
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
      # Fundo claro/neutro (CartoDB Positron) como base: ao contrario do
      # OpenStreetMap, nao desenha os proprios rios, entao a rede de drenagem
      # ANADEM abaixo fica sendo o UNICO conjunto de linhas azuis na carta -
      # evita a dissonancia de dois reticulados de rios que nao coincidem
      # (ANADEM = tracado do MNT usado no traçado; OSM = cartografico).
      # O OpenStreetMap fica disponivel como camada alternativa no controle.
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Mapa claro") |>
      leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap") |>
      leaflet::setView(lng = CENTER_LON, lat = CENTER_LAT, zoom = 7) |>
      leaflet::addPolygons(
        data = BASINS_SF,
        weight = 1,
        color = FUNCEME_GREEN,
        fillColor = FUNCEME_GREEN,
        fillOpacity = 0.05,
        label = ~as.character(BACIA),
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
      leaflet::addLayersControl(
        baseGroups = c("Mapa claro", "OpenStreetMap"),
        overlayGroups = c("Rede de drenagem", "Regiões hidrográficas"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::groupOptions("Rede de drenagem", zoomLevels = 9:18)
  })
}
