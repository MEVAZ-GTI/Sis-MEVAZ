# Dados do painel: Qgaran_Selec usa o metodo escolhido por reservatorio,
# e suas vazoes ja estao em m3/s (a sintese de balanco usa litros/s).
read_results_snapshot <- function(network_dir) {
  out <- file.path(network_dir, "output")
  general <- readRDS(file.path(out, "sintese", "DadosGerais.rds"))
  flows <- readRDS(file.path(out, "Qgaran_Selec.rds"))
  reservoirs <- openxlsx::read.xlsx(file.path(out, "Dados_Reservatorios.xlsx"))
  required <- c("Q90", "Q95", "Q98")
  if (is.null(rownames(flows)) || !all(required %in% colnames(flows))) {
    stop("As vazões regularizadas não possuem códigos ou garantias válidas.")
  }
  general$ID <- as.character(general$ID)
  for (q in required) general[[q]] <- flows[match(general$ID, rownames(flows)), q]
  general$Metodo <- reservoirs$MODELO_REGIONALIZACAO[match(general$ID, as.character(reservoirs$ID))]
  list(general = general, reservoirs = reservoirs)
}

results_connected_ids <- function(reservoirs, new_id) {
  ids <- as.character(reservoirs$ID)
  downstream <- as.character(reservoirs$ID_ACJUS)
  connected <- as.character(new_id)
  repeat {
    expanded <- intersect(ids, unique(c(connected,
      downstream[ids %in% connected], ids[downstream %in% connected])))
    if (setequal(expanded, connected)) return(expanded)
    connected <- expanded
  }
}

compare_results <- function(after, before, new_id, guarantee) {
  connected <- results_connected_ids(after$reservoirs, new_id)
  current <- after$general
  current <- current[current$ID %in% connected & current$ID != as.character(new_id), ]
  reference <- before$general
  index <- match(current$ID, reference$ID)
  previous <- reference[[guarantee]][index]
  simulated <- current[[guarantee]]
  difference <- simulated - previous
  percent <- rep(NA_real_, length(previous))
  valid <- is.finite(previous) & previous != 0 & is.finite(difference)
  percent[valid] <- difference[valid] / previous[valid] * 100
  comparison <- data.frame(Codigo = current$ID, Reservatorio = current$Reservatorio,
    Metodo_antes = reference$Metodo[index], Metodo_depois = current$Metodo,
    Antes = previous, Depois = simulated, Diferenca = difference, Percentual = percent,
    stringsAsFactors = FALSE)
  # Desconsidera ruido numerico e casos sem referencia para comparacao.
  comparison[is.finite(comparison$Diferenca) & abs(comparison$Diferenca) > 1e-8, , drop = FALSE]
}

results_number <- function(value, digits = 3) {
  if (length(value) != 1L || !is.finite(value)) return("Não disponível")
  formatC(value, format = "f", digits = digits, decimal.mark = ",", big.mark = ".")
}

read_results_series <- function(network_dir) {
  series <- readRDS(file.path(network_dir, "output", "Resultados_SimulacaoRede_Selec.rds"))
  if (!is.list(series) || !all(c("Q90", "Q95", "Q98") %in% names(series))) {
    stop("O arquivo de séries não contém as garantias Q90, Q95 e Q98.")
  }
  series
}

prepare_results_series <- function(series, reservoir_id, guarantee, period = NULL) {
  if (!guarantee %in% c("Q90", "Q95", "Q98")) stop("Selecione uma garantia válida.")
  df <- series[[guarantee]][[as.character(reservoir_id)]]
  columns <- c("Datas", "Aflu_m3s", "Ret_m3s", "Ver_m3s", "Vol_fimmes_hm3")
  if (!is.data.frame(df) || !nrow(df) || !all(columns %in% names(df))) {
    stop("Não há séries completas para este reservatório e esta garantia.")
  }
  df <- df[, columns, drop = FALSE]
  df$Datas <- as.Date(df$Datas)
  df <- df[!is.na(df$Datas), , drop = FALSE]
  df <- df[order(df$Datas), , drop = FALSE]
  if (!nrow(df)) stop("As séries não possuem datas válidas.")
  if (!all(vapply(df[, columns[-1], drop = FALSE], is.numeric, logical(1)))) {
    stop("As séries não possuem valores numéricos válidos.")
  }
  if (!is.null(period)) {
    period <- as.Date(period)
    if (length(period) != 2L || anyNA(period) || period[1] > period[2]) {
      stop("Selecione um período válido.")
    }
    df <- df[df$Datas >= period[1] & df$Datas <= period[2], , drop = FALSE]
    if (!nrow(df)) stop("Não há dados mensais no período selecionado.")
  }
  df
}

results_time_plot <- function(df, variables, volume = FALSE) {
  labels <- c(Aflu_m3s = "Afluência", Ret_m3s = "Retirada atendida",
    Ver_m3s = "Vertimento", Vol_fimmes_hm3 = "Volume ao fim do mês")
  colors <- c(Aflu_m3s = "#2471a3", Ret_m3s = "#00793C", Ver_m3s = "#b56912", Vol_fimmes_hm3 = "#12B0A0")
  unit <- if (volume) "hm³" else "m³/s"
  graph <- plotly::plot_ly()
  for (variable in variables) {
    graph <- plotly::add_trace(graph, x = df$Datas, y = df[[variable]],
      type = "scatter", mode = "lines", name = unname(labels[variable]),
      line = list(color = unname(colors[variable])), connectgaps = FALSE,
      hovertemplate = paste0("%{x|%m/%Y}<br>%{y:.3f} ", unit, "<extra>%{fullData.name}</extra>"))
  }
  graph |>
    plotly::layout(
      xaxis = list(title = "", type = "date", rangeslider = list(visible = TRUE, thickness = 0.12)),
      yaxis = list(title = if (volume) "Volume armazenado (hm³)" else "Vazão (m³/s)", rangemode = "tozero"),
      hovermode = "x unified", legend = list(orientation = "h", x = 0, y = 1.15),
      margin = list(l = 75, r = 25, b = 55, t = 55)) |>
    plotly::config(displaylogo = FALSE, locale = "pt-BR")
}
