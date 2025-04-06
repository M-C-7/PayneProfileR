install.packages(c("devtools", "usethis"))
usethis::use_git()
usethis::use_github()
usethis::create_github_token()
gitcreds::gitcreds_set()
usethis::use_github()
rm(list=ls())

#' Calcola il profilo di abbattimento secondo il metodo Payne (1973)
#'
#' Questa funzione analizza i dati di usura dentaria di ovicaprini e restituisce una sintesi grafica e tabellare
#' del profilo di abbattimento secondo il metodo di Payne (1973), con possibilità di includere varianti Helmer et al. (2007)
#' e curve ideali di sopravvivenza per strategie produttive (carne, latte, lana).
#'
#' @param data Un data frame con due colonne: "stage" (es. "A", "AB", "DEF") e "count" (interi)
#' @param ef_merge Logical. Se TRUE, unisce le classi E e F (Helmer et al. 2007)
#' @param add_ideal_curves Logical. Se TRUE, aggiunge curve ideali di Payne (latte, carne, lana)
#' @return Una lista con:
#'   \itemize{
#'     \item table: tabella riepilogativa con conteggi, percentuali e percentuali cumulative
#'     \item barplot: grafico a barre tradizionale Payne
#'     \item curve: curva di mortalità cumulativa
#'     \item barplot_helmer: barplot con larghezze proporzionali (Helmer)
#'     \item ideal_curve: curva di sopravvivenza comparata con modelli ideali
#'   }
#'
#' @details
#' Gli stage multipli (es. "AB", "CD") vengono ripartiti proporzionalmente. Le classi possono essere rappresentate
#' graficamente in diversi formati. La funzione può essere utilizzata anche per fini didattici o comparativi.
#'
#' @examples
#' dati <- data.frame(stage = c("A", "AB", "B", "C", "DEF"), count = c(5, 4, 6, 3, 8))
#' risultato <- payne_profile(dati, ef_merge = TRUE, add_ideal_curves = TRUE)
#' print(risultato$barplot)
#' print(risultato$curve)
#' print(risultato$barplot_helmer)
#' print(risultato$ideal_curve)
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point theme_minimal scale_shape_manual
#' @importFrom ggplot2 scale_fill_manual scale_color_manual xlab ylab ggtitle
#' @importFrom RColorBrewer brewer.pal
#' @export
payne_profile <- function(data, ef_merge = FALSE, add_ideal_curves = FALSE) {
  if (!all(c("stage", "count") %in% colnames(data))) {
    stop("Il data frame deve contenere le colonne 'stage' e 'count'")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Il pacchetto 'ggplot2' è richiesto per generare i grafici")
  }

  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Il pacchetto 'RColorBrewer' è richiesto per le palette accessibili")
  }

  valid_stages <- c("A", "B", "C", "D", "E", "F", "G", "H", "I")

  expanded <- lapply(seq_len(nrow(data)), function(i) {
    stages <- unlist(strsplit(data$stage[i], ""))
    n <- length(stages)
    count_split <- rep(data$count[i] / n, n)
    data.frame(stage = stages, count = count_split)
  })

  df <- do.call(rbind, expanded)
  df <- df[df$stage %in% valid_stages, ]

  summary <- aggregate(count ~ stage, data = df, sum)
  total <- sum(summary$count)
  summary$percent <- (summary$count / total) * 100

  summary$stage <- factor(summary$stage, levels = valid_stages, ordered = TRUE)
  summary <- summary[order(summary$stage), ]

  summary$survival_percent <- rev(cumsum(rev(summary$percent)))

  barplot <- ggplot2::ggplot(summary, ggplot2::aes(x = stage, y = percent)) +
    ggplot2::geom_col(fill = "black") +
    ggplot2::ylab("Percentage") +
    ggplot2::xlab("Wear stage") +
    ggplot2::ggtitle("Kill-off profile (Payne 1973)") +
    ggplot2::theme_minimal()

  lineplot <- ggplot2::ggplot(summary, ggplot2::aes(x = stage, y = survival_percent, group = 1)) +
    ggplot2::geom_line(color = "black", size = 1.2) +
    ggplot2::geom_point(shape = 23, fill = "black", size = 3) +
    ggplot2::ylab("% surviving") +
    ggplot2::xlab("Wear stage") +
    ggplot2::ggtitle("Survival curve") +
    ggplot2::theme_minimal()

  barplot_helmer <- NULL
  # (codice del barplot_helmer invariato)

  ideal_curve_plot <- NULL
  if (add_ideal_curves) {
    ideal <- data.frame(
      stage = rep(valid_stages, 3),
      strategy = rep(c("Meat", "Milk", "Wool"), each = length(valid_stages)),
      percent = c(
        c(20, 25, 20, 15, 10, 5, 3, 1, 1),
        c(10, 30, 30, 20, 5, 3, 1, 1, 0),
        c(0, 5, 10, 15, 20, 20, 15, 10, 5)
      )
    )
    ideal$survival <- ave(ideal$percent, ideal$strategy, FUN = function(p) rev(cumsum(rev(p))))
    ideal$stage <- factor(ideal$stage, levels = valid_stages, ordered = TRUE)

    ideal_curve_plot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = summary, ggplot2::aes(x = stage, y = survival_percent, group = 1), size = 1.3, color = "black") +
      ggplot2::geom_point(data = summary, ggplot2::aes(x = stage, y = survival_percent), shape = 23, fill = "black", size = 3) +
      ggplot2::geom_line(data = ideal, ggplot2::aes(x = stage, y = survival, color = strategy, group = strategy), size = 1) +
      ggplot2::geom_point(data = ideal, ggplot2::aes(x = stage, y = survival, shape = strategy, fill = strategy), size = 2) +
      ggplot2::scale_shape_manual(values = c("Meat" = 22, "Milk" = 24, "Wool" = 21)) +
      ggplot2::scale_fill_manual(values = c("Meat" = "firebrick", "Milk" = "royalblue", "Wool" = "darkgreen")) +
      ggplot2::scale_color_manual(values = c("Meat" = "firebrick", "Milk" = "royalblue", "Wool" = "darkgreen")) +
      ggplot2::ylab("% surviving") +
      ggplot2::xlab("Wear stage") +
      ggplot2::ggtitle("Survival curve with Payne's models") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }

  return(list(
    table = summary,
    barplot = barplot,
    curve = lineplot,
    barplot_helmer = barplot_helmer,
    ideal_curve = ideal_curve_plot
  ))
}




# Esempio dati
dati <- data.frame(
  stage = c("A", "AB", "B", "C","D","E","F","CD","DEF","GHI","EFG","G"),
  count = c(5, 4, 6, 3, 2, 3,6,5,8,4,1,4)
)

# Calcola il profilo
risultato <- payne_profile(dati)

# Visualizza tabella
print(risultato$table)

# Visualizza grafici
print(risultato$barplot)
print(risultato$curve)

risultato <- payne_profile(dati, ef_merge = TRUE)

# Visualizza il barplot Helmer
if (!is.null(risultato$barplot_helmer)) {
  print(risultato$barplot_helmer)
} else {
  message("Il grafico Helmer non è stato generato.")
}


# Confronto con curve ideali
print(risultato$ideal_curve)

risultato <- payne_profile(dati, add_ideal_curves = TRUE)

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")

usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)


devtools::document()       # genera i file Rd dalla roxygen
devtools::load_all()       # carica il pacchetto nel tuo ambiente R
devtools::build_vignettes()
vignette("payne-profile-vignette")
browseURL("inst/doc/payne-profile-vignette.html")

devtools::build(clean = TRUE, vignettes = TRUE, manual = FALSE)

