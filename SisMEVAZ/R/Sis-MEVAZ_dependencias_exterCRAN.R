

#' Instala dependências externas ao CRAN
#'
#' Instala bibliotecas hydrobr e phylin, externas ao CRAN
#'
#'
#' @return NULL
#' @export
SisMEVAZ_instala_dependencias_externas <- function() {
pkgs_needed <- c("remotes","hydrobr", "phylin")
for (pkg in pkgs_needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Instalando pacote:", pkg))
    if(pkg == "remotes"){
      utils::install.packages("remotes")
    }else if (pkg == "hydrobr") {
      remotes::install_github("lhmet/hydrobr@4b9c752e6c5a3e06267785aa0ad5e35b67e20241", dependencies = "hard")
    } else if (pkg == "phylin") {
      install.packages("phylin")
    }
  }
 }
}






