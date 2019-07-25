options(scipen = 999)
options(stringsAsFactors = FALSE)
options(warn = 1)

date <- format.Date(Sys.Date(), "%Y%m%d")

needed_packages <- c("dplyr", "tidyr", "openxlsx", "ggplot2")
load_packages <- function(x) {
  if (!(x %in% rownames(installed.packages()))) {
    install.packages(x)
  }
  suppressPackageStartupMessages(require(x, character.only = TRUE))
}
sapply(needed_packages, load_packages)

theme_set(theme_bw())

#------------------red--------blue-----yellow------teal------orange------lime
ATI_colors <- c("#E53935", "#80D8FF", "#FFD54F", "#009688", "#FF9800", "#CDDC39",
                "#AB000D", "#49A7CC", "#C8A415", "#00675B", "#C66900", "#99AA00")
		
#------------------blue-------red-------green------purple-----lt blue-----orange		
NHA_colors <- c("#4f81bd", "#c0504d", "#9bbb59", "#8064a2", "#4bacc6", "#f79646")

#-----------------orange----dk grey-----dk blue----lt blue----md grey---lt grey		
Kog_colors <- c("#e4832b", "#393939", "#0a3c55", "#0e94ba", "#666666", "#cccccc")

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
