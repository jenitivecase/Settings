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

fct_case_when <- function(...) {
  args <- as.list(match.call())
  levels <- sapply(args[-1], function(f) f[[3]])  # extract RHS of formula
  levels <- levels[!is.na(levels)]
  factor(dplyr::case_when(...), levels=levels)
}

linetrunc <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_"), newline = "\n") {
  if(length(tol) > 2) stop("Please specify 1 or 2 values for tol.")
  
  lets <- unlist(strsplit(textstring, split = ""))
  widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1)
  
  if(sum(widthval) <= linewidth){return(textstring)}
  
  max <- ceiling(sum(widthval)/linewidth) #how many reps to do/substrings to complete
  out <- vector("list", max) #we are going to store the substrings in a list called out
  
  for(i in 1:max){
    lets <- unlist(strsplit(textstring, split = ""))
    widthval <- ifelse(sapply(lets, function(x) x %in% LETTERS) == TRUE, capwidth, 1)
    linelength <- cumsum(widthval) 
    if(linelength[length(linelength)] <= linewidth){
      out[[i]] <- textstring
    } else {
      index <- Position(function(x){x < linewidth}, linelength, right = TRUE)
      breaks <- grepl(separator[1], lets) | grepl(separator[2], lets) 
      if(!is.na(tol[2]) && sum(breaks[(index+1):(index+tol[2])], na.rm = TRUE) > 0){ 
        ref <- Position(function(x){!is.na(x)}, breaks[(index+1):(index+tol[2])], right = TRUE) 
        index <- Position(function(x){x == TRUE}, breaks[(index+1):(index+ref)], right = TRUE)+index-1 
      } else if(sum(breaks[(index-tol[1]):(index-1)], na.rm = TRUE) > 0){ 
        index <- index-tol[1]+Position(function(x){x == TRUE}, breaks[(index-tol[1]):(index-1)], right = TRUE)-2
      } 
      out[[i]] <- paste0(paste0(lets[1:index], collapse = ""), newline) 
      textstring <- paste0(lets[(index+1):length(lets)], collapse = "")
      textstring <- gsub(paste0("^",separator[1]), "", textstring) 
      if(!is.na(separator[2])) {
        textstring <- gsub(paste0("^",separator[2]), "", textstring)  #removing the leading space if applicable. necessary for cases w/ no good break
      }
    }
  }
  
  out <- paste0(unlist(out), collapse = "")
  
  return(out)
}


cat(linetrunc("What is the optimal length of string? I have no idea.", 30, 3))

kog_theme <-  theme(legend.position = "none",
                    legend.title = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.border = element_blank(),
                    text = element_text(size = 12, face = "bold"),
                    plot.caption = element_text(face= "italic"), #Default is hjust=1; can set to 0
                    plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
                    plot.caption.position =  "plot") #NEW parameter
