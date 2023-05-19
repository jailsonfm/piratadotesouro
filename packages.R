# Pacotes R ---- 
#Packages list
pkgList <- c("tidyverse", "GetTDData", "lubridate",
             "extrafont", "ggrepel", "ggthemes", "plotly",
             "formattable", "data.table", "sidrar",
             "shiny", "plotly")
# Install packages if not already installed
newPkgs <- pkgList[!(pkgList %in% 
                       installed.packages()[,"Package"])]
if(length(newPkgs)) install.packages(newPkgs)
# Loading packages if not already loaded
lapply(pkgList, require, character.only = TRUE) 