# global.R

# libraries
#
if(!require(shiny)){install.packages("shiny", dependencies = TRUE);library(shiny)}
if(!require(bsplus)){install.packages("bsplus", dependencies = TRUE);library(bsplus)}
if(!require(readr)){install.packages("readr", dependencies = TRUE);library(readr)}
if(!require(tidyverse)){install.packages("tidyverse", dependencies = TRUE);library(tidyverse)}
if(!require(magrittr)){install.packages("magrittr", dependencies = TRUE);library(magrittr)}
if(!require(sf)){install.packages("sf", dependencies = TRUE);library(sf)}
if(!require(lubridate)){install.packages("lubridate", dependencies = TRUE);library(lubridate)}
if(!require(leaflet)){install.packages("leaflet", dependencies = TRUE);library(leaflet)}
if(!require(htmltools)){install.packages("htmltools", dependencies = TRUE);library(htmltools)}
if(!require(htmlwidgets)){install.packages("htmlwidgets", dependencies = TRUE);library(htmlwidgets)}
if(!require(ggplot2)){install.packages("ggplot2", dependencies = TRUE);library(ggplot2)}
if(!require(lwgeom)){install.packages("lwgeom", dependencies = TRUE);library(lwgeom)}
if(!require(RColorBrewer)){install.packages("RColorBrewer", dependencies = TRUE);library(RColorBrewer)}
if(!require(mapview)){install.packages("mapview", dependencies = TRUE);library(mapview)}
if(!require(webshot)){install.packages("webshot", dependencies = TRUE);library(webshot)}


if(!require(ggforce)){install.packages("ggforce", dependencies = TRUE);library(ggforce)}
if(!require(DT)){install.packages("DT", dependencies = TRUE);library(DT)}
if(!require(rsconnect)){install.packages("rsconnect", dependencies = TRUE);library(rsconnect)}

if(!require(igraph)){install.packages("igraph", dependencies = TRUE);library(igraph)}
if(!require(visNetwork)){install.packages("visNetwork", dependencies = TRUE);library(visNetwork)}

if(!require(scales)){install.packages("scales", dependencies = TRUE);library(scales)} #normalize vector of values
if(!require(gridExtra)){install.packages("gridExtra", dependencies = TRUE);library(gridExtra)} #grid arrange
if(!require(gtable)){install.packages("gtable", dependencies = TRUE);library(gtable)} #legend for grid.arrange
if(!require(plotly)){install.packages("plotly", dependencies = TRUE);library(plotly)} #interactive plots

if(!require(cowplot)){install.packages("cowplot", dependencies = TRUE);library(cowplot)} #interactive plots

# settings
#
enableBookmarking(store = "url")

# dir.dexter <- "/home/antonio/Desktop/New PC/Uni/phd/Computer Science for Heurmeneutics - Luxemburg/kickoff start/Lorella/tutorial/minidexter"
# setwd(dir.dexter)

# rsconnect::setAccountInfo(name='c2dh',
#                           token='C9758703BA28B09FBFCED8B01EB6F898',
#                           secret='hBMIq2CSxjHe+jHUUw6gYJXH3FKrtQdWs1jqQlVr')

# rsconnect::deployApp(dir.dexter, account="c2dh")