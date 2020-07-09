
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(shiny)){install.packages("shiny")}
if(!require(plotly)){install.packages("plotly")}
if(!require(wesanderson)){install.packages("wesanderson")}
if(!require(sas7bdat)){install.packages("sas7bdat")}
if(!require(haven)){install.packages("haven")}
if(!require(shinydashboard)){install.packages("shinydashboard")}
if(!require(forecast)){install.packages("forecast")}
load(Datos.Rdata)
shinyApp(ui = ui,server = server)
