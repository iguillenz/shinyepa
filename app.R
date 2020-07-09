require(ggplot2)
require(shiny)
require(plotly)
require(wesanderson)
require(sas7bdat)
require(haven)
require( shinydashboard)
require(forecast)
shinyApp(ui = ui,server = server)

tnames<-sort(fechas)
index<-read_sas('index.sas7bdat')

index<-cbind(index, fecha=c(tnames,'2020T1'))
indexnames<-names(index)[grep(names(index),pattern = 'i')]
graficos<-c('Tasas de paro', 'Formacion', 'Abandono estudios')
glabels<-c(  'Juvenil 16-24', 'Mayores 55-64',
             'Estudios superiores 30-34','Adultos en formacion 25-64',
              'Abandono temprano 18-24','Ni estudian ni trabajan 20-34')
tecnicas<-c("Desestacionalizacion","Auto-ARIMA","Prediccion")
nombres<-c('Tasa de paro juvenil ','Tasa de paro trabajadores mayores',
           'Jovenes con estudios superiores', 'Adultos en formacion continua',
           'Abandono temprado de la educacion', 'Jovenes que ni estudian ni trabajan')
indexnames<-names(index)[grep(names(index),pattern = "i")];
graficos<-c("Tasas de paro", "Formacion",'Formacion', "Abandono estudios");
titulo<-c("Tasas de Paro","Jovenes con estudios superiores","Adultos en formacion","Abandono de estudios");
glabels<-c("Juvenil 16-24", "Mayores 55-64",
           "Estudios superiores 30-34","Adultos en formacion 25-64",
           "Abandono temprano 20-24","Ni estudian ni trabajan 16-24");
subt<-c('% Sobre el total del grupo de edad, y de la poblacion activa', '% Sobre el total del grupo de edad', '% Sobre el total del grupo de edad','% Sobre el total del grupo de edad')


ui <- fluidPage(  

  dashboardPage(  
    dashboardHeader(title = "Aplicacones shiny sobre la EPA"),
    dashboardSidebar(
      selectInput("var_y", "Series temporales de algunos indices y tasas sobre el empleo", choices = titulo),
      checkboxGroupInput("var_tec","Tecnica a utilizar:",tecnicas)
    ),
  dashboardBody(
#ayuda para la amplitud de la pagina https://stackoverflow.com/questions/47784427/shiny-how-to-center-and-fix-width-of-dashboard
      tags$style(
        "body{
    min-height: auto;
    height: auto;
    max-width: 1600px;
    margin: auto;
        }"
      )
    ,
    plotlyOutput("distPlot"),
    
splitLayout(
    plotlyOutput("detrend1"),plotlyOutput("detrend2")),
splitLayout(
    plotOutput("autoarima1"),plotOutput("autoarima2")),
splitLayout(
    plotlyOutput("pred1"),plotlyOutput("pred2"))
    
    
    )))



server <- function(input, output) {

  
  output$distPlot <- renderPlotly({
    req(input$var_y)
    titulo_ind<-which(titulo==input$var_y)
    rango<-switch(titulo_ind,1,3,4,5)
    ind1<-indexnames[rango]
    ind2<-indexnames[rango+1]
    auxlab1<-which(indexnames==ind1)
    
    auxlab2<-which(indexnames==ind2)
    if(titulo_ind==2 | titulo_ind==3){
      p<-ggplot(data = index,aes(fecha))+
        geom_line(aes_(y= index[[ind1]]*100,group=1,colour=glabels[auxlab1]),size=.75)+
        geom_point(aes_(y= index[[ind1]]*100,group=1,colour=glabels[auxlab1]),size=1)+
        scale_x_discrete(breaks= index$fecha[ grepl(".T1",x = index$fecha)],labels = 2005:2020)+
        theme_light()+
        theme(axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5),
              legend.background = element_rect(linetype = 1,size = .25,colour = 1),
              axis.text = element_text(colour = 1,size = 12),
              legend.position = "right",
              panel.border = element_rect(colour = "black",fill = NA))+
        labs(caption=" ",subtitle = " ",x="",y="")+      
        scale_color_manual(" ",values =wes_palette(name = "Darjeeling1",n = 2,type = "dis"))
      pplotly<-ggplotly(p)
      pplotly<-pplotly%>%layout(title=(text=paste0("<b>",titulo[titulo_ind],"</b><br><sup>",subt[titulo_ind])),
                                legend=list(title=list(text=paste0("<b>",graficos[2],"</b>"))),
                                xaxis=list( title = "<sup>Fuente: INE y Eurostat</b>"), margin=list(l=50,r=50,t=50,b=100))
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "colour", replacement = "Serie")
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "y", replacement = "Valor indice")
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "fecha", replacement = "Fecha")
      
    }else{
      p<-ggplot(data = index,aes(fecha))+
        geom_line(aes_(y= index[[ind1]]*100,group=1,colour=glabels[auxlab1]),size=.75)+
        geom_point(aes_(y= index[[ind1]]*100,group=1,colour=glabels[auxlab1]),size=1)+
        geom_line(aes_(y= index[[ind2]]*100, group=2,colour=glabels[auxlab2]),size=.75)+
        geom_point( aes_(y=index[[ ind2]]*100, group=2,colour=glabels[auxlab2]),size=1)+
        scale_x_discrete(breaks= index$fecha[ grepl(".T1",x = index$fecha)],labels = 2005:2020)+
        theme_light()+
        theme(axis.text.x = element_text(angle = 90,size = 12 ,vjust = .5),
              legend.background = element_rect(linetype = 1,size = .25,colour = 1),
              axis.text = element_text(colour = 1,size = 12),
              legend.position = "right",
              panel.border = element_rect(colour = "black",fill = NA))+
        labs(caption=" ",subtitle = " ",x="",y="")+      
        scale_color_manual(" ",values =wes_palette(name = "Darjeeling1",n = 2,type = "dis"))
      pplotly<-ggplotly(p)
      pplotly<-pplotly%>%layout(title=(text=paste0("<b>",titulo[titulo_ind],"</b><br><sup>",subt[titulo_ind])),
                                legend=list(title=list(text=paste0("<b>",graficos[titulo_ind],"</b>"))),
                                xaxis=list( title = "<sup>Fuente: INE y Eurostat</b>"), margin=list(l=50,r=50,t=50,b=100))
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "colour", replacement = "Serie")
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "y", replacement = "Valor indice")
      pplotly$x$data[[1]]$text<-gsub(pplotly$x$data[[1]]$text,pattern = "fecha", replacement = "Fecha")
      pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "colour", replacement = "Serie")
      pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "y:", replacement = "Valor indice:")
      pplotly$x$data[[2]]$text<-gsub(pplotly$x$data[[2]]$text,pattern = "fecha", replacement = "Fecha")
    }
    pplotly
  })

  
  output$detrend1<-renderPlotly({
    req(input$var_y)
    if (tecnicas[1]%in%input$var_tec) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      auxlab1<-which(indexnames==ind1)
      

      entero<-ts(index[ind1][,1],start = 2005,frequency = 4)
      descomp<-stl(entero,4)
      ajus<-seasadj(descomp)

        p<-ggplotly(autoplot(ajus,xlab='',ylab='',main= ""))
        p%>%layout(title=(text=paste0("<b>",nombres[auxlab1],"</b><br><sup>")),margin=list(
          l = 50,r = 50,  b = 100,t = 80,pad = 4))
      }
      })
  output$detrend2<-renderPlotly({
    req(input$var_y)
    if (tecnicas[1]%in%input$var_tec & input$var_y%in%titulo[c(1,4)]) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      auxlab1<-which(indexnames==ind1)
      auxlab2<-which(indexnames==ind2)
      
      
      entero<-ts(index[ind2][,1],start = 2005,frequency = 4)
      descomp<-stl(entero,4)
      ajus<-seasadj(descomp)
      
      p<-ggplotly(autoplot(ajus,xlab='',ylab='',main= ""))
      p%>%layout(title=(text=paste0("<b>",nombres[auxlab2],"</b><br><sup>")),margin=list(
        l = 50,r = 50,  b = 100,t = 80,pad = 4))
    }
  })
  
  
  output$autoarima1<-renderPlot({
    req(input$var_y)
    if (tecnicas[2]%in%input$var_tec) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      
      entero<-ts(index[ind2][,1],start = 2005,frequency = 4)
      recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
      au<-auto.arima(recor)
      checkresiduals(au)
    }
  })
  output$autoarima2<-renderPlot({
    req(input$var_y)
    if (tecnicas[2]%in%input$var_tec & input$var_y%in%titulo[c(1,4)]) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      
      
      entero<-ts(index[ind2][,1],start = 2005,frequency = 4)
      recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
      au<-auto.arima(recor)
      checkresiduals(au)
    }
  })
  
  output$pred1<-renderPlotly({
    req(input$var_y)
    if (tecnicas[3]%in%input$var_tec) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      
      entero<-ts(index[ind1][,1],start = 2005,frequency = 4)
      recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
      au<-auto.arima(recor)
      
      z<-au%>%forecast(h=4)
      tz<-ts(cbind(as.data.frame(z)),start = 2020,frequency = 4)
      grf<-autoplot(entero, ylab = '')+
      autolayer(tz)
      ggplotly(grf)

    }
  })
  output$pred2<-renderPlotly({
    req(input$var_y)
    if (tecnicas[3]%in%input$var_tec & input$var_y%in%titulo[c(1,4)]) {
      titulo_ind<-which(titulo==input$var_y)
      rango<-switch(titulo_ind,1,2,4,5)
      ind1<-indexnames[rango]
      ind2<-indexnames[rango+1]
      
      entero<-ts(index[ind2][,1],start = 2005,frequency = 4)
      recor<-ts(entero[time(entero)<2020],start = 2005, frequency = 4)
      au<-auto.arima(recor)
      
      z<-au%>%forecast(h=4)
      tz<-ts(cbind(as.data.frame(z)),start = 2020,frequency = 4)
      grf<-autoplot(entero, ylab = '')+
        autolayer(tz)
      ggplotly(grf)

    }
  })
  
  
  
}
shinyApp(ui = ui,server = server)

