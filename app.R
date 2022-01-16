
library(shiny)#contruye la aplicacion
library(shinycssloaders)#complememntos adicionales para la app
library(modeest)#para estadistico moda
library(moments)#para estadistico Asimetri y curtosis
library(openxlsx)#para lectura de archivos xlsx
library(tidyverse)#para manejo de data frame
library(RTisean)#para predicciones con teoria del caos
library(tseriesChaos)#para parametros de teoria del caos
library(Metrics)#para calcular medidas de evaluacion del pronostico
library(DescTools)#
library(forecast)#para predicciones redes neuronales
library(tools)
library(RSNNS)#para entrenar redes elman
library(quantmod)
library(ggplot2)

#setTISEANpath(path = "TISEAN_Linux/Tisean_3.0.0/bin")
#setTISEANpath(path = "TISEAN_Windows/Tisean_3.0.0/bin")

{#Nombres de la Variables
    V1 <- "Temperatura Ambiental"
    V2 <- "Humedad Relativa"
    V3 <- "Presión Atmosférica"
    V4 <- "Radiación Solar Difusa"
    V5 <- "Radiación Solar Global"
    V6 <- "Temperatura Suelo a nivel 1"
    V7 <- "Temperatura Suelo a nivel 2"
    V8 <- "Temperatura Suelo a nivel 3"
    V9 <- "Temperatura Suelo a nivel 4"
    V10 <- "Temperatura Suelo a nivel 5"
    V11 <- "Temperatura Suelo a nivel 6"
    V12 <- "Temperatura Suelo a nivel 7"
    V13 <- "Precipitación de Lluvia"
    V14 <- "Recorrido del Viento"
    V15 <- "Direción del Viento"
    V16 <- "Direción racha del Viento"
    V17 <- "Hora de la racha de Viento"
    V18 <- "Minuto de la racha de Viento"
    V19 <- "Velocidad de Viento"
    V20 <- "Sensación Termica"
}


#||||||||||||||||||||||||||||||  Cargar funciones adicionales |||||||||||||||||||||||||||||||||||||||||||
AD <- function(dat){
    #|||||||||||||||||||||||||||||||||||
    suppressWarnings(suppressMessages({ 
        library(modeest)
        library(moments)
    }))
    #|||||||||||||||||||||||||||||||||||
    Mean <- round(mean(dat, na.rm = TRUE),digits = 2)
    Median <- round(median(dat, na.rm = TRUE),2)
    Mode <- round(mfv1(dat,na_rm = TRUE),2) #necesario modeest
    Stdev <- round(sd(dat, na.rm = TRUE),2)
    Kurt <- round(kurtosis(dat),2)#necesario moments
    Skew <- round(skewness(dat),2)#necesario moments
    Min <- round(min(dat),2)
    Max <- round(max(dat),2)
    Range <- round((Max - Min),2)
    Quant1 <- round(quantile(dat)[2],2)
    Quant3 <- round(quantile(dat)[4],2)
    result <- data.frame(Mean,Median,Mode,Stdev,Kurt,Skew,Min,Max,Range,Quant1,Quant3,row.names = variable.names(dat))
    return(result)
}

t_r <- function(data){
    n <- length(data)
    i <-  1
    min <- data[i]
    while(data[i] <= min) {
        min <- data[i]
        i <- i + 1
        if (i==n) {
            data[i]=10^9#se garantiza que se detenga en alguna de las n posiciones
        }
    }
    #min=valor de tiempo de retardo
    pos <- i-2
    #pos=posicion tiempo de retardo(el algoritmo se detiene en la posicion i+1 al ver
    #  que el siguiente valor es mayor al anterior. Ademas R no inicia contando el valor
    #  de la posicion cero entonces se tiene al final i+2 y por eso se resta -2 para
    #  encontrar la verdadera posicion)
    return(pos)
}

d_e <- function(data){
    data[is.na(data)] <- 1#cuando hay NA da error(se reemplaza por un maximo)
    n <- length(data)
    i <-  1
    val <- data[i]
    while(data[i] >= 0.001) {
        val <- data[i]
        i <- i + 1
        if (i==n) {
            data[i]=0#se garantiza que el while pare en alguna de las 20 posiciones
        }
    }
    #valor de tiempo de retardo
    #min
    #posicion tiempo de retardo
    r <- i
    return(r)
}

CE <- function(dat,pred){
    #|||||||||||||||||||||||||||||||||||
    suppressWarnings(suppressMessages({ 
        library(Metrics)
    }))
    #|||||||||||||||||||||||||||||||||||
    #dat <- as.numeric(dat)
    n1 <- length(dat)-744
    n2 <- n1+length(pred)
    #Medidas en escala
    MSE <- round(mse(dat[(n1+1):n2],pred),2)
    RMSE <- round(rmse(dat[(n1+1):n2],pred),2)
    MAE <- round(mae(dat[(n1+1):n2],pred),2)
    MDAE <- round(mdae(dat[(n1+1):n2],pred),2)
    MASE <- round(mase(dat[(n1+1):n2],pred),2)
    #Medidas en porcentaje
    MAPE <- round(mape(dat[(n1+1):n2],pred),4)
    #MDAPE
    #RMSPE
    #RMDSPE
    SMAPE <- round(smape(dat[(n1+1):n2],pred),4)
    #SMDAPE
    result <- data.frame(MSE,RMSE,MAE,MDAE,MASE,MAPE,SMAPE,row.names = variable.names(dat))
    return(result)
}
# ||||||||||||||||||||||||||  funciones de redes |||||||||||||||||||||||||||||||||||||||
logis <-function(dat){
    a<- max(dat)
    b<- min(dat)
    y<- (dat-b)/(a-b)
    return(y)
}

MLags <- function(bas1,p){
    for (i in 1:p) {
        x <- quantmod::Lag(bas1,k=i)
        bas1<- cbind(bas1,x)
    }
    slog <- bas1
    R<-slog[-(1:p),]
    return(R)
}


desnor <- function(pr,dat){
    minimo <- min(dat)
    maximo <- max(dat)
    s <- pr*(maximo-minimo)+minimo
    return(s)
}

# Inicio algoritmo de la aplicacion shiny
ui <- navbarPage("PREDICCIONES GEAA",
                 

                 #CARGA DE DATOS Y ANALISIS EXPLORATORIO
                 # Sidebar with a slider input for number of bins 
                 tabPanel("PROCESAMIENTO DE DATOS",
                          sidebarPanel(
                              p("NAVEGACION"),
                              #helpText("Cargar una base de datos de acuerdo
                              #al formato estandar: https://example.com"),
                              #fileInput("Carga", h3("Cargar Datos")),
                              helpText("Seleccione la base de datos con extensión .xlsx",
                                       "La base de datos debe estar imputada"
                              ),
                              
                              fileInput('file1', 'Cargar Base de Datos',
                                        accept = c(".xlsx")
                              ),
                              helpText("Unifique los datos en una sola tabla",
                              ),
                              helpText("Los datos debe ser de la misma estación"
                              ),
                              actionButton('add','Unificar Datos'),
                              checkboxInput("change", label = "Renombrar Variables", value = FALSE),
                              conditionalPanel(
                                  condition = "input.tabs=='Graficos Exploratorios'",
                                  h4("Opciones Graficas"),
                                  selectInput("var_sel1","Variable:",choices = ""), 
                                  selectInput(inputId = "graftype1",
                                              label = "Tipo de Grafico:",
                                              choices = c("Boxplot", "Linea", "Histograma"),
                                              selected = "Linea")
                                   
                              )
                              
                              #helpText("Descargar datos unificados"),
                              
                              
                              
                          ),
                          
                          mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Datos", 
                                                   p(),
                                                   downloadButton("downloadDB", "Descargar"),
                                                   p(),
                                                   withSpinner(dataTableOutput('data'))
                                                   ), 
                                          tabPanel("Detalle General", withSpinner(verbatimTextOutput("value"))),
                                          tabPanel("Estadisticas Descriptivas", 
                                                   p(),
                                                   downloadButton("downloadDes", "Descargar"),
                                                   p(),
                                                   withSpinner(dataTableOutput('table2'))
                                                   ),
                                          tabPanel("Graficos Exploratorios", withSpinner(plotOutput("plots1"))),
                                          #tabPanel("Patron de Valores Perdidos", plotOutput("Patron")),
                                          #tabPanel("Teoria del Caos", dataTableOutput('table3')),
                                          #tabPanel("Redes Neuronales", plotOutput("plots2")),
                                          #tabPanel("Evaluacion Pronosticos", dataTableOutput('table4')),
                                          id = "tabs"#nombre de estos subpaneles
                              )
                          )
                 ),
#|||||||||||||||||||||  TEORIA DEL CAOS   ||||||||||||||||||||||||||||||
                 tabPanel("TEORIA DEL CAOS",
                          sidebarPanel(
                              p("PREDICCIONES"),
                              
                              helpText("El tiempo de calculo puede variar segun la cantidad de datos"
                              ),
                              selectInput("var_sel2","Variable:",choices = ""
                              ),
                              helpText("Los datos debe ser de la misma estación"),
                              helpText("Pulse 'Comenzar Predicción' para inicial 
                                       el algoritmo"),
                              actionButton('PredTC','Comenzar Predicción'),
                              sliderInput("slider1", label = h3("Días maximos de pronóstico"), min = 1, 
                                          max = 14, value = 14)
                              #helpText("Descargar datos unificados")
                           
                              
                          ),
                          
                          mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("Resultados", 
                                                   p(),
                                                   downloadButton("downloadpredTC", "Descargar"),
                                                   p(),
                                                   withSpinner(dataTableOutput('ResTC'))
                                                   ), 
                                          tabPanel("Grafico", withSpinner(plotOutput("plotsTC"))),
                                          tabPanel("Estadisticos Descriptivos", 
                                                   p(),
                                                   downloadButton("downloadDesTC", "Descargar"),
                                                   p(),
                                                   withSpinner(dataTableOutput('DesTC'))
                                                   ),
                                          tabPanel("Medidas de Evaluacion", 
                                                   p(),
                                                   downloadButton("downloadMEPTC", "Descargar"),
                                                   p(),
                                                   withSpinner(dataTableOutput('MEPTC'))
                                                   ),
                                          #tabPanel("COntrol de Salidas", verbatimTextOutput("salTC")),
                                          #tabPanel("Redes Neuronales", plotOutput("plots2")),
                                          #tabPanel("Evaluacion Pronosticos", dataTableOutput('table4')),
                                          id = "tabs2"#nombre de estos subpaneles
                              )
                          ),
                         
                 ),
#||||||||||||||||||||||| REDES NERUONALES       ||||||||||||||||||||||||||||||||
                tabPanel("REDES NEURONALES",
                         sidebarPanel(
                             p("PREDICCIONES"),
                            
                             helpText("El tiempo de calculo puede variar segun la cantidad de datos"
                             ),
                             selectInput("var_sel3","Variable:",choices = ""
                             ),
                             helpText("Los datos debe ser de la misma estación"),
                             helpText("Pulse 'Comenzar Predicción' para inicial 
                                       el algoritmo"),
                             actionButton('PredRN','Comenzar Predicción'),
                             sliderInput("slider2", label = h3("Días maximos de pronóstico"), min = 1, 
                                         max = 14, value = 14)
                            #helpText("Descargar datos unificados")

                             
                         ),
                         
                         mainPanel(
                             tabsetPanel(type = "tabs",
                                         tabPanel("Resultados", 
                                                  p(),
                                                  downloadButton("downloadpredRN", "Descargar"),
                                                  p(),
                                                  withSpinner(dataTableOutput('ResRN'))
                                                  ), 
                                         tabPanel("Grafico", withSpinner(plotOutput("plotsRN"))),
                                         tabPanel("Estadisticos Descriptivos", 
                                                  p(),
                                                  downloadButton("downloadDesRN", "Descargar"),
                                                  p(),
                                                  withSpinner(dataTableOutput('DesRN'))
                                                  ),
                                         #tabPanel("Control sal", verbatimTextOutput('control')),
                                         tabPanel("Medidas de Evaluacion", 
                                                  p(),
                                                  downloadButton("downloadMEPRN", "Descargar"),
                                                  p(),
                                                  withSpinner(dataTableOutput('MEPRN'))
                                         ),
                                         #tabPanel("COntrol de Salidas", verbatimTextOutput("salRN")),
                                         #tabPanel("Redes Neuronales", plotOutput("plots2")),
                                         #tabPanel("Evaluacion Pronosticos", dataTableOutput('table4')),
                                         id = "tabs3"#nombre de estos subpaneles
                             )
                         ),
                         
                )
                 
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    rv <- reactiveValues()
    rv2 <- reactiveValues()

    
    Carga <- reactive({
        file <- input$file1#boton cargar bases
        ext <- tools::file_ext(file$datapath)
        req(file)#si se cargo una base, entonces se ejecutan las demas lineas siguientes
        validate(need(ext == "xlsx", "Solo se admiten archivos .xlsx"))
        DB <- read.xlsx(xlsxFile = file$datapath,
                        sheet = 1, detectDates = TRUE)
        colnames(DB)[1] <- "N°"
        DB[,2] <- as.Date(DB[,2])

        DB
        
        
    })
    
    
    
    observeEvent(input$add, {
        #if(is.null(rv$data)){
        #  rv$data[[length(rv$data) + 1]]  <- Carga()
        #names(rv$data) <- input$solvent
        #}
        
        rv$data[[length(rv$data) + 1]] <- Carga()
        rv2$data[[length(rv2$data) + 1]] <- Carga()
    })
    
    
    observeEvent(input$change, {
        bases <- rv$data

        if (input$add>0) {
            if (input$change==TRUE) {
                for (i in 1:length(bases)) {
                    pnames <- names(bases[[i]][])
                    for (j in 4:length(pnames)) {
                        pnames[j] <- switch(pnames[j],
                                            "X1"=V1, "X2"=V2, "X3"=V3, "X4"=V4,
                                            "X5"=V5, "X6"=V6, "X7"=V7,"X8"=V8,
                                            "X9"=V9,"X10"=V10, "X11"=V11, "X12"=V12,
                                            "X13"=V13, "X14"=V14, "X15"=V15, "X16"=V16,
                                            "X17"=V17, "X18"=V18, "X19"=V19, "X20"=V20,
                                            pnames[j])
                    }
                    chan <- bases[[i]][]
                    names(chan) <- pnames
                    bases[[i]] <- chan
                }
                rv$data <- bases 
            }else{
                rv$data <-  rv2$data
                
            }
        }
        

    })
    
  
    
    #Cuando las variables no tienen el mismo número de observaciones o si se tiene bases de datos con diferente numero de variables
    #Pueden existir inconvenientes  cuando se quiere realizar operaciones con tablas tipo dataframe
    #se puede añadir NA's para balancear y completar la tabla pero se generan errores si se realizan calculos con NA's
    #Se decide crear de una lista que contiene todas las variables por separado, de esta manera los datos 
    #no necesitan NA para balancearlos como en un datframe y se usa una variable a la vez para facilitar los calculos 
    DB_c <- reactive({
        #se nombra a la lista de todas las bases ingresadas del usuario
        Lista_Bases <- rv$data
        invar <- 4  #la columna 4 es donde inicia siempre los datos de la primera variable
        
        #se filtra cada elemento de Lista_Bases eliminando las 4 primeras columnas
        for (i in 1:length(Lista_Bases)) {
            #[[i]] = i-esimo elemento de Lista_Bases (cada elemento es de tipo dataframe)
            #[,invar:length(Lista_Bases[[i]])] = filtra los datos para cada dataframe desde la 4ta columna en adelante
            Lista_Bases[[i]] <- Lista_Bases[[i]][,invar:length(Lista_Bases[[i]])]
        }
        
        df_Variables <- c()
        for (i in 1:length(Lista_Bases)) {
            #se unen todas las Bases por fila (las filas diferentes, mismas variables en columna)
            df_Variables <- bind_rows(df_Variables,Lista_Bases[[i]])
        }
        #nota: se recomienda bind_rows en vez de rbind ya que es mejor cuando hay bases con diferente número de columnas(añade NA si no existe datos y se mantiene la estructura de un dataframe) 
        
        #se vuelve a transformar el dataframe a una lista
        Lista_Variables <- as.list(df_Variables)
        #se eliminan los NA de cada variable
        for (i in 1:length(Lista_Variables)) {
            #[[i]] = i-esimo elemento de Lista_Variables 
            #[is.na(Lista_Variables[[i]])==FALSE] = Selecciona todos los datos que no sean NA
            Lista_Variables[[i]] <- Lista_Variables[[i]][is.na(Lista_Variables[[i]])==FALSE]
        }
        #Se imprime la Lista con las variables para ser usadas
        Lista_Variables
    })
    
    #dataframe parcial para obtener los nombres de las variables que se ingresaron
    partial_df <-  reactive({
        Lista_Bases <-  rv$data
        invar <- 4
        for (i in 1:length(Lista_Bases)) {
            Lista_Bases[[i]] <- Lista_Bases[[i]][,invar:length(Lista_Bases[[i]])]
        }
        Unificar <- c()
        for (i in 1:length(Lista_Bases)) {
            Unificar <- bind_rows(Unificar,Lista_Bases[[i]])
        }
        Unificar
        
    })
    
    #dataframe total para imprimir como tabla y descargar 
    complete_df <-  reactive({
        #Lista_Bases <- DB_c()
        Lista_Bases <- rv$data
        Unificar <- c()
        for (i in 1:length(Lista_Bases)) {
            Unificar <- bind_rows(Unificar,Lista_Bases[[i]])
        }
        Unificar
        
    })
    
    #list_var <- reactiveValues()
    #list_var$data <- DB_c()
    
    observe({
        #rv$data
        #nms_cont <- nam_var()
        names1 <- names(rv$data)
       
        
        #un vector de nombres de las variables olo funciona con tablas
        nms_cont <- names(Filter(function(x) is.integer(x) ||
                                     is.numeric(x) ||
                                     is.double(x),
                                     partial_df()))
        
        avail_con <-
            if (identical(nms_cont, character(0)))
                c("No continuous vars available" = ".")
        else c(nms_cont)
        sel <- c()
        for (i in 1:length(avail_con)) {
            sel[i] <- switch(avail_con[i],
                             "X1"=V1, "X2"=V2, "X3"=V3, "X4"=V4,
                             "X5"=V5, "X6"=V6, "X7"=V7,"X8"=V8,
                             "X9"=V9,"X10"=V10, "X11"=V11, "X12"=V12,
                             "X13"=V13, "X14"=V14, "X15"=V15, "X16"=V16,
                             "X17"=V17, "X18"=V18, "X19"=V19, "X20"=V20)
        }
        #sel
        updateSelectInput(session, "var_sel1", choices = avail_con)
        updateSelectInput(session, "var_sel2", choices = avail_con)
        updateSelectInput(session, "var_sel3", choices = avail_con)
        #updateSelectInput(session, "var_sel3", choices = avail_con)
        
    })
    



    
    output$data <- renderDataTable({
        if (is.null(input$file1)) {
            return(data.frame(x = "Cargue un archivo .xlsx"))
        } else if (input$add == 0) {
            return(data.frame(x = "Presione el boton 'Unificar Datos'"))
        } else {
            complete_df()
        }
        
    })
    
    output$data3 <- renderPrint({
        #Lista_Bases <- DB_c()
        Lista_Bases <- rv$data
        Lista_Bases
        #tail(Lista_Variables,30)
    })
    
    output$value <- renderPrint({
        #Lista_Bases <- DB_c()
        Lista_Bases <- rv$data
        Lista_Variables <- c()
        for (i in 1:length(Lista_Bases)) {
            Lista_Variables <- bind_rows(Lista_Variables,Lista_Bases[[i]])
        }
        Lista_Variables
        str(Lista_Variables) 
    })
    
    
    #Calculo Estadisticas descriptivas
    est_des <- reactive({
        Lista_Variables <- DB_c()
        est_des <- lapply(Lista_Variables, AD)
        est_des <- data.frame(t(sapply(est_des,c)))
        Lista_Variables <- est_des
        Variable <- row.names(Lista_Variables)
        Lista_Variables <- cbind(Variable,Lista_Variables)
        Lista_Variables
    })
    
    #Salida Estadísticas Descriptivas
    output$table2 <- renderDataTable({
        Lista_Variables <- est_des()
        Lista_Variables
    })
    
 
    output$plots1 <- renderPlot({
        nam <- input$var_sel1
        Reales <- DB_c()[[nam]]
        max <- length(Reales)
        x <- 1:max
        res <- data.frame(x,Reales)
        df <- res %>%
            gather(key = "variable", value = "value", -x)
        
        if (input$graftype1 == "Histograma") {
            p <- ggplot(df, aes(x = value)) + 
                geom_histogram(alpha=0.6)  +
                theme(legend.position="none",
                    panel.spacing = unit(0.1, "lines"),
                    strip.text.x = element_text(size = 8)
                ) + theme_minimal() +
                labs(title= nam,
                     y="Frecuencia", x = "")+
                theme(text = element_text(size=12),
                      axis.text.x = element_text(size=rel(1.3)),
                      axis.text.y = element_text(size=rel(1.3)),
                      axis.title.x = element_text(face="bold",size=rel(1.2)),
                      axis.title.y = element_text(face="bold",size=rel(1.1)),
                      title = element_text(face="bold",size=rel(1.1)),
                      legend.title = element_text(size=rel(1)),
                      legend.text = element_text(size=rel(1)),
                      plot.margin = unit(c(1,1,1,1), "cm")
                )
        }else{
            if (input$graftype1 == "Linea") {
                p <- ggplot(df, aes(x = x, y = value)) +
                    geom_line(aes(color = variable), size = 1) +
                    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
                    labs(title= nam,
                         y=nam, x = "Horas")+
                    theme_minimal()+
                    theme(text = element_text(size=12),
                          legend.position="none",
                          axis.text.x = element_text(size=rel(1.3)),
                          axis.text.y = element_text(size=rel(1.3)),
                          axis.title.x = element_text(face="bold",size=rel(1.2)),
                          axis.title.y = element_text(face="bold",size=rel(1.1)),
                          title = element_text(face="bold",size=rel(1.1)),
                          #legend.title = element_text(size=rel(1)),
                          #legend.text = element_text(size=rel(1)),
                          plot.margin = unit(c(1,1,1,1), "cm")
                    )
                  
            }else{
                p <- ggplot(df, aes(y = value)) + 
                    geom_boxplot() +
                    labs(title= nam,
                         y=nam, x = "")+
                    theme_minimal()+
                    theme(text = element_text(size=12),
                          axis.text.x = element_text(size=rel(1.3)),
                          axis.text.y = element_text(size=rel(1.3)),
                          axis.title.x = element_text(face="bold",size=rel(1.2)),
                          axis.title.y = element_text(face="bold",size=rel(1.1)),
                          title = element_text(face="bold",size=rel(1.1)),
                          legend.title = element_text(size=rel(1)),
                          legend.text = element_text(size=rel(1)),
                          plot.margin = unit(c(1,1,1,1), "cm")
                    )
            }
        }

        p
    })
    
    output$downloadDB <- downloadHandler(
        filename = function() {
            paste("Data_Unificada-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write.xlsx(complete_df(), file)
        }
    )
    
    output$downloadDes <- downloadHandler(
        filename = function() {
            paste("Estadisticos_Descriptivos-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write.xlsx(est_des(), file)
        }
    )
    
    #||||||||||||||||||||||||||||  TERIA DEL CAOS  |||||||||||||||||||||||||||||||||||||
   
    mut_var <- reactive({
        vars <- DB_c()
        n <- input$var_sel2
        Lista_Variables <- vars[[n]]
        Mutual_var <- mutual(Lista_Variables,partitions =20,plot=FALSE,lag.max = 24)
        Mutual_var
    })
    
    parametro_d <- reactive({
        d1 <- t_r(mut_var())
        d1
    })
    
    fnn <- reactive({
        vars <- DB_c()
        n <- input$var_sel2
        d1 <- parametro_d()
        Lista_Variables <- vars[[n]]
        fn1 <- false.nearest(Lista_Variables,m=24, d=d1, t=0, rt=10,eps=1)
        fn1
    })
    
    parametro_m <- reactive({
        fn <- fnn()
        m1 <- d_e(fn[1,])
        m1
    })
    
    pred_TC <- reactive({
        nam <- input$var_sel2
        Lista_Variables <- DB_c()
        d1 <- parametro_d()
        m1 <- parametro_m()
        sel <- input$slider1
        #sel <- 14#nput$timpred
        thp=24*sel#1 dia minimo y 14 dias maximo(2 semanas)
        #thp=336#2 semanas
        #n1 <- length(Lista_Variables[[nam]])-thp
        n1 <- length(Lista_Variables[[nam]])-thp
        rbf1 <- RTisean::rbf(Lista_Variables[[nam]],m = m1,d = d1,p = 10,s = 1,n=n1,L=thp)
        #pred1 <- data.frame(rbf1$pred)
        pred1 <- rbf1$pred
        colnames(pred1) <- nam
        pred1
    })
   
    tab_pred_TC <- reactive({
        pred <- pred_TC()
        df <- data.frame(pred)
        df
    })
    
    des_TC <- reactive({
        res <- pred_TC()
        est_des <- AD(res)
        nam <- input$var_sel2
        tabla <- data.frame(Variable=nam,est_des)
        tabla
    })
    
    output$ResTC <- renderDataTable({
        if (is.null(input$file1)) {
            return(data.frame(x = "Regrese a la ventana Procesamiento de datos y Cargue un archivo .xlsx"))
        } else if (input$PredTC == 0) {
            return(data.frame(x = "Presione el boton 'Comenzar Prediccion'"))
        } else {
            tab_pred_TC()
        }
        
    })
    
    output$DesTC <- renderDataTable({
        if (is.null(input$file1)) {
            return(data.frame(x = "Regrese a la ventana Procesamiento de datos y Cargue un archivo .xlsx"))
        } else if (input$PredTC == 0) {
            return(data.frame(x = "Presione el boton 'Comenzar Prediccion'"))
        } else {
            des_TC()
        }
        
    })
    
    output$plotsTC <- renderPlot({
        nam <- input$var_sel2
        Observados <- DB_c()[[nam]]
        Pronósticos <- pred_TC()[,1]#quitar tabla
        input <- input$slider1
        #input <- 14
        max <- 24*input
        x <- 1:max
        n1 <- length(Observados)-744
        n2 <- n1+length(Pronósticos)
        Observados <- Observados[(n1+1):n2]
        res <- data.frame(x,Observados,Pronósticos)
        df1 <- res %>%
            gather(key = "variable", value = "value", -x)
        ggplot(df1, aes(x = x, y = value)) +
            geom_line(aes(color = variable), size = 1) +
            scale_color_manual(values = c("#00AFBB", "#E7B800")) +
            labs(title= "Observados vs Pronósticos",
                 y=nam, 
                 x = "Horas")+
            theme_minimal()+
            #theme_void()+
            theme(text = element_text(size=12),
                  axis.text.x = element_text(size=rel(1.3)),
                  axis.text.y = element_text(size=rel(1.3)),
                  axis.title.x = element_text(face="bold",size=rel(1.2)),
                  axis.title.y = element_text(face="bold",size=rel(1.1)),
                  title = element_text(face="bold",size=rel(1.1)),
                  legend.title = element_text(size=rel(1)),
                  legend.text = element_text(size=rel(1)),
                  plot.margin = unit(c(1,1,1,1), "cm")
            )
    })
    
    mep_TC <- reactive({
        data <- DB_c()
        pred <- pred_TC()
        nam <- input$var_sel2
        cal <- CE(data[[nam]],pred)
        res <- data.frame(Variable=nam,cal)
        res
    })
    
    output$MEPTC <- renderDataTable({
        mep_TC()
    })
    
    output$downloadpredTC <- downloadHandler(
        filename = function() {
            paste("Predicciones_TC-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write.xlsx(tab_pred_TC(), file)
        }
    )
    
    output$downloadDesTC <- downloadHandler(
        filename = function() {
            paste("Descriptivo_TC-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write.xlsx(des_TC(), file)
        }
    )
    
    output$downloadMEPTC <- downloadHandler(
        filename = function() {
            paste("MEP_TC-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write.xlsx(mep_TC(), file)
        }
    )
    
    #||||||||||||||||||||||||||| REDES NEURONALES  |||||||||||||||||||||||||||||||||||||||||
    trans_log <- reactive({
        data <- DB_c()
        data2 <- data[[input$var_sel3]]
        data2 <- logis(data2)
        data2
    })
    
    train <- reactive({
        slog <- trans_log()
        lag <- 24
        input <- input$slider2
        #input <- 14
        npron <- lag*input
        train <- 1:(length(slog)-(npron+lag))
        train
    })
    
    mret <- reactive({
        slog <- trans_log()
        #train <- train()
        y<-as.zoo(slog) 
        lag <- 24
        Mret <- MLags(bas1 = y,p = lag)
        Mret
    })
    
    fit_red <- reactive({
         slog <- mret()
         train <- train()
        inputs<-slog[,2:25]
        outputs<-slog[,1]
        fit<-elman(inputs[train],
                   outputs[train],
                   size=c(3,2),
                   learnFuncParams=c(0.07),
                   maxit=2000)
        fit
    })

    pred_RN <- reactive({
        reales <- DB_c()
        fit <- fit_red()
        nam <- input$var_sel3
        train <- train()
        inputs<-mret()[,2:25]
        pred<-predict(fit,inputs[-train])
        pred <- desnor(pred,reales[[nam]])
        colnames(pred) <- nam
        pred
    })
    
 
    
    tab_pred_RN <- reactive({
        pred <- pred_RN()
        df <- data.frame(pred)
        df
    })
    
    output$ResRN <- renderDataTable({
        if (is.null(input$file1)) {
            return(data.frame(x = "Regrese a la ventana Procesamiento de datos y Cargue un archivo .xlsx"))
        } else if (input$PredRN == 0) {
            return(data.frame(x = "Presione el boton 'Comenzar Prediccion'"))
        } else {
            tab_pred_RN()
        }
        
    })
    
    output$plotsRN <- renderPlot({
        nam <- input$var_sel3
        Observados <- DB_c()[[nam]]
        Pronósticos <- pred_RN()[,1]
        input <- input$slider2
        #input <- 14
        max <- 24*input
        x <- 1:max
        n1 <- length(Observados)-744
        n2 <- n1+length(Pronósticos)
        Observados <- Observados[(n1+1):n2]
        res <- data.frame(x,Observados,Pronósticos)
        df1 <- res %>%
            gather(key = "variable", value = "value", -x)
        ggplot(df1, aes(x = x, y = value)) +
            geom_line(aes(color = variable), size = 1) +
            scale_color_manual(values = c("#00AFBB", "#E7B800")) +
            labs(title= "Observados vs Pronósticos",
                 y=nam, x = "Horas")+
            theme_minimal()+
            theme(text = element_text(size=12),
                  axis.text.x = element_text(size=rel(1.3)),
                  axis.text.y = element_text(size=rel(1.3)),
                  axis.title.x = element_text(face="bold",size=rel(1.2)),
                  axis.title.y = element_text(face="bold",size=rel(1.1)),
                  title = element_text(face="bold",size=rel(1.1)),
                  legend.title = element_text(size=rel(1)),
                  legend.text = element_text(size=rel(1)),
                  plot.margin = unit(c(1,1,1,1), "cm")
            )
    })
    
    des_RN <- reactive({
        res <- pred_RN()
        est_des <- AD(res)
        nam <- input$var_sel3
        tabla <- data.frame(Variable=nam,est_des)
        tabla
    })
    
    mep_RN <- reactive({
        data <- DB_c()
        pred <- pred_RN()
        nam <- input$var_sel3
        sol <- CE(data[[nam]],pred)
        res <- data.frame(Variable=nam,sol)
        res  
    })
 
    output$DesRN <- renderDataTable({
        if (is.null(input$file1)) {
            return(data.frame(x = "Regrese a la ventana Procesamiento de datos y Cargue un archivo .xlsx"))
        } else if (input$PredRN == 0) {
            return(data.frame(x = "Presione el boton 'Comenzar Prediccion'"))
        } else {
            des_RN()
        }
        
    })
    
    output$MEPRN <- renderDataTable({
        mep_RN()
    })
    
    output$downloadpredRN <- downloadHandler(
        filename = function() {
            paste("Predicciones_RN-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write.xlsx(tab_pred_RN(), file)
        }
    )
    
    output$downloadDesRN <- downloadHandler(
        filename = function() {
            paste("Descriptivo_RN-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write.xlsx(Des_RN(), file)
        }
    )
    
    output$downloadMEPRN <- downloadHandler(
        filename = function() {
            paste("MEP_RN-", Sys.Date(), ".xlsx")
        },
        content = function(file) {
            write.xlsx(mep_RN(), file)
        }
    )
    

}

# Run the application 
shinyApp(ui = ui, server = server)
