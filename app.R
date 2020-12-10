################################################################################
# Trabalho Prático: Shiny                                                      #
# Disciplina: ATD [MEGAF]                                                      #
# Docente: Nuno Lavado - nlavado@isec.pt                                       #
#                                                                              #
# Grupo:  João Gonçalves  [2020149226]  - a2020149226@isec.pt                  #
#         Luís Pato       [2005009772]  - a21150211@isec.pt                    #
#         Samuel Martinho [2006005673]  - a21170106@isec.pt                    #
#                                                                              #
# Data:   2020/11/21      Data última revisão:  2020/11/25 - LP                #
#                                                                              #
################################################################################

library(shiny)


####################################
### Variáveis                   ####
####################################

# Data Frame dos Dados da Rede
dfDados <<- data.frame()   





####################################
### Funcões                     ####
####################################

# Lê os números delimitados por vírgulas das InputBox 
ler_nr <- function(tI_nr, df_rw) {
  
  lnr <- matrix(as.numeric(unlist(strsplit(tI_nr,","))),
                1,
                length(unlist(strsplit(tI_nr,","))),
                dimnames = list(df_rw,1:length(unlist(strsplit(tI_nr,","))))
                )

  return(lnr)
}


# Aumenta as colunas de uma linha para o número máximo de colunas da tabela
aum_col <- function(ncolMax, tI_colMin) {
  
  if (ncolMax == ncol(tI_colMin))
    acl <- tI_colMin
  else{
    aux <- cbind(tI_colMin,
                 matrix(t(vector(mode = "integer", ncolMax-ncol(tI_colMin))),
                        nrow = 1,
                        dimnames = (list(nrow(tI_colMin),(ncol(tI_colMin)+1):ncolMax))
                        )
                 )
    
    row.names(aux) <- c(row.names(tI_colMin))
    
    acl <- aux
  }
  
  return(acl)
  
}


# Adiciona linha na tabela
adc_lin <- function(nr_lin, txtImp) {
  

  return(aln)
}


################################################################################
# Define UI                                                               ######
################################################################################
ui <- fluidPage(

    # Application title
    # titlePanel("MEGAF"),
    # titlePanel("An application title to display", windowTitle = "The title that should be displayed by the browser window."),
    
    navbarPage("MEGAF",
        tabPanel("ATD"),
        tabPanel("GE"),
        tabPanel("GF"),
        tabPanel("LOG"),
        tabPanel("MAD",
            h2("Forma dos Arcos Emergentes (FSF - Forward Star Form)"),
            sidebarLayout(
                sidebarPanel(
                    textInput("point",HTML("point<span style=\"color:red\">*</span>") ,placeholder = "0,1,10,25"),
                    textInput("suc", HTML("suc<span style=\"color:red\">*</span>"), placeholder = "0,1,10,25"),
                    textInput("dist", HTML("dist<span style=\"color:red\">*</span>"), placeholder = "0,1,10,25"),
                    textInput("capacidade", "capacidade", placeholder = "0,1,10,25"),
                    HTML(paste("<p style=\"color:red\">*Preenchimento obrigatório!</p>" ,strong("Nota:"), "Números inteiros positivos (delimitados por vírgulas)" ,"<br/>" ,"<br/>")),
                    actionButton("update_bt","Ver dados"),
                    fluid = TRUE
                ),
                mainPanel(
                    h4("Dados da Rede"),
                    verbatimTextOutput("oid1"),
                    verbatimTextOutput("oid2"),
                    tableOutput(outputId = "table.output"),
                    fluid = TRUE
                ),
                fluid = TRUE
            )
        ),
        # tabPanel("Plot",
        #         sidebarLayout(
        #             sidebarPanel(
        #                 radioButtons("plotType", "Plot type",
        #                              c("Scatter"="p", "Line"="l")
        #                 )
        #             ),
        #             mainPanel(
        #                 plotOutput("plot")
        #             )
        #         )
        # ),
        # tabPanel("Summary",
        #         verbatimTextOutput("summary")
        # ),
        # navbarMenu("More",
        #           tabPanel("Table",
        #                    DT::dataTableOutput("table")
        #           ),
        #           tabPanel("About",
        #                    fluidRow(
        #                        column(6,
        #                               # includeMarkdown("about.md")
        #                        # ),
        #                        # column(3,
        #                        #        img(class="img-polaroid",
        #                        #            src=paste0("http://upload.wikimedia.org/",
        #                        #                       "wikipedia/commons/9/92/",
        #                        #                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
        #                        #        tags$small(
        #                        #            "Source: Photographed at the Bay State Antique ",
        #                        #            "Automobile Club's July 10, 2005 show at the ",
        #                        #            "Endicott Estate in Dedham, MA by ",
        #                        #            a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
        #                        #              "User:Sfoskett")
        #                               )
        #                        )
        #                    )
        #           ),
        collapsible = TRUE,
        fluid = TRUE
        )
)
    
    
        
################################################################################
# Define server                                                           ######
################################################################################
server <- function(input, output, session) {

  ###### Número de colunas do Data Frame ##
  nrColDF <- reactive(max(ncol(pointTb())
                          ,ncol(sucTb())
                          ,ncol(distTb())
                          ,ncol(capacidadeTb())
                          )
                      )

    

  ###### pointTb  ###########################
  pointTb <- eventReactive(input$update_bt, {
    
    if (input$point == "" | input$suc == "" | input$dist == "") return(invisible())
    
    pTb <- ler_nr(input$point, "point")

        
    return(pTb)
  })
  
  
  
  ###### sucTb  ###########################
  sucTb <- eventReactive(input$update_bt, {
    
    if (input$point == "" | input$suc == "" | input$dist == "") return(invisible())
    
    sTb <- ler_nr(input$suc, "suc")
    

    return(sTb)
  })

  
    
  ###### distTb  ###########################
  distTb <- eventReactive(input$update_bt, {
    
    if (input$point == "" | input$suc == "" | input$dist == "") return(invisible())
    
    dTb <- ler_nr(input$dist, "dist")
    
    
    return(dTb)
  })
  
  
  
  ###### capacidadeTb  ###########################
  capacidadeTb <- eventReactive(input$update_bt, {
    
    if (input$point == "" | input$suc == "" | input$dist == "" | input$capacidade == "") return(invisible())
    
    cTb <- ler_nr(input$capacidade, "capacidade")
    
    
    return(cTb)
  })
  

  
  ###### tabela  ###########################
  tabela <- eventReactive(input$update_bt, {
    
    if (input$point == "" | input$suc == "" | input$dist == "") return(invisible())

    ##### point ##############################################################
    if(row.names(pointTb()) == "point")
      if (nrColDF() < ncol(pointTb())){
  
        dfDados <<- data.frame(pointTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
  
      }
      else if ((!exists("dfDados") || nrow(dfDados) == 0) ){
        
        dfDados <<- data.frame(pointTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
  
        dfDados <<- aum_col(nrColDF(), pointTb())
        
      }
      else{
        ####IMPORTANTE: ver o aumento das colunas da tabela depois de criada!!##
        
        dfDados <<- aum_col(nrColDF(), pointTb())

      }

    
    ##### suc ##############################################################
    if(row.names(sucTb()) == "suc"){
      if (nrColDF() < ncol(sucTb())){
        
        dfDados <<- data.frame(sucTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
      }
      else if ((!exists("dfDados") || nrow(dfDados) == 0) ){
        
        dfDados <<- data.frame(sucTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
        dfDados <<- aum_col(nrColDF(), sucTb())
        
      }
      else if (nrow(dfDados) > 1){
        ####IMPORTANTE: ver o aumento das colunas da tabela depois de criada!!##
        
        dfDados["suc",] <<- aum_col(nrColDF(), sucTb())

      }
      else if (nrow(dfDados) == 1 & nrColDF() > ncol(sucTb())){
        
        dfDados <<- aum_col(nrColDF(), pointTb())
        dfDados <<- aum_col(nrColDF(), sucTb())
        
      }
      else{

        dfDados <<- rbind(dfDados,sucTb())
        
      }
    }


    ##### dist ##############################################################
    if(row.names(distTb()) == "dist")
      if (nrColDF() < ncol(distTb())){
  
        dfDados <<- data.frame(distTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
      }
      else if ((!exists("dfDados") || nrow(dfDados) == 0) ){
        
        dfDados <<- data.frame(distTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
        dfDados <<- aum_col(nrColDF(), distTb())
        
      }
      else{
        ####IMPORTANTE: ver o aumento das colunas da tabela depois de criada!!##
        
        dfDados <<- rbind(dfDados,distTb())
        
      }
    

    ##### capacidade ##############################################################
    if(input$capacidade != "" && row.names(capacidadeTb()) == "capacidade")
      if (input$capacidade != "" && nrColDF() < ncol(capacidadeTb())){
        
        dfDados <<- data.frame(capacidadeTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
      }
      else if ((!exists("dfDados") || nrow(dfDados) == 0) ){
        
        dfDados <<- data.frame(capacidadeTb())
        names(dfDados) <<- gsub("X", "", names(dfDados))
        
        dfDados <<- aum_col(nrColDF(), capacidadeTb())
        
      }
      else{
        ####IMPORTANTE: ver o aumento das colunas da tabela depois de criada!!##
        
        dfDados <<- rbind(dfDados,capacidadeTb())
        
      }
    

    dfDados[dfDados == 0] <<- ""
    

        
    return(dfDados)
    
  })

  

  output$table.output <- renderTable(tabela(), digits = 0, bordered = TRUE, rownames = TRUE)
  

}


################################################################################
# Run the application                                                     ######
################################################################################
shinyApp(ui = ui, server = server)

