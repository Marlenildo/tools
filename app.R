
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(dplyr)
aaaaaa



#Lâmina (mm/m²)
lamina1 = function(ET0){
  return(ET0*0.3)
}


#Lâmina (m³/ha)
lamina2 = function(ET0){
  return(ET0*0.3*10)
}



#Horas de irrigação (h)
irriga = function(ET0, dist_linha, dist_gotej, vazao_gotej, CE) {
  return(((((0.3*ET0)*10)/((10000/dist_linha)*(1/dist_gotej)*(vazao_gotej/1000)))*(1+(CE*0.05))))
}





sidebar <- dashboardSidebar(
  sidebarMenu(
    
    menuItem("Melão", tabName = "melao", icon= icon("vial")),  
    menuItem("Melancia", tabName = "melancia", icon = icon("atom"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "melao",
            h2(
              fluidPage(
                
                #Botões de irrigação do melão----
                box(
                  style="font-size: 50%",
                  width = 2,
                  background= "light-blue",
                  solidHeader = FALSE,
                  status = "primary",
                  div(style="display:inline-block;vertical-align:top;",
                      numericInput(inputId = "n_et0_me",label = "ET0 (mm)",value = 5, width = '100px'),
                      numericInput(inputId = "n_CE_me",label = "Condutividade Elétrica (dS/m)",value = 2, width = '100px'),
                      numericInput(inputId = "n_dist_linha_me",label = "Distância entre linhas (m)",value = 2, width = '100px'),
                      numericInput(inputId = "n_dist_gotej_me",label = "Distância entre gotejadores (m)",value = 0.5, width = '100px'),
                      numericInput(inputId = "n_vazao_gotej_me",label = "Vazão do gotejador (L/h)",value = 1.5, width = '100px'),
                  )),
                
                #Tabela de irrigação do melão----
                box(
                  style="font-size: 50%",
                  width = 10,
                  title = "SUGESTÃO DE IRRIGAÇÃO PARA O DIA ANTERIOR - MELÃO", 
                  status = "primary", 
                  solidHeader = FALSE,
                  tableOutput(outputId = "tab_melao")))
              
            )),
    tabItem(tabName = "melancia",
            h2(
              fluidPage(
                
                #Botões de irrigação do melão----
                box(
                  style="font-size: 50%",
                  width = 2,
                  background= "olive",
                  solidHeader = FALSE,
                  status = "success",
                  div(style="display:inline-block;vertical-align:top;",
                      numericInput(inputId = "n_et0_wme",label = "ET0 (mm)",value = 5, width = '100px'),
                      numericInput(inputId = "n_CE_wme",label = "Condutividade Elétrica (dS/m)",value = 2, width = '100px'),
                      numericInput(inputId = "n_dist_linha_wme",label = "Distância entre linhas (m)",value = 2, width = '100px'),
                      numericInput(inputId = "n_dist_gotej_wme",label = "Distância entre gotejadores (m)",value = 0.5, width = '100px'),
                      numericInput(inputId = "n_vazao_gotej_wme",label = "Vazão do gotejador (L/h)",value = 1.5, width = '100px'),
                  )),
                box(
                  style="font-size: 50%",
                  width = 10,
                  title = "SUGESTÃO DE IRRIGAÇÃO PARA O DIA ANTERIOR - MELANCIA", 
                  status = "success", 
                  solidHeader = FALSE,
                  tableOutput("tab_melancia")))
            ))
    )
)

ui <- dashboardPage(
  
  dashboardHeader(title = "Irrigação"),
  sidebar,
  body
)

server <- function(input, output) {
  
  output$tab_melao = renderTable({
    
    tribble(
      ~"Fase",
      ~"Kc",
      ~"Lâmina (mm/m²)",
      ~"Lâmina (m³/ha)",
      ~"Irrigação (h)",
      ~"h",
      
      "S1 - Até retirada da manta (±23 dias)",
      0.3,
      0.3*input$n_et0_me, #Lâmina (mm/m²)
      0.3*input$n_et0_me*10, #Lâmina (m³/ha)
      irriga(ET0 = input$n_et0_me,
             dist_linha = input$n_dist_linha_me,
             dist_gotej = input$n_dist_gotej_me,
             vazao_gotej = input$n_vazao_gotej_me,
             CE = input$n_CE_me),
      as.integer(irriga(ET0 = input$n_et0_me,
                     dist_linha = input$n_dist_linha_me,
                     dist_gotej = input$n_dist_gotej_me,
                     vazao_gotej = input$n_vazao_gotej_me,
                     CE = input$n_CE_me)),
      
      "S2 - Até finalizar o pegamento (±7 dias)",
      0.7,
      lamina1(ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(ET0 = input$n_et0_me), #Lâmina (m³/ha)
      irriga(ET0 = input$n_et0_me,
             dist_linha = input$n_dist_linha_me,
             dist_gotej = input$n_dist_gotej_me,
             vazao_gotej = input$n_vazao_gotej_me,
             CE = input$n_CE_me),
      as.integer(irriga(ET0 = input$n_et0_me,
                     dist_linha = input$n_dist_linha_me,
                     dist_gotej = input$n_dist_gotej_me,
                     vazao_gotej = input$n_vazao_gotej_me,
                     CE = input$n_CE_me)),
      
      "S3 - Crescimento longitudinal dos frutos (±10 dias)",
      1.2,
      lamina1(ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(ET0 = input$n_et0_me), #Lâmina (m³/ha)
      irriga(ET0 = input$n_et0_me,
             dist_linha = input$n_dist_linha_me,
             dist_gotej = input$n_dist_gotej_me,
             vazao_gotej = input$n_vazao_gotej_me,
             CE = input$n_CE_me),
      as.integer(irriga(ET0 = input$n_et0_me,
                     dist_linha = input$n_dist_linha_me,
                     dist_gotej = input$n_dist_gotej_me,
                     vazao_gotej = input$n_vazao_gotej_me,
                     CE = input$n_CE_me)),
      
      "S4 - Crescimento equatorial dos frutos (±10 dias)",
      1.2,
      lamina1(ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(ET0 = input$n_et0_me), #Lâmina (m³/ha)
      irriga(ET0 = input$n_et0_me,
             dist_linha = input$n_dist_linha_me,
             dist_gotej = input$n_dist_gotej_me,
             vazao_gotej = input$n_vazao_gotej_me,
             CE = input$n_CE_me),
      as.integer(irriga(ET0 = input$n_et0_me,
                     dist_linha = input$n_dist_linha_me,
                     dist_gotej = input$n_dist_gotej_me,
                     vazao_gotej = input$n_vazao_gotej_me,
                     CE = input$n_CE_me)),
      
      "S5 - Maturação dos frutos (±7 dias)",
      0.9,
      lamina1(ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(ET0 = input$n_et0_me), #Lâmina (m³/ha)
      irriga(ET0 = input$n_et0_me,
             dist_linha = input$n_dist_linha_me,
             dist_gotej = input$n_dist_gotej_me,
             vazao_gotej = input$n_vazao_gotej_me,
             CE = input$n_CE_me),
      as.integer(irriga(ET0 = input$n_et0_me,
                     dist_linha = input$n_dist_linha_me,
                     dist_gotej = input$n_dist_gotej_me,
                     vazao_gotej = input$n_vazao_gotej_me,
                     CE = input$n_CE_me)),
      
      "S6 - Após a colheita",
      0.6,
      lamina1(ET0 = input$n_et0_me), #Lâmina (mm/m²)
      lamina2(ET0 = input$n_et0_me), #Lâmina (m³/ha)
      irriga(ET0 = input$n_et0_me,
             dist_linha = input$n_dist_linha_me,
             dist_gotej = input$n_dist_gotej_me,
             vazao_gotej = input$n_vazao_gotej_me,
             CE = input$n_CE_me),
      as.integer(irriga(ET0 = input$n_et0_me,
                     dist_linha = input$n_dist_linha_me,
                     dist_gotej = input$n_dist_gotej_me,
                     vazao_gotej = input$n_vazao_gotej_me,
                     CE = input$n_CE_me)),
      
    )})
  
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  DT::renderDT(data, selection = 'none', server = server, editable = editable, ...)
  
}



output$tab_melancia <-  renderTable({
  
  tribble(
    ~"Fase",
    ~"Kc",
    ~"Lâmina (mm/m²)",
    ~"Lâmina (m³/ha)",
    ~"Irrigação (h)",
    
    "S1 - Até retirada da manta (±23 dias)",
    0.3,
    0.3*input$n_et0_me, #Lâmina (mm/m²)
    0.3*input$n_et0_me*10, #Lâmina (m³/ha)
    irriga(ET0 = input$n_et0_wme,
           dist_linha = input$n_dist_linha_wme,
           dist_gotej = input$n_dist_gotej_wme,
           vazao_gotej = input$n_vazao_gotej_wme,
           CE = input$n_CE_wme),
    
    "S2 - Até finalizar o pegamento (±7 dias)",
    0.7,
    lamina1(ET0 = input$n_et0_wme), #Lâmina (mm/m²)
    lamina2(ET0 = input$n_et0_wme), #Lâmina (m³/ha)
    irriga(ET0 = input$n_et0_wme,
           dist_linha = input$n_dist_linha_wme,
           dist_gotej = input$n_dist_gotej_wme,
           vazao_gotej = input$n_vazao_gotej_wme,
           CE = input$n_CE_wme),
    
    "S3 - Crescimento longitudinal dos frutos (±10 dias)",
    1.2,
    lamina1(ET0 = input$n_et0_wme), #Lâmina (mm/m²)
    lamina2(ET0 = input$n_et0_wme), #Lâmina (m³/ha)
    irriga(ET0 = input$n_et0_wme,
           dist_linha = input$n_dist_linha_wme,
           dist_gotej = input$n_dist_gotej_wme,
           vazao_gotej = input$n_vazao_gotej_wme,
           CE = input$n_CE_wme),
    
    "S4 - Crescimento equatorial dos frutos (±10 dias)",
    1.2,
    lamina1(ET0 = input$n_et0_wme), #Lâmina (mm/m²)
    lamina2(ET0 = input$n_et0_wme), #Lâmina (m³/ha)
    irriga(ET0 = input$n_et0_wme,
           dist_linha = input$n_dist_linha_wme,
           dist_gotej = input$n_dist_gotej_wme,
           vazao_gotej = input$n_vazao_gotej_wme,
           CE = input$n_CE_wme),
    
    "S5 - Maturação dos frutos (±7 dias)",
    0.9,
    lamina1(ET0 = input$n_et0_wme), #Lâmina (mm/m²)
    lamina2(ET0 = input$n_et0_wme), #Lâmina (m³/ha)
    irriga(ET0 = input$n_et0_wme,
           dist_linha = input$n_dist_linha_wme,
           dist_gotej = input$n_dist_gotej_wme,
           vazao_gotej = input$n_vazao_gotej_wme,
           CE = input$n_CE_wme),
    
    "S6 - Após a colheita",
    0.6,
    lamina1(ET0 = input$n_et0_wme), #Lâmina (mm/m²)
    lamina2(ET0 = input$n_et0_wme), #Lâmina (m³/ha)
    irriga(ET0 = input$n_et0_wme,
           dist_linha = input$n_dist_linha_wme,
           dist_gotej = input$n_dist_gotej_wme,
           vazao_gotej = input$n_vazao_gotej_wme,
           CE = input$n_CE_wme)
  )})

render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  DT::renderDT(data, selection = 'none', server = server, editable = editable, ...)
  
}

}





#Tabela melancia
  
  
shinyApp(ui, server)