#ładowanie pakietów
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinyjs)

ela1 <- read.csv("~/ela_projekt/ela1.csv")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Zarobki absolwentów"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("uczelnia", "Wybierz uczelnię:", 
                     choices = c("", unique(ela1$P_NAZWA_UCZELNI))), 
      conditionalPanel(
        condition = "input.uczelnia != ''",
        selectInput("kierunek", "Wybierz kierunek:", choices = NULL)
      ),
      conditionalPanel(
        condition = "input.kierunek != ''",
        selectInput("poziomforma", "Wybierz poziom i formę studiów:", choices = NULL)
        #zamiana na jedną listę wyboru
      ), 
      radioButtons("zarobki", "Zarobki", choices = unique(ela1$p)),
      actionButton("reset_input", "Wyczyść") 
    ),
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      plotlyOutput("wykres")
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$uczelnia, {
    updateSelectInput(session, "kierunek", selected = NULL)
    updateSelectInput(session, "poziomforma", selected = NULL) #zamiana inputId
  })
  
  uczelnia <- reactive({
    filter(ela1, P_NAZWA_UCZELNI == input$uczelnia)
  })
  
  observeEvent(input$uczelnia, {
    choices <- unique(uczelnia()$P_KIERUNEK_NAZWA)
    if (input$kierunek %in% choices) {
      updateSelectInput(session, inputId = "kierunek", choices = choices, selected = input$kierunek)
    } else {
      updateSelectInput(session, inputId = "kierunek", choices = c("", choices))
    }
  })
  
  kierunek <- reactive({
    req(input$uczelnia)
    filter(uczelnia(), P_KIERUNEK_NAZWA == input$kierunek)
  })
  
  observeEvent(input$kierunek, {
    choices <- unique(kierunek()$P_POZIOMFORMA)
    updateSelectInput(session, inputId = "poziomforma", choices = c("", choices))
    #zamiana inputId
  })
  
  poziomforma <- reactive({
    req(input$kierunek)
    filter(kierunek(), P_POZIOMFORMA == input$poziomforma)
  })
  #zmiana kolumny tabeli i Id inputu
  
  observeEvent(input$reset_input, {
    shinyjs::refresh()
  })
  
  output$wykres <- renderPlotly({
    req(input$poziomforma)
    dane_do_wykresu <- poziomforma() %>%
      filter(p == input$zarobki)
    
    p <- ggplot(dane_do_wykresu, aes(x = P_ROKDYP,
                                     y = me_zar,
                                     text = paste("Rok:", P_ROKDYP, 
                                                  "\n", "Mediana zarobków:",
                                                  me_zar))) +
      geom_line(group = 1) + theme_bw() +
      xlab("Rok uzyskania dyplomu") +
      ylab("Mediana zarobków") +
      ylim(0, 10000) +
      scale_x_continuous(breaks = c(2015:2021), limits = c(2014.5, 2021.5))
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
