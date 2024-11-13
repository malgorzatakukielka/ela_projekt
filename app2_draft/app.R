#WERSJA ZE SLIDEREM
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
      
      #slider
      conditionalPanel(
        condition = "input.poziomforma != ''",
        sliderInput("lata", "Wybierz lata:", 
                    min = 2015, 
                    max = max(ela1$P_ROKDYP), 
                    value = c(2015, 2022), step = 1, sep = "", width = '85%')) ,  
      
      actionButton("reset_input", "Wyczyść")
    ),
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }",
                 "#on_off_container { text-align: right; margin-top: 10px; }"
      ),
      plotlyOutput("wykres", height = "600px"), #dostosowanie wysokości
      conditionalPanel(
        condition = "input.poziomforma != ''",
        checkboxInput("on_off", "Pokaż/ukryj poszczególne kierunki", TRUE) #"wyłącznik" poszczególnych kierunków
      )
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
    # Filtrowanie danych
    dane_do_wykresu <- ela1 %>%
      filter(P_NAZWA_UCZELNI == input$uczelnia,
             P_KIERUNEK_NAZWA == input$kierunek,
             P_POZIOMFORMA == input$poziomforma,
             p == input$zarobki) %>%
      mutate(P_NAZWA_JEDN = ifelse(P_NAZWA_JEDN == "", 
                                   "Jednostka niesprecyzowana", 
                                   P_NAZWA_JEDN)) %>%
      select(P_ROKDYP, me_zar, P_NAZWA_JEDN, P_N, P_KIERUNEK_ID, P_NAZWA_KIERUNKU_PELNA)
    
    # Obliczenie średniej ważonej dla mediany zarobków 
    srednia_suma <- dane_do_wykresu %>%
      group_by(P_ROKDYP) %>% 
      summarise(srednia = sum(P_N * me_zar) / sum(P_N), suma_P_N = sum(P_N))
    
    # Tworzenie wykresu
    #stary wykres
    # p <- ggplot(dane_do_wykresu, aes(x = P_ROKDYP,
    #                                  y = me_zar,
    #                                  color = P_NAZWA_JEDN,
    #                                  group = P_NAZWA_JEDN,
    #                                  text = paste("Rok:", P_ROKDYP, 
    #                                               "\nMediana zarobków:", me_zar, 
    #                                               "\nJednostka:", P_NAZWA_JEDN))) +
    #   geom_line() +
    #   theme_bw() +
    #   xlab("Rok uzyskania dyplomu") +
    #   ylab("Mediana zarobków") +
    #   labs(color = "Jednostka Organizacyjna") +
    #   ylim(0, 10000) +
    #   scale_x_continuous(breaks = seq(2015, 2022, by = 1), limits = c(2014.5, 2022.5))
    
    #nowy wykres
    p <- dane_do_wykresu %>% 
      ggplot(aes(x = P_ROKDYP)) +
      geom_line(data = srednia_suma, aes(y = srednia), color = 'black', linewidth = 1) +
      geom_point(data = srednia_suma, 
                 aes(y = srednia, 
                     text = paste("Średnia ważona:", round(srednia, 2), 
                                  "\nLiczba absolwentów:", suma_P_N)), 
                 color = 'black', size = 3) +
      xlab("Rok uzyskania dyplomu") +
      ylab("Mediana zarobków") +
      ylim(0, 10000) +
      scale_x_continuous(breaks = c(2015:2022), limits = c(min(input$lata)-0.5, max(input$lata)+0.5))+
      theme_bw()
    
    if (input$on_off) {
      p <- p + geom_line(aes(y = me_zar, group = P_KIERUNEK_ID), color = 'grey') +
        geom_point(aes(y = me_zar,
                       text = paste("Rok:", P_ROKDYP, 
                                    "\nMediana zarobków:" ,me_zar, 
                                    "\nLiczba absolwentów:", P_N,
                                    "\nNazwa kierunku:" ,P_NAZWA_KIERUNKU_PELNA, 
                                    "\nJednostka:", P_NAZWA_JEDN)), 
                   color = 'grey')
    }
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)

