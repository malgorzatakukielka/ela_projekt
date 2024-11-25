#WERSJA ZE SLIDEREM
#ładowanie pakietów
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinyjs)
library(bslib)
library(shinyToastify)

#źródło danych
ela1 <- read.csv("~/ela_projekt/ela1.csv")

#źródło do slidera
source("./Rsource/SwitchButton.R")

#tabela do porównań
comparison_data <- reactiveVal(data.frame(P_ROKDYP = integer(), 
                                          srednia = numeric(), 
                                          uczelnia = character(), 
                                          kierunek = character(), 
                                          poziomforma = character(),
                                          mediana_lata = character()
                                          ))

ui <- page_navbar(
  title = "Ekonomiczne Losy Absolwentów",
  nav_panel( #1. zakładka aplikacji
    title = "Zarobki absolwentów",
    fluidPage(theme = "button.css",
      useShinyjs(),
      useShinyToastify(),
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
          ), 
          radioButtons("zarobki", "Zarobki", choices = unique(ela1$p)),
      
      #slider
      conditionalPanel(
        condition = "input.poziomforma != ''",
        sliderInput("lata", "Wybierz lata:", 
                    min = 2015, 
                    max = max(ela1$P_ROKDYP), 
                    value = c(2015, 2022), step = 1, sep = "", width = '85%')) ,
      
       #dodaj do porównania 
      conditionalPanel(
        condition = "!output.isAdded",
        actionButton("add_to_comparison", "Dodaj do porównania")
      ),
      
      #usuń z porównania
      conditionalPanel(
        condition = "output.isAdded",
        actionButton("remove_from_comparison", "Usuń z porównania")
      ),
      
      br(),
      
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
        switchButton("on_off", "Pokaż/ukryj poszczególne kierunki", value = TRUE, col = "GB") #"wyłącznik" poszczególnych kierunków
     )
    )
   )
  )
 ),
 nav_panel( # 2. zakładka aplikacji
   title = "Porównanie",
   fluidPage(
     useShinyjs(),
     titlePanel("Zarobki absolwentów - porównanie"),
     plotlyOutput("comparison_plot", height = "600px")),
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
    
    #dodanie do porównania
    observeEvent(input$add_to_comparison, {
      req(input$poziomforma)
      
      dane_do_dodania <- ela1 %>%
        filter(P_NAZWA_UCZELNI == input$uczelnia,
               P_KIERUNEK_NAZWA == input$kierunek,
               P_POZIOMFORMA == input$poziomforma,
               p == input$zarobki) %>%
        group_by(P_ROKDYP) %>%
        summarise(srednia = sum(P_N * me_zar) / sum(P_N)) %>%
        mutate(uczelnia = input$uczelnia, kierunek = input$kierunek, 
               poziomforma = input$poziomforma, mediana_lata = input$zarobki)
      
      # Dodanie do istniejącej tabeli
      current_data <- comparison_data()
      updated_data <- bind_rows(current_data, dane_do_dodania)
      comparison_data(updated_data)
    })
    
    #sprawdzenie, czy kierunek jest w comparison_data
    output$isAdded <- reactive({
      req(input$kierunek, input$uczelnia)
      any(
        comparison_data()$kierunek == input$kierunek & 
          comparison_data()$uczelnia == input$uczelnia &
          comparison_data()$poziomforma == input$poziomforma &
          comparison_data()$mediana_lata == input$zarobki
      )
    })
    #Kod do logiki dodaj/usuń
    outputOptions(output, "isAdded", suspendWhenHidden = FALSE)
    
    
    #Logika usuwania z porównania
    observeEvent(input$remove_from_comparison, {
      comparison_data(
        comparison_data() %>%
          filter(!(kierunek == input$kierunek & uczelnia == input$uczelnia))
      )
    })
    
    observeEvent(input$remove_from_comparison, {
      showToast(
        session,
        input,
        text = "Pomyślnie usunięto z porównania",
        position = "bottom-right",
        hideProgressBar = T,
        style = list(
          color = "#3b3b3b"
        )
      )
    })
    
    observeEvent(input$add_to_comparison, {
      if (input$kierunek != "" &
         input$uczelnia != "" &
         input$poziomforma != "")
        showToast(
          session,
          input,
          text = "Pomyślnie dodano do porównania",
          position = "bottom-right",
          hideProgressBar = T,
          style = list(
            color = "#3b3b3b"
          )
        )
      else {
        showToast(
          session,
          input,
          text = "Aby dodać do porównania uzupełnij nazwę uczelni, kierunku oraz poziom i formę studiów",
          position = "bottom-right",
          hideProgressBar = T,
          style = list(
            width = "400px",
            right = "100px",
            bottom = "20px",
            padding = "10px",
            color = "#3b3b3b"
          )
      )
      }
      
    })
    
    #output drugiego okna
    output$comparison_plot <- renderPlotly({
      data <- comparison_data()
      

        comp_p <- ggplot(data, aes(x = P_ROKDYP, y = srednia, 
                                   color = paste(kierunek, uczelnia, poziomforma, mediana_lata, sep = ",\n"),
                         text = paste("Zarobki:", round(srednia, 2),
                                      "\nKierunek:", kierunek,
                                      "\nUczelnia:", uczelnia,
                                      "\nZarobki:", mediana_lata)
                         )) +
          geom_line(aes(group = interaction(kierunek, uczelnia, poziomforma))) +
          geom_point() +
          ylab("Mediana zarobków")+
          xlab("Rok uzyskania dyplomu") +
          theme_bw()+
          guides(color=guide_legend(title="Kierunki studiów w porównaniu"))+
          scale_x_continuous(breaks = c(2015:2022), limits = c(2014.5, 2022.5))
        
        ggplotly(comp_p, tooltip = "text")
      
    })
    

}

shinyApp(ui, server)

