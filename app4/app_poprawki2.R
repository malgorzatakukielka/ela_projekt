#WERSJA ZE SLIDEREM
#ładowanie pakietów
library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinyjs)
library(bslib)
library(shinyToastify)
library(DT)
library(data.table)
library(scales)
library(shinyWidgets)

#źródło danych
ela1 <- read.csv("~/ela_projekt/ela1.csv")
ela1 <- ela1 %>% drop_na()

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
                          pickerInput("uczelnia", "Wybierz uczelnię:", 
                                      choices = c("", unique(ela1$P_NAZWA_UCZELNI)),
                                      options = pickerOptions(
                                          noneSelectedText = "Brak wyboru",
                                          liveSearch = TRUE
                                      )), 
                          
                          pickerInput("kierunek", "Wybierz kierunek:", 
                                      choices = "",
                                    options = pickerOptions(
                                      noneSelectedText = "Brak wyboru",
                                      liveSearch = TRUE
                                    )),
                        
                          pickerInput("poziomforma", "Wybierz poziom i formę studiów:", 
                                      choices = "",
                                      options = pickerOptions(
                                          noneSelectedText = "Brak wyboru",
                                          liveSearch = TRUE
                                      )),

                          radioButtons("zarobki", "Zarobki", choices = unique(ela1$p)),
                          # textOutput("test"),
                          
                          #slider
                          sliderInput("lata", "Wybierz lata:", 
                                          min = 2015, 
                                          max = max(ela1$P_ROKDYP), 
                                          value = c(2015, 2022), step = 1, sep = "", width = '85%') ,
                          
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
            plotlyOutput("comparison_plot", height = "600px"),
            div(
                style = "display: flex; justify-content: flex-end; margin-bottom: 10px;",
                actionButton("reset_comparison", "Wyczyść porównanie")
            ),
            DTOutput("table"),
        ),
    )        
)




server <- function(input, output, session) {
    
    # output$test <- renderText({input$poziomforma})
    
    
    
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
            updatePickerInput(session, inputId = "kierunek", choices = choices, selected = input$kierunek)
        } else {
            updatePickerInput(session, inputId = "kierunek", choices = c("", choices))
        }
    })
    
    kierunek <- reactive({
        req(input$uczelnia)
        filter(uczelnia(), P_KIERUNEK_NAZWA == input$kierunek)
    })
    
    observeEvent(input$kierunek, {
        choices <- unique(kierunek()$P_POZIOMFORMA)
        if (input$poziomforma %in% choices) {
            updatePickerInput(session, inputId = "poziomforma", 
                              choices = c("", choices), selected = input$poziomforma)
        } else {
            updatePickerInput(session, inputId = "poziomforma", choices = c("", choices),
                              selected = "")
        }
        
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
                           text = paste("Średnia ważona:", round(srednia, 2),"zł" ,
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
                                            "\nMediana zarobków:" ,me_zar,"zł", 
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
       # Funkcja do znajdowania najmniejszego dostępnego ID w przedziale 1-7
        find_smallest_id <- function(data, id_column) {
          # Pobieranie istneijących ID
          existing_ids <- data[[id_column]]
          # Dostępne ID w 1-7
          available_ids <- setdiff(1:7, existing_ids)
          # Najmniejsze dostępne ID
          new_id <- if(length(available_ids) > 0) min(available_ids) else NA
          return(new_id)
        }
        
        # Funkcja do dodawania ID do nowych wierszy
        add_ids_to_new_rows <- function(data_to_add, current_data) {
          new_id <- find_smallest_id(current_data, "id")
          data_to_add$id <- new_id
          return(data_to_add)
        }
        
        # Dodanie ID do nowych wierszy w danych do dodania
        dane_do_dodania_z_id <- add_ids_to_new_rows(dane_do_dodania, current_data)
        updated_data <- bind_rows(current_data, dane_do_dodania_z_id)

        
        # Sprawdzenie liczby unikalnych kombinacji
        unique_combinations <- updated_data %>%
            mutate(kombinacja = paste(kierunek, uczelnia, poziomforma, mediana_lata, sep = ",\n")) %>%
            distinct(kombinacja)
        
        if (nrow(unique_combinations) > 7) {
            showModal(modalDialog(
                title = "Ograniczenie liczby porównań",
                "Maksymalna liczba elementów w porównaniu to 7. Usuń jeden z elementów z porównania, aby dodać nowy.",
                easyClose = TRUE,
                footer = NULL
            ))
        } else {
            # Aktualizacja comparison_data tylko jeśli limit nie został przekroczony
            comparison_data(updated_data)
            
            # kod okna przeniesiony
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
            
        }
        
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
                filter(!(kierunek == input$kierunek & 
                             uczelnia == input$uczelnia & 
                             poziomforma == input$poziomforma & 
                             mediana_lata == input$zarobki)) 
            #poprawione filtorwanie na usuwanie - dodanie filtra na poziomforma i mediana_lata
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
    
    
    # tu jest zmiana względem wczesniejszej wersji
    observeEvent(input$add_to_comparison, {
        if (input$kierunek == "" |
            input$uczelnia == "" |
            input$poziomforma == "") {
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
      
      # Statyczna mapa kolorów przypisana do id
      kolor_map <- c("1" = "#F8766D", "2" = "#C49A00", "3" = "#53B400", 
                     "4" = "#00C094", "5" = "#00B6EB", "6" = "#A58AFF", 
                     "7" = "#FB61D7")
      
      # Przypisanie kolorów na podstawie id
      data$kolor <- kolor_map[as.character(data$id)]
      
      # Unikalna kombinacja zmiennych
      data <- data %>% 
        mutate(kombinacja = paste(kierunek, uczelnia, poziomforma, mediana_lata, sep = ",\n"))
      
      # Przypisanie kolorów do kombinacji (wyświetlanej w legendzie)
      unique_combinations <- unique(data$kombinacja)
      color_palette <- setNames(kolor_map[unique(data$id)], unique_combinations)
      
      comp_p <- ggplot(data, aes(x = P_ROKDYP, y = srednia, 
                                 color = kombinacja,  # Użyj kombinacji jako mapowania kolorów
                                 text = paste("Zarobki:", round(srednia, 2), "zł",
                                              "\nKierunek:", kierunek,
                                              "\nUczelnia:", uczelnia,
                                              "\nPoziom i forma studiów:", poziomforma,
                                              "\nZarobki:", mediana_lata)
      )) +
        geom_line(aes(group = interaction(kierunek, uczelnia, poziomforma))) +
        geom_point() +
        ylab("Mediana zarobków")+
        xlab("Rok uzyskania dyplomu") +
        theme_bw() +
        guides(color = guide_legend(title = "Kierunki studiów w porównaniu")) +
        scale_color_manual(values = color_palette) + 
        scale_x_continuous(breaks = c(2015:2022), limits = c(2014.5, 2022.5))
      
      ggplotly(comp_p, tooltip = "text")
    })

    
        
    # Usuwanie wiersza z tabeli
    observeEvent(input$delete_from_table, {
        # Wyciągnięcie ID klikniętego przycisku
        id_to_remove <- input$delete_from_table
        
        # Sprawdzenie, czy ID jest poprawne
        if (grepl("^delete_from_table_", id_to_remove)) {
            # Uzyskanie numeru wiersza z ID (po usunięciu prefixu)
            row_id <- as.numeric(sub("delete_from_table_", "", id_to_remove))
            
            # Usunięcie wiersza z danych comparison_data
            comparison_data(
                comparison_data() %>%
                    filter(id != row_id)  # Filtrowanie danych i usuwanie wiersza o podanym ID
            )
        }
    })
    
    #tabela - usuwanie danych z porównania
    output$table <- renderDT({
        data <- comparison_data()
        
        if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
            # Tworzenie pustej tabeli z nagłówkami, aby wyeliiminować błąd z pustym id z comparison_data
            table_comparison <- data.frame(
                uczelnia = character(),
                kierunek = character(),
                poziomforma = character(),
                mediana_lata= character(),
                id = integer(),
                Akcja = character()
            ) %>% 
                rename(
                    "Uczelnia" = uczelnia, 
                    "Kierunek" = kierunek, 
                    "Poziom i forma studiów" = poziomforma, 
                    "Mediana zarobków w pierwszym/drugim roku od uzyskania dyplomu" = mediana_lata
                ) 
        } else {
            # Tworzenie tabeli z danymi
            table_comparison <- data %>% 
                select(uczelnia, kierunek, poziomforma, mediana_lata, id) %>% 
                distinct() %>% 
                rename(
                    "Uczelnia" = uczelnia, 
                    "Kierunek" = kierunek, 
                    "Poziom i forma studiów" = poziomforma, 
                    "Mediana zarobków w pierwszym/drugim roku od uzyskania dyplomu" = mediana_lata
                ) %>% 
                mutate(
                    Akcja = paste0('<button id="delete_from_table_', id, 
                                   '" type="button" class="btn btn-default action-button" 
                       onclick="Shiny.setInputValue(\'delete_from_table\', 
                       this.id, {priority: \'event\'})">Usuń</button>')
                )
        }
        
        datatable(
            table_comparison,
            escape = FALSE,
            selection = "none",
            options = list( #opcje dla DT
                searching = F,
                lengthChange = F,
                ordering = F,
                paging = F,
                language = list(
                    info = "Wyświetlono _START_ do _END_ z _TOTAL_ rekordów",
                    infoFiltered = "(odfiltrowano z _MAX_ rekordów)",
                    infoEmpty = "Brak rekordów do wyświetlenia",
                    paginate = list(previous = "Poprzednia", `next` = "Następna"),
                    zeroRecords = "Brak danych. Dodaj pierwszy kierunek do porównania."
                ),
                columnDefs = list(
                    list(
                        targets = 5, #ukrycie kolumny ID
                        visible = F
                    )
                )
            )
        )
    })
    
    # czyszczenie porównania
    observeEvent(input$reset_comparison, {
        comparison_data(data.frame(
            uczelnia = character(),
            kierunek = character(),
            poziomforma = character(),
            mediana_lata = character()
        ))
    })
    
    
    
}

shinyApp(ui, server)

