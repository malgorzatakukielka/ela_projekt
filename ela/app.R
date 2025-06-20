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
library(bsicons)

#źródło danych 
ela1 <- read.csv("ela1.csv")
ela1 <- ela1 %>% 
  pivot_longer(cols = 10:23, names_to = "zmienna", values_to = "wartosc") %>% 
  drop_na()

#źródło do slidera
source("./Rsource/SwitchButton.R")

#tabela do porównań
comparison_data <- reactiveVal(data.frame(P_ROKDYP = integer(), 
                                          srednia = numeric(), 
                                          uczelnia = character(), 
                                          kierunek = character(), 
                                          poziomforma = character(),
                                          zmienna = character()
))

#unikalne wartości zmiennych z ela1$zmienna
unikalne_zmienne <- unique(ela1$zmienna)

#funkcja mapowania nazw zmiennych ela1$zmienna
przeksztalc_nazwe <- function(skrot) {
  if (grepl("^P_E_ZAR_P[1-5]$", skrot)) {
    rok <- substr(skrot, 10, 10)
    return(paste("Średnie miesięczne wynagrodzenie absolwentów w", 
                 ifelse(rok == "1", "pierwszym", 
                        ifelse(rok == "2", "drugim", 
                               ifelse(rok == "3", "trzecim", 
                                      ifelse(rok == "4", "czwartym", "piątym")))), 
                 "roku po uzyskaniu dyplomu"))
  } 
  
  if (grepl("^P_ME_ZAR_P[1-5]$", skrot)) {
    rok <- substr(skrot, 11, 11)
    return(paste("Mediana średnich miesięcznych wynagrodzeń absolwentów w", 
                 ifelse(rok == "1", "pierwszym", 
                        ifelse(rok == "2", "drugim", 
                               ifelse(rok == "3", "trzecim", 
                                      ifelse(rok == "4", "czwartym", "piątym")))), 
                 "roku po uzyskaniu dyplomu"))
  }
  if (skrot == "P_CZAS_PRACA") {
    return("Średni czas (w miesiącach) od uzyskania dyplomu do podjęcia pierwszej pracy po uzyskaniu dyplomu")
  }
  
  if(skrot == "P_CZY_BEZR") {
    return("Odsetek absolwentów z doświadczeniem bezrobocia po uzyskaniu dyplomu")
  }
  if(skrot == "P_WWZ") {
    return("Względny Wskaźnik Zarobków absolwentów po uzyskaniu dyplomu")
  }
  if(skrot == "P_WWB") {
    return("Względny Wskaźnik Bezrobocia absolwentów po uzyskaniu dyplomu")
  }
  
  return(skrot)
} # jeśli nie pasuje do wzorca, zwraca oryginalną wartość

#ramka danych z mapowaniem skrótów
mapowanie_zmiennych <- data.frame(
  skrot = unikalne_zmienne,
  pelna_nazwa = sapply(unikalne_zmienne, przeksztalc_nazwe),
  stringsAsFactors = FALSE
)


ui <- page_navbar(
    title = "Ekonomiczne Losy Absolwentów",
    nav_panel( #1. zakładka aplikacji
        title = "Statystyki absolwentów", #zmiana na Statystyki
        fluidPage(theme = "button.css",
                  useShinyjs(),
                  useShinyToastify(),
                  titlePanel("Statystyki absolwentów"), # zmiana na Statystyki
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
                          #Analizowane zmienne
                          tags$div(
                            style = "display: flex; align-items: center; gap: 8px;", # Użycie gap dla odstępu
                            tags$span("Wybierz zmienną do analizy:"), # Tytuł pickera
                            tooltip(
                              bsicons::bs_icon("info-circle", style = "cursor: pointer;"),
                              HTML(
                                "<b>Względny Wskaźnik Zarobków (WWZ)</b>, 
                                to stosunek średnich zarobków absolwenta 
                                do średnich zarobków w jego powiecie zamieszkania.<br>
                                <b>Względny Wskaźnik Bezrobocia (WWB)</b>, 
                                to stosunek ryzyka bezrobocia absolwenta 
                                do stopy bezrobocia w jego powiecie zamieszkania."
                              )
                            )
                          ),
                          pickerInput("zmienna", 
                                      choices = setNames(mapowanie_zmiennych$skrot, 
                                                         mapowanie_zmiennych$pelna_nazwa),
                                      selected = NULL,
                                      options = pickerOptions(
                                        noneSelectedText = "Brak wyboru",
                                        liveSearch = TRUE
                                      )),
                          
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
            titlePanel("Statystyki absolwentów - porównanie"), #zmiana na Statystyki
            pickerInput("zmienna2", "Wybierz zmienną do analizy:", 
                                      choices = setNames(mapowanie_zmiennych$skrot, 
                                                         mapowanie_zmiennych$pelna_nazwa),
                        width = "auto",
                        options = pickerOptions(
                          noneSelectedText = "Brak wyboru",
                          liveSearch = TRUE
                        )),
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
    
    
  observeEvent(input$uczelnia, {
    req(input$uczelnia)
    
    kierunki_uczelni <- unique(uczelnia()$P_KIERUNEK_NAZWA)
    
    # aktualizacja kierunku
    if (input$kierunek %in% kierunki_uczelni) {
      updatePickerInput(session, "kierunek", choices = kierunki_uczelni, selected = input$kierunek)
    } else {
      updatePickerInput(session, "kierunek", choices = c("", kierunki_uczelni), selected = "")
      updatePickerInput(session, "poziomforma", choices = "", selected = "")
      return() # jeśli kierunek nie istnieje
    }
    
    # jeśli kierunek istnieje, ale forma nie
    poziomy <- unique(kierunek()$P_POZIOMFORMA)
    
    if (input$poziomforma %in% poziomy) {
      updatePickerInput(session, "poziomforma", choices = c("", poziomy), selected = input$poziomforma)
    } else {
      updatePickerInput(session, "poziomforma", choices = c("", poziomy), selected = "")
    }
  })
  
    
    uczelnia <- reactive({
        filter(ela1, P_NAZWA_UCZELNI == input$uczelnia)
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
                   zmienna == input$zmienna) %>%
            mutate(P_NAZWA_JEDN = ifelse(P_NAZWA_JEDN == "", 
                                         "Jednostka niesprecyzowana", 
                                         P_NAZWA_JEDN)) %>%
            select(P_ROKDYP, zmienna, wartosc,  P_NAZWA_JEDN, P_N, P_KIERUNEK_ID, P_NAZWA_KIERUNKU_PELNA)
        
        # Obliczenie średniej ważonej
        
        srednia_suma <- dane_do_wykresu %>%
            filter(zmienna == input$zmienna) %>% 
            group_by(P_ROKDYP) %>%
            summarise(
              srednia = round(sum(P_N * wartosc, na.rm = TRUE) / sum(P_N, na.rm = TRUE), 2), # Średnia ważona
              suma_P_N = sum(P_N, na.rm = TRUE)                                   # Suma liczebności
            )
        
        # Pobranie pełnej nazwy zmiennej na podstawie skrótu
        nazwa_pelna <- mapowanie_zmiennych$pelna_nazwa[mapowanie_zmiennych$skrot == input$zmienna]
        
        # Definicja etykiet Y-osi
        etykieta_y <- case_when(
          input$zmienna %in% mapowanie_zmiennych$skrot[grepl("^P_ME_ZAR_P[1-5]$", mapowanie_zmiennych$skrot)] ~ "Mediana zarobków (zł)",
          input$zmienna %in% mapowanie_zmiennych$skrot[grepl("^P_E_ZAR_P[1-5]$", mapowanie_zmiennych$skrot)] ~ "Średnie zarobki (zł)",
          input$zmienna == "P_CZAS_PRACA" ~ "Średni czas poszukiwania pracy (miesiące)",
          input$zmienna == "P_CZY_BEZR" ~ "Odsetek z doświadczeniem bezrobocia (%)",
          input$zmienna == "P_WWZ" ~ "Względny Wskaźnik Zarobków (WWZ)",
          input$zmienna == "P_WWB" ~ "Względny Wskaźnik Bezrobocia (WWB)",
          TRUE ~ "Wartość wskaźnika"
        )
        
        # Tworzenie wykresu
        p <- dane_do_wykresu %>% 
          ggplot(aes(x = P_ROKDYP)) +
          geom_line(data = srednia_suma, aes(y = srednia), color = 'black', linewidth = 1) +
          geom_point(data = srednia_suma, 
                     aes(y = srednia, 
                         text = paste(
                           case_when(
                             input$zmienna %in% mapowanie_zmiennych$skrot[grepl("^P_ME_ZAR_P[1-5]$", mapowanie_zmiennych$skrot)] ~ paste("Średnia ważona:", srednia, "zł"),
                             input$zmienna %in% mapowanie_zmiennych$skrot[grepl("^P_E_ZAR_P[1-5]$", mapowanie_zmiennych$skrot)] ~ paste("Średnia ważona:", srednia, "zł"),
                             input$zmienna == "P_CZAS_PRACA" ~ paste("Średnia ważona:", srednia, "mies."),
                             input$zmienna == "P_CZY_BEZR" ~ paste("Średnia ważona:", srednia, "%"),
                             input$zmienna %in% c("P_WWZ", "P_WWB") ~ paste("Średnia ważona:", srednia),
                             TRUE ~ paste("\nWartość wskaźnika:", srednia)
                           ),
                           "\nLiczba absolwentów:", suma_P_N)), 
                     color = 'black', size = 3) +
          xlab("Rok uzyskania dyplomu") +
          ylab(etykieta_y) +
          scale_x_continuous(breaks = c(2015:2022), limits = c(min(input$lata)-0.5, max(input$lata)+0.5)) +
          theme_bw()
        
        if (input$on_off) {
          p <- p + geom_line(aes(y = wartosc, group = P_KIERUNEK_ID), color = 'grey') +
            geom_point(aes(y = wartosc,
                           text = paste(
                             "Rok:", P_ROKDYP,
                             case_when(
                               input$zmienna %in% mapowanie_zmiennych$skrot[grepl("^P_ME_ZAR_P[1-5]$", mapowanie_zmiennych$skrot)] ~ paste("\nMediana:", wartosc, "zł"),
                               input$zmienna %in% mapowanie_zmiennych$skrot[grepl("^P_E_ZAR_P[1-5]$", mapowanie_zmiennych$skrot)] ~ paste("\nŚrednia:", wartosc, "zł"),
                               input$zmienna == "P_CZAS_PRACA" ~ paste("\nŚredni czas poszukiwania pierwszej pracy:", wartosc, "mies."),
                               input$zmienna == "P_CZY_BEZR" ~ paste("\nOdsetek z doświadczeniem bezrobocia:", wartosc, "%"),
                               input$zmienna %in% c("P_WWZ", "P_WWB") ~ paste("\nWWZ:", wartosc),
                               TRUE ~ paste("\nWartość wskaźnika:", wartosc)
                             ),
                             "\nLiczba absolwentów:", P_N,
                             "\nNazwa kierunku:", P_NAZWA_KIERUNKU_PELNA,
                             "\nJednostka:", P_NAZWA_JEDN
                           )),
                       color = 'grey')
        }
        
        # Generowanie interaktywnego wykresu
        ggplotly(p, tooltip = "text")
    })
    
    #dodanie do porównania
    observeEvent(input$add_to_comparison, {
        req(input$poziomforma)
        
      dane_do_dodania <- ela1 %>%
        filter(P_NAZWA_UCZELNI == input$uczelnia,
               P_KIERUNEK_NAZWA == input$kierunek,
               P_POZIOMFORMA == input$poziomforma) %>% # Filtrowanie tylko po uczelni, kierunku i formie
        group_by(P_ROKDYP, zmienna) %>% # Grupowanie zarówno po roku dyplomu, jak i zmiennej
        summarise(
          srednia = round(sum(P_N * wartosc, na.rm = TRUE) / sum(P_N, na.rm = TRUE), 2), # Średnia ważona dla każdej zmiennej
          suma_P_N = sum(P_N, na.rm = TRUE) # Suma liczebności dla każdej zmiennej
        ) %>%
        mutate(
          uczelnia = input$uczelnia,
          kierunek = input$kierunek,
          poziomforma = input$poziomforma
        )
        
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
            mutate(kombinacja = paste(kierunek, uczelnia, poziomforma, sep = ",\n")) %>%
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
            print(comparison_data)
            
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
                comparison_data()$poziomforma == input$poziomforma
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
                             poziomforma == input$poziomforma)) 
            #poprawione filtorwanie na usuwanie - dodanie filtra na poziomforma - usunięcie mediana_lata
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
        mutate(kombinacja = paste(kierunek, uczelnia, poziomforma, sep = ",\n"))
      
      # Przypisanie kolorów do kombinacji (wyświetlanej w legendzie)
      unique_combinations <- unique(data$kombinacja)
      color_palette <- setNames(kolor_map[unique(data$id)], unique_combinations)
      
      # Generowanie danych na podstawie input$zmienna2
      data_zmienna <- data %>%
        filter(zmienna == input$zmienna2) # Filtrowanie danych dla wybranej zmiennej
        
      
      # Tworzenie wykresu
      comp_p <- ggplot(data_zmienna, aes(
        x = P_ROKDYP,
        y = srednia,
        color = kombinacja,  # kombinacja jako mapowania kolorów
        text = paste(
          "Rok dyplomu:", P_ROKDYP,
          "\nKierunek:", kierunek,
          "\nUczelnia:", uczelnia,
          "\nPoziom i forma studiów:", poziomforma,
          case_when(
            input$zmienna2 %in% c("Mediana zarobków w pierwszym roku od uzyskania dyplomu",
                                 "Mediana zarobków w drugim roku od uzyskania dyplomu",
                                 "Mediana zarobków w trzecim roku od uzyskania dyplomu",
                                 "Mediana zarobków w czwartym roku od uzyskania dyplomu",
                                 "Mediana zarobków w piątym roku od uzyskania dyplomu") ~ paste("\nZarobki:", srednia, "zł"),
            input$zmienna2 == "Średni czas (w miesiącach) od uzyskania dyplomu do podjęcia pierwszej pracy po uzyskaniu dyplomu" ~ paste("\nŚredni czas poszukiwania pierwszej pracy:", srednia, "mies."),
            input$zmienna2 == "Odsetek absolwentów z doświadczeniem bezrobocia po uzyskaniu dyplomu" ~ paste("\nOdsetek z doświadczeniem bezrobocia:", srednia, "%"),
            input$zmienna2 == "Względny Wskaźnik Zarobków absolwentów po uzyskaniu dyplomu" ~ paste("\nWWZ:", srednia ),
            input$zmienna2 == "Względny Wskaźnik Bezrobocia absolwentów po uzyskaniu dyplomu" ~ paste("\nWWB:", srednia),
            TRUE ~ paste("\nWartość wskaźnika:", srednia)
          )
        )
      )) +
        geom_line(aes(group = interaction(kierunek, uczelnia, poziomforma))) +
        geom_point() +
        ylab(case_when(
          input$zmienna2 %in% c("Mediana zarobków w pierwszym roku od uzyskania dyplomu",
                               "Mediana zarobków w drugim roku od uzyskania dyplomu",
                               "Mediana zarobków w trzecim roku od uzyskania dyplomu",
                               "Mediana zarobków w czwartym roku od uzyskania dyplomu",
                               "Mediana zarobków w piątym roku od uzyskania dyplomu") ~ "Mediana zarobków (zł)",
          input$zmienna2 == "Średni czas (w miesiącach) od uzyskania dyplomu do podjęcia pierwszej pracy po uzyskaniu dyplomu" ~ "Średni czas poszukiwania pracy (miesiące)",
          input$zmienna2 == "Odsetek absolwentów z doświadczeniem bezrobocia po uzyskaniu dyplomu" ~ "Odsetek z doświadczeniem bezrobocia (%)",
          input$zmienna2 == "Względny Wskaźnik Zarobków absolwentów po uzyskaniu dyplomu" ~ "Względny Wskaźnik Zarobków (WWZ)",
          input$zmienna2 == "Względny Wskaźnik Bezrobocia absolwentów po uzyskaniu dyplomu" ~ "Względny Wskaźnik Bezrobocia (WWB)",
          TRUE ~ "Wartość wskaźnika"
        )) +
        xlab("Rok uzyskania dyplomu") +
        theme_bw() +
        guides(color = guide_legend(title = "Kierunki studiów w porównaniu")) +
        scale_color_manual(values = color_palette) +
        scale_x_continuous(breaks = c(2015:2022), limits = c(2014.5, 2022.5))
      
      # Przekonwertowanie do wykresu interaktywnego
      ggplotly(comp_p, tooltip = "text")

# Przekonwertowanie do wykresu interaktywnego
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
            # usunięcie mediana_lata
            table_comparison <- data.frame(
                uczelnia = character(),
                kierunek = character(),
                poziomforma = character(),
                id = integer(),
                Akcja = character()
            ) %>% 
                rename(
                    "Uczelnia" = uczelnia, 
                    "Kierunek" = kierunek, 
                    "Poziom i forma studiów" = poziomforma 
                ) 
        } else {
            # Tworzenie tabeli z danymi, usunięcie mediana_lata
            table_comparison <- data %>% 
                select(uczelnia, kierunek, poziomforma, id) %>% 
                distinct() %>% 
                rename(
                    "Uczelnia" = uczelnia, 
                    "Kierunek" = kierunek, 
                    "Poziom i forma studiów" = poziomforma
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
                        targets = 4, #ukrycie kolumny ID
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
            zmienna = character()
        ))
    })
    
    
    
}

shinyApp(ui, server)

