# Ekonomiczne Losy Absolwentów – Aplikacja RShiny

## Opis projektu
Aplikacja RShiny pt. **„Ekonomiczne Losy Absolwentów”** została stworzona w celu eksploracji danych dotyczących zarobków absolwentów uczelni wyższych w Polsce. Umożliwia użytkownikom analizowanie danych w zależności od wybranej uczelni, kierunku studiów, poziomu i formy studiów, a także porównywanie wyników dla różnych kierunków i uczelni.

Aplikacja jest narzędziem wspierającym podejmowanie decyzji przez kandydatów na studia, studentów oraz inne osoby zainteresowane danymi o rynku pracy absolwentów.

## Funkcje aplikacji
1. **Eksploracja zarobków absolwentów:**
   - Możliwość filtrowania danych według:
     - Uczelni,
     - Kierunku studiów,
     - Poziomu i formy studiów,
     - Okresu (lata ukończenia studiów).
   - Wizualizacja danych za pomocą dynamicznych wykresów z wykorzystaniem biblioteki `plotly`.
   - Obliczanie średniej ważonej zarobków absolwentów w wybranych latach.

2. **Porównywanie danych:**
   - Użytkownik może wybrać do 7 różnych kombinacji (kierunek, uczelnia, poziom i forma studiów) do porównania.
   - Wizualizacja porównań w formie interaktywnego wykresu liniowego.
   - Możliwość usuwania elementów z listy porównań.

3. **Dynamiczna interakcja z interfejsem:**
   - Wybór danych z wykorzystaniem intuicyjnych narzędzi takich jak `pickerInput`, `sliderInput`, oraz dynamiczne przyciski akcji.
   - Obsługa błędów i powiadomienia dla użytkownika (`shinyToastify`).

## Oprogramowanie:
  - R
  - RStudio
  - Pakiety R:
    - `shiny`
    - `ggplot2`
    - `plotly`
    - `tidyverse`
    - `shinyjs`
    - `bslib`
    - `shinyToastify`
    - `DT`
    - `data.table`
    - `scales`
    - `shinyWidgets`



