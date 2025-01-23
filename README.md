# Ekonomiczne Losy Absolwentów – Aplikacja RShiny

## Opis projektu
Aplikacja RShiny pt. **„Ekonomiczne Losy Absolwentów”** została stworzona w celu eksploracji danych dotyczących różnych aspektów związanych z absolwentami uczelni wyższych w Polsce. Umożliwia użytkownikom analizowanie danych takich jak zarobki, wskaźniki zatrudnienia i inne dostępne zmienne w zależności od wybranej uczelni, kierunku studiów, poziomu i formy studiów. Dodatkowo oferuje możliwość porównywania wyników dla różnych kierunków i uczelni.

Dane wykorzystane w aplikacji pochodzą z **[Systemu Ekonomicznych Losów Absolwentów (ELA)](https://ela.nauka.gov.pl/)**, prowadzonego przez Ministerstwo Nauki i Szkolnictwa Wyższego.

Aplikacja jest narzędziem wspierającym podejmowanie decyzji przez kandydatów na studia, studentów oraz inne osoby zainteresowane danymi o rynku pracy absolwentów.

## Funkcje aplikacji
1. **Eksploracja danych absolwentów:**
   - Możliwość filtrowania danych według:
     - Uczelni,
     - Kierunku studiów,
     - Poziomu i formy studiów,
     - Okresu (lata ukończenia studiów),
     - Wybranej zmiennej (np. zarobki, wskaźnik zatrudnienia).
   - Wizualizacja danych za pomocą dynamicznych wykresów z wykorzystaniem biblioteki `plotly`.
   - Obliczanie średnich ważonych dla wybranych zmiennych w określonych latach.

2. **Porównywanie danych:**
   - Użytkownik może wybrać do 7 różnych kombinacji (kierunek, uczelnia, poziom i forma studiów) do porównania.
   - Wizualizacja porównań w formie interaktywnego wykresu liniowego.
   - Możliwość usuwania elementów z listy porównań.

3. **Dynamiczna interakcja z interfejsem:**
   - Wybór danych z wykorzystaniem intuicyjnych narzędzi takich jak `pickerInput`, `sliderInput`, oraz dynamiczne przyciski akcji.
   - Obsługa błędów i powiadomienia dla użytkownika (`shinyToastify`).

## Oprogramowanie
- **Języki programowania:**
  - R
- **Środowisko programistyczne:**
  - RStudio
- **Pakiety R:**
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

