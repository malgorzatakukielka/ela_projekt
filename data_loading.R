#ładowanie pakietów
library(tidyverse)

ela_nowe <- read.csv2("~/ela_projekt/graduates-major-data-nowe.csv")

# ela1 <- select(ela_nowe, P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA, P_POZIOM, P_FORMA, 
#                P_ME_ZAR_P1, P_ME_ZAR_P2, P_ROKDYP, P_NAZWA_JEDN, 
#                P_KIERUNEK_ID, P_NAZWA_KIERUNKU_PELNA, P_N) %>% 
#   pivot_longer(cols = 5:6, names_to = "p", values_to = "me_zar") #wybór kolumn



ela1 <- select(ela_nowe, P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA, P_POZIOM, P_FORMA, 
               P_ROKDYP, P_NAZWA_JEDN, P_KIERUNEK_ID, 
               P_NAZWA_KIERUNKU_PELNA, P_N, #dane identyfikacyjne
               P_ME_ZAR_P1, P_ME_ZAR_P2, P_ME_ZAR_P3, P_ME_ZAR_P4, P_ME_ZAR_P5,
               P_CZAS_PRACA, P_CZY_BEZR, P_WWZ, P_WWB) %>% #zmienne do porównania
  pivot_longer(cols = 10:18, names_to = "zmienna", values_to = "wartosc")



ela1$zmienna <- factor(ela1$zmienna, levels = c("P_ME_ZAR_P1", "P_ME_ZAR_P2", 
                                                "P_ME_ZAR_P3", "P_ME_ZAR_P4", 
                                                "P_ME_ZAR_P5", "P_CZAS_PRACA", 
                                                "P_CZY_BEZR", "P_WWZ", "P_WWB"), 
                 labels = c("Mediana zarobków w pierwszym roku od uzyskania dyplomu", 
                            "Mediana zarobków w drugim roku od uzyskania dyplomu",
                            "Mediana zarobków w trzecim roku od uzyskania dyplomu",
                            "Mediana zarobków w czwartym roku od uzyskania dyplomu",
                            "Mediana zarobków w piątym roku od uzyskania dyplomu",
                            "Średni czas (w miesiącach) od uzyskania dyplomu do podjęcia pierwszej pracy po uzyskaniu dyplomu",
                            "Odsetek absolwentów z doświadczeniem bezrobocia po uzyskaniu dyplomu",
                            "Względny Wskaźnik Zarobków absolwentów po uzyskaniu dyplomu",
                            "Względny Wskaźnik Bezrobocia absolwentów po uzyskaniu dyplomu"
                            ))
ela1$P_FORMA <- factor(ela1$P_FORMA, levels = c("S", "N"), 
                       labels = c("Stacjonarne", "Niestacjonarne"))
ela1$P_POZIOM <- factor(ela1$P_POZIOM, levels = c("1", "2", "JM"), 
                        labels = c("Studia I stopnia", 
                                   "Studia II stopnia", 
                                   "Studia jednolite magisterskie"))

ela1$P_POZIOMFORMA <- paste(ela1$P_POZIOM, ela1$P_FORMA, sep = ", ") 
#dodanie nowej kolumny, zespolenie dotychcasowych kolumn P_POZIOM i P_FORMA

ela1 <- select(ela1, -P_POZIOM, -P_FORMA) %>% 
  relocate(P_POZIOMFORMA, .before = P_ROKDYP)

ela1 <- arrange(ela1, P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA)
#ułożenie alfabetycznie wg uczelni i kierunku

write.csv(ela1, "~/ela_projekt/ela1.csv", row.names=T)
#zapisanie nowego pliku
