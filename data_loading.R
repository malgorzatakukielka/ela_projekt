#ładowanie pakietów
library(tidyverse)

ela <- read.csv2("~/ela_projekt/graduates-major-data(1).csv")

ela1 <- select(ela, P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA, P_POZIOM, P_FORMA, P_ME_ZAR_P1, P_ME_ZAR_P2, P_ROKDYP) %>% 
  pivot_longer(cols = 5:6, names_to = "p", values_to = "me_zar") #wybór kolumn

ela1$p <- factor(ela1$p, levels = c("P_ME_ZAR_P1", "P_ME_ZAR_P2"), 
                 labels = c("Mediana zarobków w pierwszym roku od uzyskania dyplomu", 
                            "Mediana zarobków w drugim roku od uzyskania dyplomu"))
ela1$P_FORMA <- factor(ela1$P_FORMA, levels = c("S", "N"), 
                       labels = c("Stacjonarne", "Niestacjonarne"))
ela1$P_POZIOM <- factor(ela1$P_POZIOM, levels = c("1", "2", "JM"), 
                        labels = c("Studia I stopnia", 
                                   "Studia II stopnia", 
                                   "Studia jednolite magisterskie"))

ela1$P_POZIOMFORMA <- paste(ela1$P_POZIOM, ela1$P_FORMA, sep = ", ") 
#dodanie noej kolumny, zespolenie dotychcasowych kolumn P_POZIOM i P_FORMA

ela1 <- ela1[, c(1, 2, 8, 5, 6, 7)] #usunięcie kolumn forma i poziom

ela1 <- arrange(ela1, P_NAZWA_UCZELNI, P_KIERUNEK_NAZWA) 
#ułożenie alfabetycznie wg uczelni i kierunku

write.csv(ela1, "~/ela_projekt/ela1.csv", row.names=T)
#zapisanie nowego pliku
