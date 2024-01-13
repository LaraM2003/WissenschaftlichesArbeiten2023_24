###############################################################################
####################### Wissenschaftliches Arbeiten ###########################
################################ WS 2023/24 ###################################
###############################################################################

# Pakete laden
library("dplyr")
library("tidyr")
library("stringr")

# Daten einlesen
titanic <- read.csv("titanic.csv")

# Variablen transformieren
titanic_aufbereitet <- titanic %>%
    # Extrahiere Titel aus der Namensspalte
    mutate(Title = str_extract(titanic$Name, "\\w+(?=\\.)")) %>%
    # Transformiere  Survived, Sex und Embarked in factor
    mutate(across(c("Survived", "Sex", "Embarked"), as.factor)) %>%
    # Transformiere  Pclass in ordered factor
    mutate(Pclass = factor(Pclass, ordered = TRUE)) %>%
    # Imputiere fehlende Alterswerte (Age) mit Gruppenmittelwert
    group_by(Title) %>%
    mutate(Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>%
    ungroup () %>%
    # Extrahiere und erstelle Deck-Variable
    mutate(Deck = str_extract(Cabin, "[A-Za-z](?=[0-9])")) %>%
    mutate(Zimmer = gsub("[[:alpha:]]", "", Cabin)) %>% # Hilfsvariable Zimmer
    # Extrahiere und erstelle Zimmer-Spalten
    separate(Zimmer, into = (c("Zimmer_1", "Zimmer_2", "Zimmer_3", "Zimmer_4")), sep = " ") %>%
    mutate(across(c("Zimmer_1", "Zimmer_2", "Zimmer_3", "Zimmer_4"), as.numeric)) %>%
    # Bestimmme Seite laut Zimmernummer
    mutate(across(c("Zimmer_1", "Zimmer_2", "Zimmer_3", "Zimmer_4"),
                  function(Zimmer)
                      ifelse(Zimmer  %% 2 == 1, "Steuerbord", "Backbord"))) %>%
    # Waehle vorgegebene Spalten aus
    select(!c("PassengerId", "Name", "Ticket", "Cabin"))


# Aufbereiteten Datensatz exportieren
write.csv(titanic_aufbereitet, "titanic_aufbereitet.csv")