###############################################################################
####################### Wissenschaftliches Arbeiten ###########################
################################ WS 2023/24 ###################################
###############################################################################

# Pakete laden
library("dplyr")
library("tidyr")
library("stringr")

# lesenUndTransformieren:
#       Funktion zum Einlesen des Titanic-Datensatzes und Variablen-
#       transformation gemaess Aufgabe 1 der Aufgabenstellung 
#       (Wissenschaftliches Arbeiten WS 23/24)
# Eingabe:
#       path: (Relativer) Dateipfad; Zeichenkette
#       filename: Dateiname; Zeichenkette
# Ausgabe:
#       Aufbereiteter Titanic-Datensatz

lesenUndTransformieren <- function(path, filename){
    # Daten einlesen
    titanic <- read.csv(paste(path, filename, sep=""))
    
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
        mutate(Room = gsub(" ", " ",gsub("[[:alpha:]]", "", Cabin))) %>% # Hilfsvariable Room 
        # Extrahiere und erstelle Room-Spalten
        separate(Room, into = (c("Room_1", "Room_2", "Room_3", "Room_4")), sep = " ") %>%
        mutate(across(c("Room_1", "Room_2", "Room_3", "Room_4"), as.numeric)) %>%
        # Bestimmme Seite laut Zimmernummer
        mutate(across(c("Room_1", "Room_2", "Room_3", "Room_4"),
                      function(Room)
                          ifelse(Room  %% 2 == 1, "Starboard", "Port"))) %>%
        # Waehle vorgegebene Spalten aus
        select(!c("PassengerId", "Name", "Ticket", "Cabin"))
}

# Lese Daten ein und transformiere sie
titanic_aufbereitet <- lesenUndTransformieren("Data/", "titanic.csv")

# Aufbereiteten Datensatz exportieren
# Bei Bedarf bitte auskommentieren
#write.csv(titanic_aufbereitet, "Data/titanic_aufbereitet.csv")