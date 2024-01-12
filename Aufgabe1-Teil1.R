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
  mutate(Pclass = factor(Pclass, ordered = TRUE))
# Habs jetzt erstmal bis hier gemacht. Falls es noch Probleme gibt kommentiert einfach :)