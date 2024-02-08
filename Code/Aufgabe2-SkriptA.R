###############################################################################
####################### Wissenschaftliches Arbeiten ###########################
################################ WS 2023/24 ###################################
###############################################################################

###### Aufgabe 2 - Skript A

# Lade Pakete
library(ggplot2)
library(gridExtra)
# Lade Hilfsfunktionen
source("Code/Aufgabe2-SkriptB.R")

## An Alle: Bitte sprechende Funktionsnamen waehlen (und dann bitte ueberall im
# Code aendern; auch in den anderen Skripts)
## AK: ich wuerde die Beispielaufrufe auskommentieren oder besser loeschen.
# Skripte sind so nicht alleinstehend lauffaehig bzw. werfen Fehler, da man 
# davon aussgeht das der Datensatz geladen wurde.

###  i
# Beschreibung: Eine Funktion, die deskriptive Statistiken fuer metrische
#               Variablen berechnet und ausgibt.
# Input:        metrics - Vektor mit metrischen Daten
# Output:       Angaben zum Minimum,Maximum, arithm.Mittel und Median der
#               Werte.


# Funktion zur Berechnung mehrerer deskr.Statistiken fuer metrische Variablen
statsmetric <- function(metrics){
  # Sortierung zur einfacheren Bestimmung mancher Werte
  sortedm <- quickSort(metrics)
  # Dataframe zur Darstellung der Ergebnisse
  data.frame(
    "Minimum"= sortedm[1], 
    "Maximum"= sortedm[length(metrics)], 
    "Mean"= sum(metrics)/length(metrics),
    "Median"= newMedian(metrics)
  )
}

###  ii
# Beschreibung: Eine Funktion, die deskriptive Statistiken fuer kategoriale
#               Variablen berechnet und ausgibt.
# Input:        categorials - Vektor mit kategorischen Daten
# Output:       Angaben zu jedem einzigartigem Eintrag des Vektors. Dazu jeweils
#               Anzahl und relative Haeufigkeit.

statscategorials <- function(categorials){
  # Dataframe zur Darstellung der Ergebnisse
  data.frame(
    "Category"= table(categorials),
    "Percent"= table(categorials)/length(categorials)
  )
}


### iii


# Beschreibung: Eine Funktion, die passende deskriptive bivariate Statistiken 
#               erstellt und ausgibt, um den Zusammenhang zwischen zwei 
#               kategorialen Variablen zu analysieren.
# Input:        kategorial1 & kategorial2 - Vektor, der die Ausprägungen eines 
#               kategorialen Merkmals enthält.
# Output:       result ## AK: Was sagen denn die Tabellen aus?


iii <- function(kategorial1, kategorial2){
  
  # Erstellung einer Kreuztabelle
  cross_table <- table(kategorial1, kategorial2)
  print(cross_table)
  
  
  #Durchführung des Chi-Quadrat-Tests
  chi_square_test <- chisq.test(cross_table)
  print(chi_square_test)
}



### iv
## AK: Umlaute bitte mit ae, oe, ue umschreiben
# Beschreibung: Funktion, welche für zwei Variablen eine deskriptive bivariate 
#               Statistik berechnet und ausgibt
# Input:        metrisch - Vektor mit Merkmalsausprägungen eines metrischen
#                          Merkmals
#               dichotom - Vektor mit Merkmalsausprägungen eines dichotomen
#                          Merkmals
# Output:       result   - ## AK: Was sagt mir denn die ausgegebene Tabelle?



iv <- function(metrisch, dichotom, labelX){
  
  # prüft, ob beide Merkmale die gleiche Zahl an Beobachtungen haben
  if (length(metrisch) != length(dichotom)){
    stop("Die Vektoren sind unterschiedlich lang")
  }
  
  
  daten <- data.frame(metrisch, dichotom)
  
  # Visualisierung
  plot1 <- ggplot(daten) +
    aes(x = metrisch, col = dichotom) +
    geom_boxplot() +
    scale_color_manual(values = c("red2", "red4")) +
    labs(x = labelX)
  
  print(plot1)
  
  # Einteilung nach dichotom
  dichotomGruppe1 <- daten[which(daten[,2] == daten[1,2]), ]
  dichotomGruppe2 <- daten[which(daten[,2] != daten[1,2]), ]
  
  tabelle <- matrix(0, nrow = 3, ncol = 2)
  colnames(tabelle) <- c(dichotomGruppe1[1,2], dichotomGruppe2[1,2])
  rownames(tabelle) <- c("mittel", "varianz", "standardabweichung")
  
  # Mittelwerte gruppiert nach dichotom
  tabelle[1,] <- c(mean(dichotomGruppe1[,1]), mean(dichotomGruppe2[,1]))
  
  # Varianzen gruppiert nach dichotom
  tabelle[2,] <- c(var(dichotomGruppe1[,1]), var(dichotomGruppe2[,1]))
  
  # standardabweichung gruppiert nach dichotom
  tabelle[3,] <- c(sd(dichotomGruppe1[,1]), sd(dichotomGruppe2[,1]))

  
  return(list("Kennzahlen aufgeteilt nach dichotom" = tabelle))
  
}


# Beispiel Aufruf
iv(metrisch = titanic_aufbereitet$Age,
   dichotom = titanic_aufbereitet$Survived, "Alter")

### v

# Beschreibung: plottet für drei kategoriale Merkmal Balkendiagramme
# Input:        dataframe mit vier kategorialen Merkmalen, benannt "a" - "c"
# output:       (Graphen) ## AK: Auch hier was zeigen mir die Plots? X-Y-Achsen?

v <- function(kategorial, Name1, Name2, Name3){
  
    # erstellt die 3 plots
    plot1 <- ggplot(kategorial) +
      aes(x = a) +
      geom_bar(fill = "red4") +
      labs(x = Name1)
  
    plot2 <- ggplot(kategorial) +
      aes(x = b) +
      geom_bar(fill = "red4") +
      labs(x = Name2)
    
    plot3 <- ggplot(kategorial) +
      aes(x = c) +
      geom_bar(fill = "red4") +
      labs(x = Name3)
  
  # plottet die vier plots
  grid.arrange(plot1, plot2, plot3, ncol = 2, nrow = 2)
}


# beispiel Aufruf
v(data.frame("a" = titanic_aufbereitet$Survived, 
             "b" = titanic_aufbereitet$Sex,
             "c" = titanic_aufbereitet$Pclass))


### vi

# Beschreibung: plottet die Alters- und Geschlechtsstrukturen
# Input:        -
# output:       -
vi <- function(){
  ggplot(data = titanic_aufbereitet, aes(x = Age, fill = Sex)) + 
    geom_histogram(binwidth = 5, color = "black") +
    labs(
      title = "Alters- und Geschlechterstruktur (auf der Titanic)",
      x = "Alter (in Jahren)",
      y = "Anzahl",
      fill = "Geschlecht"
    ) +
    theme_classic() +
    scale_fill_manual(values=c("red2", "red4"))
}


### vii

# Beschreibung: plottet die Überleben und die Seite, auf der die Kabine lag
# Input:        -
# output:       -

vii <- function(){
  ggplot(data = titanic_aufbereitet, aes(x = Room_1, fill = Survived)) + 
    geom_bar(stat = "count", color = "black") +
    scale_fill_manual(values = c("red2", "red4"))
}
