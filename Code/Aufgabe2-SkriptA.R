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


###  i
# statsmetric:  Eine Funktion, die deskriptive Statistiken fuer metrische
#               Variablen berechnet und ausgibt.
# Input:        metrics - Vektor mit metrischen Daten
# Output:       Angaben zum Minimum,Maximum, arithm.Mittel und Median der
#               Werte; Dataframe mit numerischen Eintraegen


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
# statscategorials: 
#               Eine Funktion, die deskriptive Statistiken fuer kategoriale
#               Variablen berechnet und ausgibt.
# Input:        categorials - Vektor mit kategorischen Daten
# Output:       Angaben zu jedem einzigartigem Eintrag des Vektors. Dazu jeweils
#               Anzahl und relative Haeufigkeit; Dateframe mit numerischen 
#               Eintraegen

statscategorials <- function(categorials){
  # Dataframe zur Darstellung der Ergebnisse
  data.frame(
    "Category"= table(categorials),
    "Percent"= table(categorials)/length(categorials)
  )
}


### iii

# Beschreibung: 
# statsTwoCategorials:
#               Eine Funktion, die passende deskriptive bivariate Statistiken 
#               erstellt und ausgibt, um den Zusammenhang zwischen zwei 
#               kategorialen Variablen zu analysieren.
# Input:        kategorial1 & kategorial2 - Vektor, der die Ausprasgungen eines 
#               kategorialen Merkmals enthaslt.
# Output:       Kreuztabelle und Statistiken des T-Quadrat-Tests


statsTwoCategorials <- function(kategorial1, kategorial2){
  
  # Erstellung einer Kreuztabelle
  cross_table <- table(kategorial1, kategorial2)
  print(cross_table)
  
  
  #Durchfuehrung des Chi-Quadrat-Tests
  chi_square_test <- chisq.test(cross_table)
  print(chi_square_test)
}


### iv
# Beschreibung: 
# statsDichoMetric:
#               Funktion, welche fuer zwei Variablen eine deskriptive bivariate 
#               Statistik berechnet und ausgibt
# Input:        metrisch - Vektor mit Merkmalsausprasgungen eines metrischen
#                          Merkmals
#               dichotom - Vektor mit Merkmalsausprasgungen eines dichotomen
#                          Merkmals
# Output:       Tabelle mit Kennzahlen (Mittelwert, Varianz und Std.abw.) sowie
#               Boxplots (gruppiert nach der dich. Variable).


statsDichoMetric <- function(metrisch, dichotom, labelX){
  
  # prueft, ob beide Merkmale die gleiche Zahl an Beobachtungen haben
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


### v

# Beschreibung: 
# plotCategorials: 
#               plottet fuer drei kategoriale Merkmal Balkendiagramme
# Input:        dataframe mit vier kategorialen Merkmalen, benannt "a" - "c"
# output:       (3 Graphen) Die die absolute Anzahl der jeweiligen Kategorien
#               darstellt.

plotCategorials <- function(kategorial, Name1, Name2, Name3){
  
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


### vi

# Beschreibung: 
#   plotAgeSex: 
#           plottet die Alters- und Geschlechtsstrukturen
# Input:        -
# output:   Absolute Anzahl an Passagieren (in Altersgruppen von 5 Jahren/ unter
#           -teilt nah Geschlecht)
plotAgeSex <- function(){
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

# Beschreibung: 
# plotRoom:
#           plottet die Ueberleben und die Seite, auf der die Kabine lag
# Input:        -
# output:   Anzahl an Ueberlebenden und Verstorbenen je nach Zimmerseite.

plotRoom <- function(){
  ggplot(data = titanic_aufbereitet, aes(x = Room_1, fill = Survived)) + 
    geom_bar(stat = "count", color = "black") +
    scale_fill_manual(values = c("red2", "red4"))
}

