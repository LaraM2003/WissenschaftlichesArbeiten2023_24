###############################################################################
####################### Wissenschaftliches Arbeiten ###########################
################################ WS 2023/24 ###################################
###############################################################################

###### Aufgabe 2 - Skript A

library(ggplot2)
library(gridExtra)

###  i

# Funktion zur Berechnung mehrerer deskr.Statistiken fuer metrische Variablen
i <- function(metrics){
  # Sortierung zur einfacheren Bestimmung mancher Werte
  sortedm <- quickSort(metrics)
  # Dataframe zur Darstellung der Ergebnisse
  data.frame(
    "Minimum"= sortedm[1], "Maximum"= sortedm[length(metrics)], 
    "Mean"= sum(metrics)/length(metrics),
    "Median"= sum(sortedm)/length(metrics)
  )
}

###  ii

# Funktion zur Berechnung mehrerer deskr.Statistiken fuer kategoriale Variablen

ii <- function(categorials){
  # Dataframe zur Darstellung der Ergebnisse
  data.frame(
    "Category"= unique(categorials), 
    "Count"= as.numeric(table(useNA ="ifany", categorials)),
    "Percent"= as.numeric(table(useNA ="ifany", categorials))
    /length(categorials)
  )
}

### iv


# Beschreibung: Funktion, welche für zwei Variablen eine deskriptive bivariate 
#               Statistik berechnet und ausgibt
# Input:        metrisch - Vektor mit Merkmalsausprägungen eines metrischen
#                          Merkmals
#               dichotom - Vektor mit Merkmalsausprägungen eines dichotomen
#                          Merkmals
# Output:       result   - 

iv <- function(metrisch, dichotom){
  
  # prüft, ob beide Merkmale die gleiche Zahl an Beobachtungen haben
  if (length(metrisch) != length(dichotom)){
    stop("Die Vektoren sind unterschiedlich lang")
  }
  
  
  daten <- data.frame(metrisch, dichotom)
  
  # Visualisierung
  plot1 <- ggplot(daten) +
    aes(x = metrisch, col = dichotom) +
    geom_boxplot() +
    scale_color_manual(values = c("red2", "red4"))
  
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
   dichotom = titanic_aufbereitet$Survived)

### v

# Beschreibung: plottet für vier kategoriale Merkmal Balkendiagramme
# Input:        dataframe mit vier kategorialen Merkmalen, benannt "a" - "d"
# output:       (Graphen)

v <- function(kategorial){
  
    # erstellt die 4 plots
    plot1 <- ggplot(kategorial) +
      aes(x = a) +
      geom_bar(fill = "red4")
  
    plot2 <- ggplot(kategorial) +
      aes(x = b) +
      geom_bar(fill = "red4")
    
    plot3 <- ggplot(kategorial) +
      aes(x = c) +
      geom_bar(fill = "red4")
    
    plot4 <- ggplot(kategorial) +
      aes(x = d) +
      geom_bar(fill = "red4")
  
  # plottet die vier plots
  grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
}


# beispiel Aufruf
v(data.frame("a" = titanic_aufbereitet$Survived, 
             "b" = titanic_aufbereitet$Sex, 
             "c" = titanic_aufbereitet$Embarked,
             "d" = titanic_aufbereitet$Pclass))
