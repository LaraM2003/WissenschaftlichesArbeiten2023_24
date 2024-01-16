###############################################################################
####################### Wissenschaftliches Arbeiten ###########################
################################ WS 2023/24 ###################################
###############################################################################

###### Aufgabe 2 - Skript A

library(ggplot2)

### iv

# Beschreibung: Funktion, welche für zwei Variablen eine deskriptive bivariate 
#               Statistik berechnet und ausgibt
# Input:        metrisch - Vektor mit Merkmalsausprägungen eines metrischen
#                          Merkmals
#               dichotom - Vektor mit Merkmalsausprägungen eines dichotomen
#                          Merkmals
# Output:       result   - 

iv <- function(metrisch, dichotom){
  if (length(metrisch) != length(dichotom)){
    stop("Die Vektoren sind unterschiedlich lang")
  }
  
  
  daten <- data.frame(metrisch, dichotom)
  
  # Visualisierung
  ggplot(daten) +
    aes(y = dichotom, x = metrisch) +
    geom_boxplot()
  
  
  
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

## Grafik 1 - Verteilung Geschlecht
plot(table(titanic_aufbereitet$Sex)) ### noch schöner machen


## Grafik 2 - Verteilung Embarked
plot(table(titanic_aufbereitet$Embarked)) ### noch schöner machen


## Grafik 3 - Verteilung pClass
plot(table(titanic_aufbereitet$Pclass)) ### noch schöner machen


## Grafik 4 - Verteilung survived
plot(table(titanic_aufbereitet$Survived)) ### noch schöner machen
