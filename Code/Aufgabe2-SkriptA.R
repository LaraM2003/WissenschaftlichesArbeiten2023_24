###############################################################################
####################### Wissenschaftliches Arbeiten ###########################
################################ WS 2023/24 ###################################
###############################################################################

###### Aufgabe 2 - Skript A


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
  
  
  
  plot(metrisch, dichotom)
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
