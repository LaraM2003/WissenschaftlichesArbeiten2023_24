#####################
###Aufgabe 2.a.iii###
#####################

### SKRIPT 1



#Allgemeiner Pfad zur titanic.csv Datei                                                                 
titanic <- read.csv("Pfad/zur/titanic.csv")


#Funktion für deskriptive bivariate Statistiken zwischen zwei kategorialen Variablen
suitable_stats <- function(data, variable1, variable2){
  
  #Überprüfung, ob die beiden Variablen im Datensatz vorhanden sind
  if (!(variable1 %in% colnames(data) && variable2 %in% colnames(data))) {
    stop("Die angegebenen Variablen sind nicht im Datensatz vorhanden.")
  }
  
  #Überprüfung, ob die angegebenen Variablen kategorial sind
  if (!is.factor(data[[variable1]]) || !is.factor(data[[variable2]])) {
    stop("Die angegebenen Variablen müssen kategorial sein.")
  }
  
  #Erstellung einer Kreuztabelle
  cross_table <- table(data[[variable1]], data[[variable2]])
  
  #Durchführung des Chi-Quadrat-Tests
  chi_square_test <- chisq.test(cross_table)
  
  
  #Ausgabe der Ergebnisse
  cat("Kreuztabelle zwischen", variable1, "und", variable2, ":\n")
  print(cross_table)
  cat("\nChi-Quadrat-Test:\n")
  print(chi_square_test)
}


#Faktorisierung der kategorialen Variablen
titanic$Name <- as.factor(titanic$Name)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Ticket <- as.factor(titanic$Ticket)
titanic$Cabin <- as.factor(titanic$Cabin)
titanic$Embarked <- as.factor(titanic$Embarked)



#Beispielaufruf
suitable_stats(titanic, "Name", "Embarked")
