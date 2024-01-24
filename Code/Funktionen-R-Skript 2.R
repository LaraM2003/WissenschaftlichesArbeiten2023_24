#####################
###Aufgabe 2.a.iii###
#####################

### SKRIPT 2




#Helfer-Funktion für "Funktionen-R-Skript 1"
helper_function <- function(data) {
  
  #Berechnung der Anzahl fehlender Werte in einem bestimmten Datensatz
  missing_values_count <- sum(is.na(data))
  
  #Ausgabe
  cat("Anzahl der fehlenden Werte im Datensatz:", missing_values_count, "\n")
  
  #Rückgabe des Ergebnisses oder weiterer benötigter Daten
  return(list(missing_values = missing_values_count))
}

helper_function(titanic)
