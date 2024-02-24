# Aufgabe 2 - Skript B

# Quick Sort Algorithmus
# quickSort:
#       Hilfsfunktion: Sortiert einen Vektor mit numerischen Eintraegen 
#       aufsteigend mittels Quick Sort Algorithmus.
# Eingabe:
#       arr: Vektor mit numerischen Eintraegen
# Ausgabe:
#       aufsteigend sortierter Vektor

quickSort <- function(arr) {
    if (length(arr) <= 1) {
        return(arr)
    } else {
        pivot <- arr[1]
        smaller <- arr[arr < pivot]
        equal <- arr[arr == pivot]
        greater <- arr[arr > pivot]
        
        return(c(quickSort(smaller), equal, quickSort(greater)))
    }
}

# Median Algorithmus
# newMedian:
#       Zur berechnung des Medians eines Vektors mit numerischen Werten.
# Eingabe:
#       numr: Vektor mit numerischen Eintraegen.
# Ausgabe:
#       Median des Vektors.

newMedian <- function(numr) {
  numr <- quickSort(numr)
  if((length(numr) %% 2 )== 0) {
    return((numr[length(numr) / 2] + numr[(length(numr) / 2) + 1]) /2)
  }
  else {
    return(numr[(length(numr) + 1)/ 2])
  }
}
