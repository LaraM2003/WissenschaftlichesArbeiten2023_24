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

## AK: weitere Hilfsfunktionen bitte hier eingfuegen (inkl. Doku)