# Aufgabe 2 - Skript B

# Quick Sort Algorithmus
quickSort <- function(arr) {
    if (length(arr) <= 1) {
        return(arr)
    } else {
        pivot <- arr[1]
        smaller <- arr[arr < pivot]
        equal <- arr[arr == pivot]
        greater <- arr[arr > pivot]
        
        return(c(quick_sort(smaller), equal, quick_sort(greater)))
    }
}
