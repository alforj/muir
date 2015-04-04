ff <- function(fun, col) {
        
        ## Create function filter as string for filtering values
        paste0(fun, "(", col, ")")
        
        
}

pct <- function(numerator, denominator, decimals = 2) {
        
        p <- format(round((numerator/denominator) * 100, decimals), nsmall=decimals)
        p
}
