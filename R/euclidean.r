#' Euclidean algorithm for GCD
#'
#'This function implements euclidean algorithm to calculate the GCD of two given numbers (a, b).
#'This function iteratively calculates the remainder of the whole division between the previous two remainders.
#'The output is the last non-zero remainder
#'
#' @param a integer scalar
#' @param b integer scalar
#' @return  integer scalar
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
#' @export
euclidean <- function(a, b){
    stopifnot(is.numeric(a), is.numeric(b), a%%1 == 0, b%%1 == 0, length(a)==1, length(b)==1)

    if(b>=a){
        smaller<-a
        bigger<-b
    }
    else {
        smaller<-b
        bigger<-a
    }

    while((bigger %% smaller) != 0){
        rest <- bigger %% smaller
        bigger <- smaller
        smaller <- rest
    }

    return(abs(smaller))
}