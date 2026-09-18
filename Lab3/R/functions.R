#name
#liu-id

euclidean <- function(a, b){
    stopifnot(is.numeric(a), is.numeric(b))

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

    return(smaller)
}