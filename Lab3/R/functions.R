#name
#liu-id

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

    return(smaller)
}

dijkstra <- function(graph, init_node){
    stopifnot(is.data.frame(graph), names(graph)==c("v1","v2","w"), init_node%%1==0, is.numeric(init_node), 
              length(init_node)==1, init_node %in% append(wiki_graph[[1]], wiki_graph[[2]]) )


    # Initialize the vector with the vertices and the weights. The weights are all Infinity except for the starting node
    vertices <- unique(append(graph[[1]], graph[[2]]))
    weight_vector <- c(1 : length(vertices)) *Inf
    weight_vector[init_node] <- 0

    while (length( vertices) != 0){
        # Check which vertex has the minimum weight and remove it from the search list
        position <- which(weight_vector[vertices] %in% min(weight_vector[vertices]))[1]
        u <- vertices[ position  ]
        vertices <- vertices[-position]

        # We look for every edge
        for(i in 1:length(graph[[1]])){
            # If my edge starts in u
            if(graph[[1]][i] == u){
                # If getting to V is greater than going from source to U and then to V
                v <- graph[[2]][i]
                w <- graph[[3]][i]

                if(weight_vector[v] >  w + weight_vector[u]){
                    weight_vector[v] <- w + weight_vector[u]
                }
            }
        }
    }

    return(weight_vector)
}


# wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#                          v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#                          w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))