#' Dijkstra's algorithm for minimum distances in a graph.
#'
#'This function implements Dijkstra's algorithm finding the shortest path between two nodes in a graph.
#'For each node in the graph, the function updates the distances vector if it finds a new shortest path to its connected nodes.
#'
#' @param graph data_frame with three columns v1, v2, w. The three columns describe the nodes of an edge and its weight
#' @param init_node node from which we want to calculate the distances from
#' @return vector of dimension of the number of nodes in the graph
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @export
dijkstra <- function(graph, init_node){
    stopifnot(is.data.frame(graph), identical(names(graph), c("v1","v2","w")), init_node%%1==0, is.numeric(init_node),
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