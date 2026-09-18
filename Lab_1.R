
name<- "Giulio Collazuol"
liuid<-"giuco986"


my_num_vector <- function() {
  a = log10(11)
  b = cos(pi / 5)
  c = exp(pi /3)
  d = (1173 %% 7)/19
  return(c(a,b,c,d))
}

filter_my_vector <- function(x, leq) {
  vettore = c()
  for(i in x){
    if (i < leq){
      vettore = c(vettore,i)
    }
    else {
      vettore = c(vettore, NA)
    }
  }
  return(vettore)
}

dot_prod <- function(a, b) {
  return(sum(a * b))
}

approx_e <- function(N){
  risultato = 0
  for(i in 0:N){
    risultato = risultato + 1 / factorial(i)
  }
  return(risultato)
}
# For N => 9 the result shown does not change because it changes on decimals not visible by R approximation

my_magic_matrix <- function(){
  return(matrix(c(4,3,8,9,5,1,2,7,6), nrow = 3, ncol = 3))
}
#The sum of each column and each row in fifteen

calculate_elements <- function(A) {
  return(nrow(A) * ncol(A))
}

row_to_zero <- function(A, i){
  B = A
  B[i,] = 0
  return(B)
}

add_elements_to_matrix <- function(A, x, i, j){
  B = A
  B[i,j]= B[i,j] +x
  return(B)
}

my_magic_list <- function(){
  lista = list("info"="my own list",my_num_vector(),my_magic_matrix())
  return(lista)
}


change_info <- function(x, text){
  x["info"] = text
  return(x)
}

add_note <- function(x, note){
  x <- append(x, list("note"=note))
  return(x)
}

sum_numeric_parts <- function(x){
  y=as.numeric(unlist(x))
  return(sum(y, na.rm = TRUE))
  
}

my_data.frame <- function(){
  id <- 1:3
  income <- c(7.30, 0.00, 15.21)
  name <- c('John', 'Liza', 'Azra')
  rich <- c(FALSE,FALSE,TRUE)
  return(data.frame(id,name,income,rich))
}

sort_head <- function(df, var.name, n) {
  
  head(df[order(df[[var.name]], decreasing = TRUE), ], n)
  
}


add_median_variable <- function(df, j) {
  mediana <- median(df[[j]])
  df$compared_to_median <- character(length(df[[j]]))
  for(i in 1:length(df$compared_to_median)){
    if(df[[j]][i] >mediana){
      df$compared_to_median[i] <- "Greater"
    }
    if(df[[j]][i] < mediana){
      df$compared_to_median[i]  <- "Smaller"
    }
    if(df[[j]][i]  ==mediana){
      df$compared_to_median[i]  <- "Median"
    }
  }
  return(df)
}




analyze_columns <- function(df, j) {
  vettore1 <- c(mean = mean(df[[j[1]]]),median = median(df[[j[1]]]),sd = sd(df[[j[1]]]))
  vettore2 <- c(mean = mean(df[[j[2]]]),median = median(df[[j[2]]]),sd = sd(df[[j[2]]]))
  result <- list(vettore1,vettore2,correlation_matrix = cor(df[,j]))
  names(result)[1:2] <- names(df)[j]
  return(result)
}
