is_prime <- function(num) {
  stopifnot(is.numeric(num), num%%1==0, num>=0)
  t_val <- TRUE
  if(num<2){
    t_val <- FALSE
  } else if(num>2) {
    for(i in 2:sqrt(num)) {
      if(num%%i==0){
        t_val <- FALSE
        break
      }
    }
  }
  return(t_val)
}

is_prime(13)

a <- seq(5, 2)
demo_func_1 <- function(x) {
  x[2] <- -10
  print(x)
}
demo_func_1(a)

no_input <- function(x) {
  100
}
print(no_input())

subtraction_function <- function(num_to_sub) {
  output_function <- function(x) {
    return(x - num_to_sub)
  }
  return(output_function)
}
a <- 1
f1 <- subtraction_function(a)
f1(2)
a <- 2
f1 <- subtraction_function(a)
f1(2)

