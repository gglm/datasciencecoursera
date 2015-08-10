## These functions cache inverse matrix values to save on calculation time when possible

## create a square matrix, which is a list containing a function to
## 1.  set the value of the matrix (set_matrix)
## 2.  get the value of the matrix (get_matrix)
## 3.  set the value of the inverse matrix (set_inverse_matrix)
## 4.  get the value of the inverse matrix (get_inverse_matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set_matrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  get_matrix <- function() x
  set_inverse_matrix <- function(inv) i <<- inv
  get_inverse_matrix<- function() i
  list(
    set_matrix = set_matrix, 
    get_matrix = get_matrix, 
    set_inverse_matrix = set_inverse_matrix, 
    get_inverse_matrix = get_inverse_matrix
  )
}


## Calculates the inverse of the matrix created above.  
## If the inverse has already been found, R will return that from the cache.
## Otherwise, the inverse matrix will be calculated.

cacheSolve <- function(x, ...) {
  i <- x$get_inverse_matrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get_matrix()
  i <- solve(data, ...)
  x$set_inverse_matrix(i)
  i
}  

