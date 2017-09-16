## Put comments here that give an overall description of what your
## functions do

## This function checks to see if inverse for x is saved in cache.
## If inverse exists already, "Inv.exist" is set to NULL. 
## If not, "Inv.exist" is loaded with matric "x".

makeCacheMatrix <- function(x = matrix()) {
  Inv.exist <- NULL
  set <- function(y) {
    x <<- y
    Inv.exist <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(mat_inverse) Inv.exist <<- mat_inverse
  
  get_inverse <- function() Inv.exist
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function.
## This function will return cached data for matrix "x" if
##   inverse is available in cache, otherwise it will compute
##   the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv.exist <- x$get_inverse()
  if(!is.null(Inv.exist)) {
    message("getting cached data")
    return(Inv.exist)
  }
  data <- x$get()
  Inv.exist <- solve(data, ...)
  x$set_inverse(Inv.exist)
  Inv.exist
}
