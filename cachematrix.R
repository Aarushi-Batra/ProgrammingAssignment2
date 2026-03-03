## This file contains two functions:
## 1. makeCacheMatrix: creates a special matrix object that can cache its inverse.
## 2. cacheSolve: computes the inverse of the matrix returned by makeCacheMatrix.
##    If the inverse has already been calculated, it retrieves it from the cache
##    instead of recomputing it.


## Creates a special "matrix" object that can store its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inverse_store <- NULL   # cached inverse
  
  # Set a new matrix and reset cached inverse
  setMatrix <- function(new_matrix) {
    x <<- new_matrix
    inverse_store <<- NULL
  }
  
  # Get the current matrix
  getMatrix <- function() {
    x
  }
  
  # Store the inverse
  setInverseValue <- function(inv_matrix) {
    inverse_store <<- inv_matrix
  }
  
  # Retrieve the cached inverse
  getInverseValue <- function() {
    inverse_store
  }
  
  # Return functions as a list
  list(
    set = setMatrix,
    get = getMatrix,
    setInverse = setInverseValue,
    getInverse = getInverseValue
  )
}


## Computes the inverse of the special matrix
## If cached, returns stored inverse instead of recomputing
cacheSolve <- function(x, ...) {
  
  cached_value <- x$getInverse()
  
  # Return cached inverse if it exists
  if (!is.null(cached_value)) {
    message("inverse retrieved from cache")
    return(cached_value)
  }
  
  # Otherwise compute inverse
  matrix_data <- x$get()
  computed_inverse <- solve(matrix_data, ...)
  
  # Store computed inverse
  x$setInverse(computed_inverse)
  
  computed_inverse
}
