## Cache and return the inverse of a matrix

## Return a list of three functions to: get a matrix, cache its inverse, and get its inverse

makeCacheMatrix <- function(some_matrix = matrix()) {
  inverse <- NULL
  get_matrix <- function() {
    some_matrix
  }
  set_inverse <- function(matrix_inverse) {
    inverse <<- matrix_inverse
  }
  get_inverse <- function() {
    inverse
  }
  cache_functions <- list(get_matrix = get_matrix,
                          set_inverse = set_inverse,
                          get_inverse = get_inverse
                          )
}


## Return the inverse of a matrix from the cache,
## if it is null, calculate the inverse, cache it, 
## and return the value

cacheSolve <- function(cache_functions, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- cache_functions$get_inverse()
  if(!is.null(inverse)) {
    return(inverse)
  }
  matrix <- cache_functions$get_matrix()
  matrix_inverse <- solve(matrix, ...)
  cache_functions$set_inverse(matrix_inverse)
  matrix_inverse
}
