
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #initiate the inverse variable
  inv <- NULL
  #store matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #basic get/set functions
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  #list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  #if inverse has already been calculated, return it
  if (!is.null(inv)) {
    message("Getting Cached Information")
    return(inv)
  }
  
  #if inverse hasn't been calculated, then calculate it, store it and return it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
