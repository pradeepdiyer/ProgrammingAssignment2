## makeCacheMatrix can set and get the value of a matrix created in it's parent environment.
## It can also set and get the value of the inverse of that matrix.
## cacheSolve returns the inverse of the special "matrix" from above by either computing it or retrieving it from cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(matrixInverse) inv <<- matrixInverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of the special "matrix" from above by either computing it or retrieving it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
