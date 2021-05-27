## Cache time-consuming functions and retrieve computed variables from the cache

## This function creates a special "matrix"  object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<-function() x
  setInv <- function(solveMatrix) inv <<- solveMatrix
  getInv <- function() inv
  list(set = set, get = get,setInv = setInv,getInv = getInv)
}


## This function computes the inverse of the special "matric" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
