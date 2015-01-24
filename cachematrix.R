## Pair of functions to allow for computation of matrix inversion with caching of result

## Create a matrix with getters & setters for solve (matrix inverse)
## Allows inversion to be stored
makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  ## Getter/Setters for stored inverse
  get <- function() x
  setsolve <- function(solve) matInverse <<- solve
  getsolve <- function() matInverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Solve (invert) a matrix, with caching capability
## check to see if the inverse is already stored via the getsolve function
## If is is return that inverse.   If not compute inverse and set stored inverse
cacheSolve <- function(x, ...) {
  matInverse <- x$getsolve()
  ## Check if chached inverse is present
  if(!is.null(matInverse)) {
    message("getting cached inverse")
    return(matInverse)
  }
  data <- x$get()
  matInverse <- solve(data, ...)
  x$setsolve(matInverse)
  matInverse
}