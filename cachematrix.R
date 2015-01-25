## Pair of functions to allow for computation of matrix inversion with caching of result

## Create a matrix with getters & setters for solve (matrix inverse)
## Allows inversion to be stored
makeCacheMatrix <- function(x = matrix()) {
  matInverse <- NULL
  set <- function(y) {
    x <<- y
    ## if setter is applied clear inverse (new matrix present)
    matInverse <<- NULL
  }
  ## Getter/Setters for data
  get <- function() x
  setinverse <- function(inverse) matInverse <<- inverse
  getinverse <- function() matInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solve (invert) a matrix, with caching capability
## check to see if the inverse is already stored via the getsolve function
## If is is return that inverse.   If not compute inverse and set stored inverse
cacheSolve <- function(x, ...) {
  matInverse <- x$getinverse()
  ## Check if chached inverse is present
  if(!is.null(matInverse)) {
    ## if cached inverse is present return, no need to compute again
    message("getting cached inverse")
    return(matInverse)
  }
  data <- x$get()
  ## calculate inverse
  matInverse <- solve(data, ...)
  ## set matrix solution (inverse) in x using setter
  x$setinverse(matInverse)
  matInverse
}