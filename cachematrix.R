## these functions provide a mechanism for caching the 
## inverse of matrixes.  These are useful for when performing
## this expensive calculation from within loops, etc.

## makeCacheMatrix will create a special "matrix" object
## that can cache its inverse.  This is used by the 
## cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  ## inverse initially null
  i <- NULL
  
  ## set function will replace our inner matrix (x), and
  ## reset any calculated inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get returns our inner matrix
  get <- function() x
  
  ## setinverse will cache a calculated inv value into i variable
  setinverse <- function(inv) i <<- inv
  
  ## get inverse returns the calculated i variable
  getinverse <- function() {
    i
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function uses the makeCacheMatrix's caching
## mechanism to ensure the inverse of each matrix is
## only calculated once.  
cacheSolve <- function(x, ...) {
  
  ## check for cached inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## not cached, we need to calculate inverse, and set it in cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}