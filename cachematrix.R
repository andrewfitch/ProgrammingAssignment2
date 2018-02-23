## makeCacheMatrix creates a cache for calculating the inverse of a square matrix
## Functions:
## get() get the matrix store
## set(y) set a new matrix 
## setinverse(y) caches the matrix inverse
## getinverse() gets the cached inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    ## Test to ensure that y is a matrix
    if(class(y) == "matrix") 
    { 
      x <<- y
      m <<- NULL
    } 
    else
    {
      message("Please supply a matrix.")
    }
  }
  get <- function() {x}
  setinverse <- function(solve) {m <<- solve}
  getinverse <- function() {m}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve takes in a cacheMatrix and calculates the inverse of it and cashes the result
##or returns the result if it was already cashed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

