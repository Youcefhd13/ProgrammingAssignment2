
## This codes provides the inverse of a matrix while checking for a cached result to save processing time

## makeCacheMatrix provides the funcions to create, check and get the inverse of a matrix x

makeCacheMatrix <- function(x = matrix()) {
  
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inversem) m <<- inversem
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve to execute the inverse function while checking for a previously calculated result from makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  
}
