## Matrix class which caches the inverse matrix calculation

## Class constructor with class variables:
## x - matrix
## i - cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## Use cached value if available
cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("returning cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

