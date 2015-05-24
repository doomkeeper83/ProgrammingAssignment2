## These two functions cache and invert a matrix for efficient resource usage
## Assumptions: the given matrix will always be invertible

## Usage
## cacheSolve(x)


## Caches a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list( set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## If the inverse has already been calculated, retrieve it from the cache
## created by the function "makeCacheMatrix". Otherwise, cache the matrix and
## calculate the inverse
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
