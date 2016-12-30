## Matrix inverse is a costly computation and therefore, it seems reasonable 
# to calculate the inverse of a matrix once and retrieve it from a cache for use at multiple places in a program.
# The two functions - makeCacheMatrix and cacheSolve help us to achieve this objective using LEXICAL SCOPING in R. The 
#makeCacheMatrix function stores the inverse and the cacheSolve function retrieves that cached value. If a new 
# matrix is used, cacheSolve calculate the inverse and caches it for future use


## makeCacheMatrix creates a matrix object that can cache its inverse. It returns a list containing functions to set a new matrix, get the existing matrix, cache the inverse and retrieve that inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve retrieves the cached inverse of an existing matrix using getinverse function of MakeCachematrix. If the cached inverse does not exist it calculates the inverse and caches it using setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

  