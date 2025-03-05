## These functions create a special matrix object that can cache its inverse.  
## This helps avoid redundant computations by storing the inverse once calculated.

## makeCacheMatrix creates a special "matrix" object that stores  
## a matrix and its inverse. It provides methods to set and get  
## the matrix, as well as to set and get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special matrix returned by  
## makeCacheMatrix. If the inverse has already been calculated and cached,  
## it retrieves the cached result instead of recomputing it, improving efficiency.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
