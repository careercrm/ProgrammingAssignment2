## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    
    ## Initialize the inverse property
    i <- NULL
    
    ## Method to set the matrix
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    ## Method the get the matrix
    get <- function() {
      ## Return the matrix
      x
    }
    
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
      i <<- inverse
    }
    
    ## Method to get the inverse of the matrix
    getInverse <- function() {
      ## Return the inverse property
      i
    }
    
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  


# Compute the inverse of the special matrix returned by "makeCacheMatrix" above.
##If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  i <- solve(data,...)
  
  ## Set the inverse to the object
  x$setInverse(i)
  
  ## Return the matrix
  i
}