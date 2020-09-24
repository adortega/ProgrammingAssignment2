## The aim is to define a pair of functions that are able to cache the inverse
## of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Set the matrix
  set <- function( y ) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function(){x} 
  
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) 
    inv <<- inverse
  
  
  ## Get the inverse of the matrix
  getInverse <- function() {inv}
    
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inv <- solve(data)
  
  ## Set the inverse of the object
  x$setInverse(inv)
  
  ## Return the matrix
  inv
}
