########################################################
## The purpose of this script is to accept a matrix
## (square or otherwise) and cache that matrix as well
## as its inverse.  Computing the matrix inverse can be 
## a computationally demanding endeavour, so caching it
## does away with the need to compute the inverse on 
## every call for the same matrix.
########################################################

## The makeCacheMatrix function accepts a matrix and caches it. It also
## also provides accessor functions for getting/setting the matrix and
## the matrix inverse.
makeCacheMatrix <- function(curMatrix = matrix()) {
  
  ## Initialize the matrix inverse to null
  inverse <- NULL
  
  ## Sets the matrix to the new value and resets the inverse to null
  set <- function(newMatrix) {
    curMatrix <<- newMatrix
    inverse <<- NULL
  }
  
  ## Return matrix
  get <- function() curMatrix
  
  ## Set (cache) inverse of matrix
  setInverse <- function(computedInverse) inverse <<- computedInverse
  
  ## Return inverse of matrix
  getInverse <- function() inverse
  
  ## Default return (list of all embedded function states/environment hash)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The cacheSolve function accepts an instance of the makeCacheMatrix function 
## along with any associated arguments. If the matrix is already in cache, it
## will return that matrix.  Otherwise, it will handle the matrix inverse 
## computation and call the setInverse function on the instance of the 
## makeCacheMatrix function that is passed in as a parameter.
cacheSolve <- function(curMatrix, ...) {
  
  ## Get the current inverse of the matrix
  inverse <- curMatrix$getInverse()
  
  ## Return the cached matrix if the inverse has already been computed
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Get the matrix itself (implies no inverse cached)
  data <- curMatrix$get()
  
  ## Compute the inverse of the matrix
  inverse <- solve(data, ...)
  
  ## Set the computed inverse in the cache
  curMatrix$setInverse(inverse)
  
  ## Return the inverse
  inverse
}