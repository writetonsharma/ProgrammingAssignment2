## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  
  inverse <- NULL
  setMatrix <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  
  getMatrix <- function() m
  
  setInverse <- function(i) inverse <<- i
  
  getInverse <- function() inverse
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  
  data <- x$getMatrix()
  i <- solve(data)
  x$setInverse(i)
  
  i
}
