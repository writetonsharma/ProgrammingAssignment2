
# this function sets and gets matrix inverse
# maintains the inverse of the matrix in the cache
makeCacheMatrix <- function(m = matrix()) {
  
  # make a matrix
  inverse <- NULL
  setMatrix <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  
  # get the matrix already created
  getMatrix <- function() m
  
  # set the inverse of the matrix
  setInverse <- function(i) inverse <<- i
  
  # get the inverse of the matrix if already set
  getInverse <- function() inverse
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)

}

# this function get the inverse of the matrix if that is already set from cache
# if the inverse is not already set, it calculates the inverse and set it in the cache
cacheSolve <- function(x, ...) {

  # check if inverse already exist for the passed matrix
  # if exist, return it
  i <- x$getInverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  
  # we reached here which means the inverse didnt exist in the cache and we need to calculate one
  # and also need to set that in the cache
  data <- x$getMatrix()
  i <- solve(data)
  x$setInverse(i)
  
  i
}
