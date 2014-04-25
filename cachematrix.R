#Calculate the inverse of x and store x and the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
  #If you do w <- matrix(8:11,ncol=2) and then ww <- makeCacheMatrix(w), you will see that ww is a list containing the four functions above.
}


#Return the inverse of x
#For more information about the inverse, see http://en.wikipedia.org/wiki/Inverse_of_a_matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
