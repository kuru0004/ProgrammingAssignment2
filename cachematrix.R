## makeCacheMatrix and cacheSolve work together to determine the
## inverse of a matrix:
## 1. If the inverse has been solved earlier, cacheSolve obtains the
##    value from list of functions made in makeCacheMatrix.
## 2. If the inverse has not been previously solved, cacheSolve
##    computes the value and stores it in makeCacheMatrix variable



## makeCacheMatrix takes matrix (need to be square to get inverse)
## and returns a list of functions to set or get the matrix or inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    setMat <- function(y) {
      x <<- y
      Inv <<- NULL
    }
    getMat <- function() x
    setInv <- function(Inverse) Inv <<- Inverse
    getInv <- function() Inv
    list(setMat = setMat, getMat = getMat,
         setInv = setInv,
         getInv = getInv)
}



## Return a matrix that is the inverse of 'x',
## 'x' must be variable of type getCacheMatrix because calls are made
##  to the function list elements from getCacheMatrix

cacheSolve <- function(x, ...) {
        
    Inv <- x$getInv()
    if(!is.null(Inv)) {
      message("getting cached data")
      return(Inv)
    }
    data <- x$getMat()
    Inv <- solve(data, ...)
    x$setInv(Inv)
    Inv
}


