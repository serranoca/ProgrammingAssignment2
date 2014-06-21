## These funtions can be used to minimize having to caclulate over and
## over the inverse of a square Matrix (n x n). "makeCacheMatrix is able
## to cache the time consuming operation of determining the inverse
## of a square matrix (n x n).  
## Usage:
##      d = square matrix(n x n)
##      test <- makeCacheMatrix(d)
##      cacheSolve(test)

## "makeCacheMatrix is able to cache time consuming operation to determin
## the inverse of a suqare matrix(n x n). 
makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  setmatrix <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  getmatrix <-  function() x
  setinverse <- function(inverse) xInv<<- inverse
  getinverse <- function() xInv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  ## The function returns a list to to:
  ##   - set in cache the original matrix (setmatrix)
  ##   - getthe original matrix (getmatrix)
  ##   - set the inverse of the original matrix (setinverse)
  ##   - get the inverse of the original matrix (get inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getinverse()
  if(!is.null(xInv)) {
    message("getting cached inverse matrix")
    return(xInv)
  }
  data <- x$getmatrix()
  xInv <- solve(data, ...)
  x$setinverse(xInv)
  xInv  
}

## Test examples
d <- rbind(c(2, 4, 3, 4), c(1, 3, 9, 3), c(8, 5, 2, 9), c(2, 3, 6, 8))
d <- rbind(c(2, 4), c(1, 3)
           
