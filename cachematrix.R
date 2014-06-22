## These funtions can be used to minimize having to caclulate over and
## over the inverse of a square Matrix (n x n). 
##
## Usage:
##      d is an R square matrix(n x n) object
##      test <- makeCacheMatrix(d)
##      cacheSolve(test)
##

## "makeCacheMatrix" can cache time consuming operation to determin
## the inverse of a square matrix(n x n). 
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

## "cacheSolve" calculates the inverse of a square matrix (n x n) 
## created by "makeCacheMatrix". Before the inverse matrix is
## calculated using R function "solve()" it verifies whther the
## inverse matrix has been caculated before or not. if the inverse 
## matrix has been calculated then it get's it from cache, otherwise
## it calculates the inverse matrix and set xInv in cahe
cacheSolve <- function(x, ...) {
  ## As input it requires the lest returned by "makeCacheMatrix"
  xInv <- x$getinverse()
  if(!is.null(xInv)) {
    message("getting cached inverse matrix")
    return(xInv)
  }
  data <- x$getmatrix()
  xInv <- solve(data, ...)
  x$setinverse(xInv)
  xInv  
  ## Return a matrix that is the inverse of 'x'
}

## Test examples

## Example with a square matrix (2x2)
## d <- rbind(c(2, 4), c(1, 3)
## test <- makeCacheMatrix(d)
## cacheSolve(test)
## cacheSolve(test)
           
## Example with a square matrix (4x4)
## d<- rbind(c(2, 4, 3, 4), c(1, 3, 9, 3), c(8, 5, 2, 9), c(2, 3, 6, 8))
## test <- makeCacheMatrix(d)
## cacheSolve(test)
## cacheSolve(test)



           
