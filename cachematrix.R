## The two R functions below provide for the calculation 
## of the inverse of an invertable matrix, caching the 
## solution of inverting the matrix, but avoiding the 
## calculation if there is already a cached solution.

## Function "makeCacheMatrix()" creates a special invertable 
## "matrix" which is really a list object with functions to:
##      1/. set the values of the matrix
##      2/. get the value of the matrix
##      3/. set the value of the matrix inverse
##      4/. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() {
          side = sqrt(length(x))
          x <- matrix(x, nrow = side, ncol = side, 
               byrow = FALSE, dimnames = NULL)
          } ## It must be a square non-singular matrix
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse,
          getinverse = getinverse)
}

## Function "cacheSolve()" returns as a solution, the 
## inverse matrix of an input matrix. Firstly by seeking 
## for the solution in the above function's cached memory.
## Otherwise calculating and storing the solution anew.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     ## m <- x["getinverse()"]
     if(!is.null(m)) {
          message("Getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
##
## vec <- c(9, 13, 5, 2, 1, 11, 7, 6, 3, 7, 4, 1, 6, 0, 7, 10) ## 16 long
## test <- makeCacheMatrix(vec) ## To be a square matrix, 4 by 4
## tested <- cacheSolve(test) ## Do it twice to show as from cache
##


