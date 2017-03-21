## Programming Assignment 2 : Lexical Scoping 
## By caching the result of matrix inversion 
## in the makeCacheMatrix,I try to avoid doing
## meaning-less recalculations of time-consuming process 


## prepare 4 functions set/get to manage the target matrix 
## and setInv/getInv for inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) mInv <<- inv
  getInv <- function() mInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## calculate inversed matrix unless it was calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  targetm <- x$get()
  m <- solve(targetm, ...)
  x$setInv(m)
  m
  
}
