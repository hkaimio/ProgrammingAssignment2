## Caching matrix inverse (R programming course assignment 2)
## Harri Kaimio

## These functions cache the calculated value of matrix inverse to avoid recalculation later
##
## Usage:
## cm <- makeCacheMatrix(m)
##    Creates a wrapper fro amtrix m thatn can cache its value
## inv <- cacheSolve( cm )
##    Returns the inverse of m
## cm$get()
##    Returns m (the original matrix)
## cm$set(m2)
##    Resets the wrapper and replaces previous matrix with m2



## Creates a wrapper for matrix  that supports caching its inverse
## Parameters:
## x - the matrix to be wrapped

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the member variable(s)
  inverse <- NULL
  
  # Define accessor functions for closure content
  set <- function(newVal) {
    x <<- newVal
    inverse <<- NULL
  }
  
  get <- function() {
    x
  } 
  
  setInverse <- function( newInverse ) {
    inverse <<- newInverse
  }
  
  getInverse <- function() {
    inverse
  }
  
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## Returns the inverse of x, recalculating it if it is not cached
## Parameters:
## x - wrapper for a matrix, created by calling makeCacheMatrix
## Other parameters are passed to solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getInverse()
  if ( is.null( inverse) ) {
    # There is no cached value of inverse so compute it
    inverse <- solve( x$get(), ...)
    x$setInverse(inverse)
  }
  
  inverse
  
}


# Some unti tests to verify functionality

testCachematrix <- function() {
  ## Create a test matrix
  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
  h8 <- hilbert(8);
  
  ch8 <- makeCacheMatrix( h8 )
  
  tmp = ch8$get()
  if ( all(tmp == h8) ){
    print( "PASS: created matrix")
  } else  {
    print( "FAIL: get returns wrong value")
  }
  
  if ( is.null( ch8$getInverse())) {
    print( "PASS: inverse null after construction")
  } else {
    print( "FAIL: iverse not set to null after construction")
  }
    
  sh8 <- solve( h8 )
  
  tmp <- cacheSolve( ch8 )
  if ( all(tmp == sh8) ){
    print( "PASS: solved correctly")
  } else  {
    print( "FAIL: cacheSolve returns wrong value")
  }
  
  ## Check that inverse is not recalculated unnecessarily
  ch8$setInverse(matrix())
  tmp <- cacheSolve( ch8 )
  if ( nrow(tmp) == 1 ){
    print( "PASS: no recalculation")
  } else  {
    print( "FAIL: solveCacheMatrix modivies inverse even if it is set")
  }
  
  ## Check that setting new value to the cached matrix resets inverse
  ch8$set(h8)
  if ( is.null(ch8$getInverse())) {
    print( "PASS: set resets inverse")
  } else  {
    print( "FAIL: inverset still set after changing matrix with set()")
  }
  
  tmp <- cacheSolve( ch8 )
  if ( all(tmp == sh8) ){
    print( "PASS: solved correctly after resetting inverse")
  } else  {
    print( "FAIL: solveCacheMatrix returns wrong value after reset")
  }
}
