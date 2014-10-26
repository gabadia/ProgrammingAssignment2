#########################################################################################
##      NOTE: this code was created directly from the ProgrammingAssignment2 repository 
##      The new functions were created by simply doing the following:
##
##      1) copying and pasting the code
##      2) replacing the word "x" with "inv"
##      3) replacing the word "mean" witn "inverse"
##      4) replacing the word "getmean" with "getinverse"
##      5) replacing the word "setmean" with "setinverse"
##      6) replacing the function "mean()" with "solve()"
#########################################################################################

## From what I can tell, this assignment allows us to 
## - practice creating customized functions
## - store functions in a list with named elements
## - reference functions in a list with named elements
## - use assignment operator "<<-" which cause a search to be made through parent environments 
##   for an existing definition of the variable being assigned
##
## NOTE: Function is very limitied as it can only store one matrix and its inverse


## makematrix creates a list with named element members containing functions to

##    set the value of the matrix   - set()
##    get the value of the matrix   - get()
##    set the value of the inverse  - setinverse()
##    get the value of the inverse  - getinverse() 

## Returns the list specified above

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" created by makematrix.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the value of the inverse in the 
## cache via the setinverse function.

## Returns the matrix that is the inverse of 'x' that
## was specified by the last call to makeCacheMatrix
##
## It is assumed that matrix x is always invertible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  
}
