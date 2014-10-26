#########################################################################################
##      NOTE: this code was created directly from the ProgrammingAssignment2 repository 
##      The new functions were created by simply doing the following:
##
##      1) copying and pasting the code
##      2) replacing the word "x" with "inv"
##      3) replacing the word "mean" witn "inverse"
##      4) replacing the word "getmean" with "getinverse"
##      5) replacing the word "setmean" with "setinverse"
#########################################################################################

## From what I can tell, this assignment allows us to 
## - practice creating customized functions
## - store functions in a list with named elements
## - reference functions in a list with named elements
## - store objects in the environment
## - retrieve objects from the environment
##
## NOTE: Function is very limitied as it can only store one matrix and its inverse.  However all we
## would need to do is create a list of list and loop through it order to create a function
## that could store several matrices and its inverse 


## makematrix creates a special "matrix", which is really a list containing a function to

##    set the value of the matrix
##    get the value of the matrix
##    set the value of the inverse
##    get the value of the inverse

## Returns the special matrix described above

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
