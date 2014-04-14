## The functions below are used to create a matrix and cache it's inverse
## functions do

## This function creates a list of functions to
##   1) set the value of the matrix
##   2) get the value of the matrix
##   3) set the value of the inverse
##   4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse <- function(inverse) i <<-inverse
  getinverse <-function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function first checks to see if the inverse of x has already been calculated.  
## If so it will skip computation and fetch inverse from cache. 
## If not it will calculate the inverse and cache the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data<-x$get()
  
  ##Computation of inverse
  i <- solve(data) %*% data
  
  ##Cache matrix
  x$setinverse(i)
  
  ##Return inverse
  x
}



