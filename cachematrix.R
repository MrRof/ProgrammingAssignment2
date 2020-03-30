## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## The makeCacheMatrix 
##This is a function that will produce a matrix that will cache its own
##inverse as the input result

makeCacheMatrix <- function(x = matrix()) {
  con <- NULL
  set <- function(y){
    x <<- y
    con <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) con <<- inverse
  getInverse <- function() con
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
##cacheSolve
##This function will work together with the makeCacheMatrix function; cacheSolve will
##compute the matrix obtained by makeCacheMatrix. In case that the inverse has been
##calculated, the result will retrieve the inverse of the chache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  con <- x$getInverse()
  if(!is.null(con)){
    message("getting cached data")
    return(con)
  }
  mat <- x$get()
  con <- solve(mat,...)
  x$setInverse(con)
  con
}
