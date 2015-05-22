## Put comments here that give an overall description of what your
## functions do

##These two functions can be used to cache the inverse of a matrix and
## then use the cached result for further computations rather then
## calculating the inverse of a matrix repeatedly which is a costly
## operation. These functions helps to improve performance of the
## inverse calculation operation on matrix.

## Write a short comment describing this function

##Name : makeCacheMatrix
##Arguments: A Square Invertible matrix
##Returns: A Special Matrix object then can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

##Name : cacheSolve
##Arguments: Special Matrix Object returned by 'makeCacheMatrix' function
##Returns: Inverse of the Matrix, if the inverse is already computed and the 
## matrix is not changed then it returns the inverse from the cache otherwise 
## it calculate the inverse, set the inverse of Special Matrix and then returns
## the newly calculated value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached Invserse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
