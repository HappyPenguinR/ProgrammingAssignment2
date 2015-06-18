#The set of functions below allow you to cache potentially time-consuming computations

#The first function below creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    #1 set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #2 get the matrix
  setinverse <- function(inverse) inv <<- inverse
  #3 set the inverse
  getinverse <- function() inv
  #4 get the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    #check to see if the inverse has been calculated
    message ("getting cached data")
    return(inv)
    #retrieves the inverse from the cache
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv
}