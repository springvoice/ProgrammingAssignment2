## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set variable s (inverse in this case) to NULL
  s <- NULL
  #set function sets x to the argument y and set m to null
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  #get returns the value of x 
  get <- function() x

  setinverse <- function(inverse) s <<- inverse

  getinverse <- function() s

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  #attempts to get the inverse matrix from x (if it was calculated previously)
  s <- x$getinverse()
  #if not null, a valued was cached, so return s 
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  #since its null, set data to x from makeCatheMatrix
  data <- x$get()
  #calculate the inverse of data
  s <- solve(data, ...)
  #set s in x to calculated inverse
  x$setinverse(s)
  #return inverse matrix
  s
}
