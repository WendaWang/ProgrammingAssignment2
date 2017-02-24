## (1)get is a function that returns the vector x stored in the main function.
## (2)set is a function that changes the vector stored in the main function.
## (3)setinverse and getinverse are functions very similar to set and get.
## (4)They don't calculate the inverse, they simply store the value of the input in a variable m.
## (5)into the main function makeVector (setinverse) and return it (getinverse).

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}