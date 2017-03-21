makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub-functions/methods
  
  ## define the cache temp
  temp <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the
    ## parent environment
    temp <<- NULL ## re-initialize temp cache in the parent environment to null
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) temp <<- inverse ## set the cache temp equal to the inverse of the matrix x
  getinverse <- function() temp ## return the cached inverse of x
  list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  temp <- x$getinverse()
  if(!is.null(temp)) {
    message("getting cached data")
    return(temp)
  }
  data <- x$get()
  temp <- solve(data, ...)
  x$setinverse(temp)
  temp

}