## These two functions create an object to hold a numerical matrix
## and its inverse in a cached space.  This is useful if the 
## inverse will be used repeatedly so that it can be easily 
## retrieved from memory rather than calculated each time.

## This function creates a cached object containing the matrix data 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function determines if the matrix inverse has alredy been 
## calculated, and if so, it returns it from cached memory.  Else, 
## it calculates the inverse and stores in cached memory.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mtx <- x$get()
  ## Return a matrix that is the inverse of 'mtx'
  i <- solve(mtx, ...)
  x$setinv(i)
  i
}
