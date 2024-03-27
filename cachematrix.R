## To cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y){
    x <<- y
    invm <<- NULL
  }
  get <- function()x
  setinversematrix <- function(inverse) invm <<- inverse
  getinversematrix <- function()invm
  list(set=set,get=get,setinversematrix=setinversematrix,getinversematrix=getinversematrix)
}


## compute the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinversematrix()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinversematrix(invm)
  invm
}