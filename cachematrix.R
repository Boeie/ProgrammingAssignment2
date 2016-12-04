## This function is to cache a inverse matrix of the matrix made by makeCacheMatrix.  Everytime When a matrix is newly 
## made or set (makeCacheMatrix() instantiated), cacheSolve() computes and gives a inverse matrix. Otherwise, it caches a inverse matrix and shows a
## massage "getting cached data".
## makeCacheMatrix makes a list of functions. Objects 'x' and'i' are assigned values in the parent evironment. So that cacheSolve
## can call and operate them.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(input) i <<- input
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve returns a inverse of 'x'. Using 'x' in parent environment to exam if the inverse matrix has been 
## computed (x$getinveser!=NULL) and 'i' to cache inverse in parent eviroment.

cacheSolve <- function(x, ...) {
 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
## Return a matrix that is the inverse of 'x'
}



