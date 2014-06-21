## Put comments here that give an overall description of what your
## functions do

##---The function "makeCacheMatrix" creates a matrix object that contains
##   the functions: set, get, setinverse and getinverse. The setinverse
##   calculates the inverse of the matrix passed in. The resulting inverse
##   matrix is stored in the parent environment as "m".

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
  get     <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  

}

##---The function "cacheSolve" takes advantage of setinverse's saving of the inverse
##   matrix to the parent environment to determine whether the inverse matrix has 
##   already been calculated. If so, then it simply returns the existing inverse 
##   matrix already computed. If the inverse matrix has not been computed as evident
##   by a NULL matrix then the setinverse method is called. After setinverse method
##   is called then the inverse matrix will be saved in the parent envronment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } else {
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    return(m) }  
}