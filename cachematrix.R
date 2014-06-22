##---cachematrix.R contains two functions: makeCacheMatrix and cacheSolve.
##   These functions demonstrate R programming lexical scoping rules by
##   caching the computationally complex operation of matrix inversion.

##---The function "makeCacheMatrix" creates a matrix object that contains
##   the functions: set, get, setinverse and getinverse. The setinverse
##   calculates the inverse of the matrix passed in. The resulting inverse
##   matrix is stored in the parent environment as "invMatrix".

makeCacheMatrix <- function(x = matrix()) {
     
     invMatrix <- NULL
     set <- function(y) { 
          x <<- y 
          invMatrix <<- NULL 
     }
     get <- function() x
     setinverse <- function(solve) invMatrix <<- solve
     getinverse <- function() invMatrix
     
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
     
}

##---The function "cacheSolve" takes advantage of setinverse's saving of the inverse
##   matrix to the parent environment to determine whether the inverse matrix has 
##   already been calculated. If so, then it simply returns the existing inverse 
##   matrix already computed. A message "getting cached data" is returned to the user if
##   the function is using cached data. If the inverse matrix has not been computed, as evident
##   by a NULL matrix, then the setinverse method is called. After setinverse method
##   is called, the inverse matrix will be stored in the parent envronment.
cacheSolve <- function(x, ...) {
     
     invMatrix <- x$getinverse()
     if(!is.null(invMatrix)) { 
          message("getting cached data")
          return(invMatrix)
     } else {
          data <- x$get()
          invMatrix <- solve(data, ...)
          x$setinverse(invMatrix)
          return(invMatrix) } 
     
}