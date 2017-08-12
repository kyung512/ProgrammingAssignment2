## R-Programming-Assignment=2
## makeCacheMatrix creates an object with matrix and its cached inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)  {
          x <<- y
          inv <<- NULL
    }
    get <- function() x
    setinverse <-  function(inverse) inv <<- inverse
    getinverse <-  function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve subrountine computes the inverse of a matrix defined in makeCacheMatrix function
## under the condition that it has not been calculated before. It will retrieve its 
## cached value if it has has been calculated already. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          inv <- x$getinverse()
          if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
          }
          data <- x$get()
          inv  <- solve(data, ...)  #inverse of a matrix is performed by "solve"
          x$setinverse(inv)
          inv
}







