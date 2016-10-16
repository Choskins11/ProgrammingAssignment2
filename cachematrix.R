## The fuctions take a square matix and returns the inverse
## matrix stored in the parent environment. 
## 
## makeCacheMatrix initializes elements x and m and sets them
## in memory in the parent environment. It then returns a list
## of functions to the parent environment to be used downstream
makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
                      x <<- y
                      m <<- NULL
               }
  
              get <- function() x
              setinverse <- function(solve) m <<- solve
              getinverse <- function() m
              list(set = set, get = get,
                    setinverse = setinverse,
                    getinverse = getinverse)
}

## cacheSolve works using solve()that returns the inverse of
## a matrix. It checks to see if a result for m exists and if
## so, returns it. Otherwise it sets the return of the solve()
## This function only works with the list created by
## makeCacheMatrix()
cacheSolve <- function(x, ...) {
              m <- x$getinverse()
              if(!is.null(m)) {
                      message("getting cached data")
                      return(m)
              }
              data <- x$get()
              m <- solve(data, ...)
              x$setinverse(m)
}
