## Inverting matrix is a computationally intensive operation.
## Following functions make the process faster.

## makeCacheMatrix sets and gets the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set <- function (y) {
            x<<-y
            m<<-NULL
      }
      get <- function () x
      setinv<- function(inv) m<<-inv
      getinv<- function() m
      list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve checks if matrix is in the cache, if yes, uses the
## existing inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m<-x$getinv()
      if(!is.null(m)){
            message("getting cached data")
            return (m)
            
      }
      data <- x$get()
      m<-solve(data,...)
      x$setinv(m)
      m
}
