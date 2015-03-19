## Caching the Inverse of a Matrix 

# This function creates a special "matrix" object that can cache its inverse.

#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  invert the matrix
#4.  get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m<-x$getinv()
  #print(m)
  #print(length(m))
  if(!is.na(m[1,1])) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data, ...)
  x$setinv(m)
  
}
