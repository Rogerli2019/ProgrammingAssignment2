## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m<-matrix(NA)
    set <- function(y) {
      x <<- y
      m <<- matrix(NA)
    }
    get <- function() x
    setInverse <- function(z) m <<- z ## As explained by professor 
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
  


## Just like the sample provided by Professor Peng, this function contains four parts 
## 1.set the value of the matrix
## 2.get the value of the matrix 
## 3.set the value of the inverse of matrix
## 4.get the value of the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getInverse()
    if(!is.na(m[1,1])) {
      message("getting cached data")
      return(m)
    }
        ##Only have to assess the first element in reverse matrix, since other elements will be either
        ##NULL or valid.
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
