## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create a 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(solve) m <<- solve
    getInverseMatrix <- function() m
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function
## This function computes the inverse of the 'matrix' returned by 
## makeCacheMatrix above
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getInverseMatrix()
    ## If the inverse has already been calculated, then the cachesolve
    ## should retrieve the inverse from the cache
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data, ...)
    x$setInverseMatrix(m)
    m
}
