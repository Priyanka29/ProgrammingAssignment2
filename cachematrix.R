## Put comments here that give an overall description of what your
## functions do
## The makeCacheMatrix funtion take a square matrix and create a list of functions to get and set the matrix and its inverse
## The cacheSolve funtion returns the inverse of square matrix, if the inverse is not cached it will calculate the inverse using solve() method otherwise returns the cached matrix using the getinverse() method.

## Write a short comment describing this function
## This function returns the list of functions to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get  <- function() x
    setinverse  <- function(inverse) inv <<- inverse
    getinverse  <- function() inv
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse= getinverse)

}


## Write a short comment describing this function
## This function returns the inverse of the matrix x. If it is cached it will return the cached inverse, else it will calculate the inverse for new matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
