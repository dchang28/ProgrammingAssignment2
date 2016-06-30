## A pair of functions to set, calculate the inverse of a square matrix, and
## store the inverse in a cache.

## The first function contains several functions which set a square matrix 
## in a vector, retrieve the matrix, calculate the inverse of 
## the matrix and store it in a cache, and retrieve the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversion <- NULL
        set <- function(y){
                x <<- y
                inversion <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inversion <<- solve
        getinv <- function() inversion
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This second function will check to see if the inverse has already been
## calculated by the first function. If the inverse has been calculated, 
## this function will retrive it from the cache and skips computation. 
## If no inverse has been calculated, the function will calculate the inverse
## of the square matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversion <- x$getinv()
        if(!is.null(inversion)) {
                message("getting cached data")
                return(inversion)
        }
        data <- x$get()
        inversion <- solve(data, ...)
        x$setin(inversion)
        inversion
}