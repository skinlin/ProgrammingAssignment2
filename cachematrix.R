## These two functions work to store and retrieve a matrix and its inverse.
## The primary purpose is to save computation cost by not calculating the
## inverse if it has previously been calculated.

## The makeCacheMatrix function creates a structure as a list of functions
## that hold a matrix and optionally its inverse.  The functions provide 
## access to retrieve and to store the matrix and its inverse value.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function accesses the matrix in the structure produced by
## makeCacheMatrix.  If the inverse of the matrix has not been calculated,
## cacheSolve will calculate and store it.  If the inverse has been previously
## calculated, cacheSolve will retrieve the inverse value and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
