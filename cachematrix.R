## This function will create a matrix and calculate the inverse of it.
## when calculating the inverse it will also cache the value so if called
## again it doesn't have to recalculate it, it can just return the cached
## version

## This will create the matrix.  Can be called by:
## z <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(solve) m <<- solve
    getinvmatrix <- function() m
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## This will return the inverse of the matrix, if cached it won't recalculate
## Can be called by:
## cacheSolve(z)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinvmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmatrix(m)
    m
}