# creates a special "matrix", which is really a list containing a function to
# 1.  sets the matrix
# 2.  gets the matrix
# 3.  set the inverse of the matrix
# 4.  get the inverse of the matrix

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
         setinverse  = setinverse ,
         getinverse  = getinverse)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse matrix from the
# cache and skips the computation. Otherwise, it calculates the inverse matrix of
# the data and sets the inverse matrix in the cache via the `setinverse function.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
    x$setinverse(m)
    m
}

