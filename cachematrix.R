#These two functions cache the inverse of a matrix, first by creating a special object (a matrix) that is able to cache its inverse.
##Secondly, the inverse of the matrix object created in the first funtio is computed.
#These functions are useful to caching the inverse of a matrix rather than compute it constantly, which has a high computation cost.

##The following function creates a "matrix" object that can catch its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##The following function returns the inverse of the "matrix" created with the above function
cacheSolve <- function(x, ...) {
        require(matlib)
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- Inverse(data, ...)
        x$setinverse(inv)
        inv
        }
