# Given matrix X solve for and cache the inverse of X
makeCacheMatrix <- function(X = matrix()) {
        inv <- NULL
        set <- function(y) {
            X <<- y
            inv <<- NULL
        }
        get <- function() X
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# Retrieve solution for inverted matrix from cache
cacheSolve <- function(X, ...) {
        inv <- X$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- X$get()
        inv <- solve(data, ...)
        X$setInverse(inv)
        inv
}