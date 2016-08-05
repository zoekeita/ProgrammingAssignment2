## pair of functions that caches the inverse of a matrix for easy retrieval

## creates special "matrix" object (list of functions) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set <- function(y) {
                x <<- y
                inv<<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv<<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## returns inverse of original matrix by looking up in cache or computation

cacheSolve <- function(x, ...) {
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
