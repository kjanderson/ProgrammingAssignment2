## Summary of functions in this file:
##  makeCacheMatrix: create a vector to store the matrix
##  cacheSolve: compute the matrix inverse

## makeCacheMatrix: create a vector to store matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(fcnInverse) m <<- fcnInverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: calculate matrix inverse if cache misses;
##             otherwise return cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

