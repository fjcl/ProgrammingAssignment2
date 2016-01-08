##
## Creates a special matrix which is a list containing a function to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the cache matrix
## 4. get the value of the cache matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## Cache matrix initialization
        cache <- NULL
        
        ## Set matrix function
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        
        ## Get matrix function
        get <- function() x
        
        ## Set cache matrix function
        setCache <- function(m) cache <<- m
        
        ## Get cache matrix function
        getCache <- function() cache
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}

##
## Returns the inverse of the special matrix created with makeCacheMatrix.
## Execution stops if special matrix is not square.
##
## The inverse is computed using solve() but only when this function is called for the first time.
## In next calls, the cache matrix is just returned avoiding the need for the inverse computation.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_1 <- x$getCache()
        if(!is.null(x_1)) {
                message("getting cached data")
                return(x_1)
        }

        ## Generate an error if matrix is not square
        dimensions <- dim(x$get())
        if ((length(dimensions) != 2) ||
            (dimensions[1] != dimensions[2]))
            stop ("Matrix is not square")

        ## Compute matrix inverse
        data <- x$get()
        x_1 <- solve(data, ...)
        x$setCache(x_1)
        x_1
}
