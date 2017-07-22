## Functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        cacheinv <- NULL
        set <- function(y) {
                x <<- y
                cacheinv <<- NULL
        }
        get <- function() x
        setcacheinv <- function(inverse) cacheinv <<- inverse
        getcacheinv <- function() cacheinv
        list(set = set, get = get,
             setcacheinv = setcacheinv,
             getcacheinv = getcacheinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheinverse should retrieve the inverse
## from the cache

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        invfromcache <- x$getcacheinv()
        if(!is.null(invfromcache)) {
                message("getting cached data")
                return(invfromcache)
        }
        data <- x$get()
        invfromcache <- solve(data, ...)
        x$setcacheinv(invfromcache)
        invfromcache
}
