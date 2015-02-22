
## The functions create a cache for a defined matrix and its inverse value.
## The inverse value is calculated and cached the first time that it is requested, 
## after that the next requests are answered from the cached value.


## The function makeCacheMatrix, saves a matrix and its inverse.
## Also this function returns a list with the get, set, getinverse and setinverse functions to:
## set/get de matrix, and save/get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ##initial value for the inverse, it has not been calculated yet
        set <- function(y) {    ##Sets the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m   
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve, calculates the inverse of the matrix defined on the "vector" received as parameter.
## In order to return the inverse, first checks if it has already been cached, calling the getinverse function,
## if it has not been cached, calculates the inverse by calling the solve function, and then caches the value
## for further requests.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {       ## It is cached
                message("getting cached data")
                return(m)       ##Returns cached value
        }
        data <- x$get()
        m<-solve(data)          ##Calculates matrix inverse
        x$setinverse(m)
        m
}


