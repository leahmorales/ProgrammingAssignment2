## These functions create a special matrix object and
## calculate and cache its inverse.

## The following function formats a matrix that can cache its 
## inverse.
makeCacheMatrix <- function(x = matrix() ){
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## The following function returns the inverse of the special
## matrix object. It first checks to see if the inverse for 
## the given matrix has already been calculated, in which case
## it returns that value from the cache.
                
cacheSolve <- function(x, ...){
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m,...)
        x$setinverse(i)
        i
}
