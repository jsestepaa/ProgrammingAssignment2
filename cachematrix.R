## This code defines a new matrix class who has the capacity to store it's own inverse
## in a cache, in case there's the need for it.

makeCacheMatrix <- function(baseMatrix = matrix()) {
    baseInverse <- NULL
    set <- function(newMatrix){
        baseMatrix <<- newMatrix
        baseInverse <<- NULL
    }
    get <- function(){
        baseMatrix
    }
    setInverse <- function(newInverse){
        baseInverse <<- newInverse
    }
    getInverse <- function(){
        baseInverse
    }
    list(set = set, get = get,
        setInverse = setInverse, 
        getInverse = getInverse
        )
}

## This function search for the inverse of the custom matrix. If it exist in the cache,
## it calls it, but if the cached inverse is null, thefunction calculates it and saves
## it in the cache.

cacheSolve <- function(baseMatrix, ...) {
    baseInverse <- baseMatrix$getInverse()
    if(!is.null(baseInverse)) {
        message("Getting cached data")
        return(baseInverse)
    }
    temp <- baseMatrix$get()
    baseInverse <- solve(temp)
    baseMatrix$setInverse(baseInverse)
    return(baseInverse)
}
