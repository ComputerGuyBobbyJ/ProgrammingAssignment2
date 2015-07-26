## makeCacheMatrix - create cache matrix inverse object
## cacheSolve - return matrix inverse
##            - if cached inverse not NULL, return cached result
##            - otherwise calcuate matrix inverse using solve() and return it

## create object to cache inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
    ## Create matrix object to cache matrix inverse
    x.inverse <- NULL
    set <- function(y) {
        x <<- y
        x.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) x.inverse <<- solve
    getinverse <- function() x.inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## return inverse of matrix x
## first see if inverse is cached and return cached inverse,
## otherwise calculate matrix inverse using solve() and return result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x.inverse <- x$getinverse()
    if(!is.null(x.inverse)) {
        message("getting cached data")
        return(x.inverse)
    }
    data      <- x$get()    
    x.inverse <- solve(data, ...)
    x$setinverse(x.inverse)
    x.inverse
}