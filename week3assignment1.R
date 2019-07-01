#makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),then cacheSolve should retrieve the inverse from the cache

#These functions are token from Coursera : R Programming by JHU W3
#Week 3 Assignment; 01/07/2019; GitHub: nursenc




makeCacheMAtrix <- function(x=matrix()) {
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



cacheSolve <- function(x,...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
        
        
}