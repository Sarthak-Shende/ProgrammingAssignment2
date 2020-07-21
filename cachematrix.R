##      makeCacheMatrix function sets and returns the matrix and it's inverse and cacheSolve function computes
## the inverse of the 


##      makeCacheMatrix function sets the value of x to the matrix and i to null/ It creates a list of the set,
## get, setinverse, and getinverse function to call them using the $ operator. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##      cacheSolve function computes the inverse of the matrix if i is null or returns inverse if i is ot NULL

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

