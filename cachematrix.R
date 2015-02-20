## makeCacheMatrix is a function containing functions which store a matrix, 
## return this stored matrix, store its inverse, and return its stored inverse.
## When cacheSolve is called over an instance of makeCacheMatrix, the solve() function
## is computed over the stored matrix and the inverse matrix is then stored within
## the instance of makeCacheMatrix. If cacheSolve is called when an inverse matrix
## is already stored in an instance of makeCacheMatrix, the stored inverse matrix
## is instead returned and no solve() function is called.



## makeCacheMatrix allows for storing and returning a given matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## cacheSolve runs solve() over a stored matrix in makeCacheMatrix and stores
## the result in makeCacheMatrix while also returning the result of store()
## unless an inverse matrix is already stored in makeCacheMatrix, in which case
## the stored matrix is returned

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
