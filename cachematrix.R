## makeCacheMatrix is a matrix which can cache its inverse, 
## which can be calculated by cacheSolve (for non-singular square numeric matrices).
## 

## Takes a square numeric inversible matrix
## and creates a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    mx <- x
    get <- function() mx
    set <- function(y){
        mx <<- y
        inv <<- NULL
    }
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list (get = get,
          set = set,
          setInv = setInv, 
          getInv = getInv)
}


## Takes a "matrix" produced by makeCacheMatrix
## and returns its inverse.
## If the inverse has already been calculated, gets it from cache;
## if not, calculates it and caches in the makeCacheMatrix object.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        inv
    }
    else {
        inv <-solve(x$get())
        x$setInv(inv)
        inv
    }
}
