## makeCacheMatrix
## Creates an object that stores a matrix (sm) and it's inverse (invsm) 
## and provides get and set methods for these attributes.
## The function returns a list of four functions:
## get()     -    returns sm 
## set(m)    -    sets sm
## getinv    -    returns invsm (inverse matrix if cached else NULL)
## setinv(m) -    sets invsm

makeCacheMatrix <- function(x = matrix()) {
    invsm <- NULL
    sm <- x  # explicit is better than implcit!
    
    set <- function(y) {
        sm <<- y
        invsm <<- NULL  
    }
    
    get <- function() sm
    
    setinv <- function(inverseMatrix) invsm <<- inverseMatrix
    
    getinv <- function() invsm
    
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}  


## cacheSolve
## Let M be an invertible matrix. Do x <- makeCacheMatrix(M). Call
## cacheSolve(x) to compute and return the inverse M' of M. Subsequent
## calls cacheSolve(x) will return M' without re-computing it
## x$set(M) will save a new matrix. 

cacheSolve <- function(x, ...) {
    invsm  <- x$getinv()
    if ( !is.null(invsm) ) {
        message("Inverse cached")
        return(invsm) # inverse is cached
    }
    
    invsm <- solve(x$get())
    x$setinv(invsm)
    return(invsm)
}

m <- matrix(rnorm(25), nrow=5)
m1 <- solve(m)
x <- makeCacheMatrix(m)
cacheSolve(x)
m2 <- cacheSolve(x)
m1 == m2
