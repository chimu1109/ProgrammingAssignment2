## The following functions creates a special objects which take a matrix and 
##cache it's inverse

## The makeCacheMatrix function builds a set of functions and returns the 
## functions whithin a list to the parent environment

## It creates a list wich has the containing a function to do each of the following :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the vakue of the inverse


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## The function cacheSolve finds out the innverse of the "Matrix", 
## however, it first checks if the inverse has already been computed 
## if so, it skips the computation and returns the stored value

cacheSolve <- function(x, ...) {
        
        
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
                
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        ## Return a matrix that is the inverse of 'x'
        s
}
