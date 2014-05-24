## This function calculates the inverse of a square matrix
## If the inversion has done before, it will call from cache 
## instead of re-calculating

## This function set and get the input square matrix &
## set and get the inverse of the input square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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


## This function calculates the inverse of a square matrix
## it caches it in memory and recalls it if it is available in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

## the following steps can be used to test the functions
##1. matrix <- matrix(c(2, 0, 0, 0, 2, 0, 0, 0, 2), 3, 3)  # this makes a simple matrix
##2. mm = makeCacheMatrix(matrix)                 # this returns the "special matrix" as discussed in the assignment
##3. attributes(mm)                          # this verifies that vv is special (i.e. a list with some functions in it)
##4. cacheSolve(mm)                        # this will force a computation of the inverse of the input matrix  
##5. cacheSolve(mm)                        # this should show you the message "getting cached inverse"        

## in order to use the cached memory, the input needs to have unique names
## if we calculate 2 different data set using the same input name, then 
## we need to run the above steps from 1 to 5 