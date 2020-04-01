## Put comments here that give an overall description of what your
## functions do
##These functions may improve performance by allowing one to cache 
#the inverse of a matrix.

## Write a short comment describing this function
# This function creates the framework to allow caching.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #this will be the inverse
        set <- function(y) {
                x<<-y #this changes the matrix value in a different environment
                i<<-NULL #this clears the inverse when the matrix is changed
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set=set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#This function will get the matrix, set the matrix, or get the inverse from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix,...)
        x$setinverse(i)
        i
}

# test<- matrix(1:4,4:8,nrow=2, ncol=2)
# assigned_function <- makeCacheMatrix(test)
# cacheSolve(assigned_function)
# assigned_function$set(matrix(4:7,9:12,nrow = 2,ncol = 2))
# solve(test)
