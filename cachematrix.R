##Funtion makeCacheMatrix below is storing list of four functions
##If necessary you can bring the particular fuction using $
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ##this function changes the matrix stored in the main function
        
        get <- function() x 
        ##this function returns argument of makeCacheMatrix function
        
        
        
        setinverse <- function(solve) inv <<- solve
        ##this function stores the value of the input (solve)
        
        getinverse <- function() inv
        ##returns argument inv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        ##to have these four functions in one list

}



cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if inv is diffrent than zero it means that inversion of matrix
        ## was calculated and should be returned from cache
        
        else{
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        }
        ## if inv was equal to zero (it was set in makeCacheMatrix$set function)
        ## we have to calculate inverse of matrix and store it in inv
}
