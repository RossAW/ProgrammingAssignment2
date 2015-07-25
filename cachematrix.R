## These two functions will calculated the inverse of a square invertible matrix
## and then store it. If the inverse is needed again it will retrieve the inverse 
## instead of calculating it again.


## This function creates a list of functions that allow for the storing and 
## retrieving of the input matrix and the inverse matrix.

## makeCacheMatrix is the main function that takes the input matrix and creates 
## the list of functions
makeCacheMatrix <- function(x = matrix()) {
    ## This creates a NULL object that will be used to store the inverse matrix
    m <- NULL
    
    ## The set function allows us to change the input matrix and will reset 
    ## the inverse matrix at the same time. The <<- operator needs to be used 
    ## as the variables we want to modify were defined in the makeCacheMatrix
    ## environment. using <- would simpliy change x and m in the local set 
    ## enironment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## The get function allows us to display the current input matrix
    get <- function() x
    
    ## The setInverse function allows us to take the inverse matrix as an 
    ## input and store it.
    setInverse <- function(Inverse) m <<- Inverse
    
    ## The getInverse function will display the currently stored inverse matrix
    ## (or NULL object if nothing has been stored yet).
    getInverse <- function() m
    
    ## This takes the above defined functions and stores them in a list with
    ## appropiate names. This allows the desired function to be called by using
    ## the "$" operator
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function will check to see if the inverse matrix has allready been 
## calculated and then either retrieve it or calculate it

cacheSolve <- function(x, ...) {
    
    ## This will retrieve call the getInverse function from the list, which will 
    ## retrieve the inverse matrix from the makeCacheMatrix environment and store
    ## it in the cacheSolve environment.
    m <- x$getInverse()
    
    ## This checks to see if it is NULL or not
    if(!is.null(m)) {
        
        ## If it isn't NULL it will simply display the inverse matrix and let
        ## you know that it was cached. This could probably use some additional 
        ## checks as anything could be assigned to m using setInverse and it 
        ## would be displayed here
        message("getting cached data")
        return(m)
    }
    
    ## If it is NULL the input matrix is retrieved
    data <- x$get()
    
    ##The inverse is calculated and assigned to m in the cacheSolve environment
    m <- solve(data, ...)
    
    ## The inverse matrix is stored in m in the makeCacheMatrix Environment
    x$setInverse(m)
    
    ## Displays the inverse matrix
    m
}
