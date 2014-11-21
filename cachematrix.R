## function name   : makeCacheMatrix
## function purpose: This function creates a special "matrix" object that can cache its inverse
## author : ddedmari
## created: 21 nov 2014


makeCacheMatrix <- function(input_matrix = matrix()) {

        inverse_matrix <- NULL  # inverse_matrix will be our inverse and it's reset to NULL every time makeCacheMatrix is called
        
        get_matrix <- function() { input_matrix }   # this function returns the value of the original matrix
        
        set_inverse <- function(inverse)  { inverse_matrix <<- inverse }
        # this is called by cacheSolve() during the first cacheSolve()
        #  access and it will store the value using superassignment
        
        get_inverse <- function() { inverse_matrix } # this will return the cached value to cacheSolve() on
        #  subsequent accesses
        
        set_matrix <- function(y) {    # takes an input vector
                input_matrix <<- y         # saves the input vector 
                inverse_matrix <<- NULL      # resets the mean to NULL, basically what happens when a new object is generated.
        }
        
        list(get_matrix = get_matrix,          #  OK, this is accessed each time makeVector() is called, 
             set_matrix = set_matrix,
             set_inverse = set_inverse,  #   that is, each time we make a new object.  This is a list of 
             set_inverse = set_inverse)  #   the internal functions ('methods') so a calling function
        #   knows how to access those methods. 
        
        
        
}


## function name   : cacheSolve
## function purpose: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##                   If the inverse has already been calculated (and the matrix has not changed), then the 
##                   cachesolve should retrieve the inverse from the cache.
## author : ddedmari
## created: 21 nov 2014

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse_matrix <- x$getmean()               # accesses the object 'x' and gets the value of the mean
        if(!is.null(inverse_matrix)) {              # if mean was already cached (not NULL) ...
                
                message("getting cached data")  # ... send this message to the console
                return(inverse_matrix)                       # ... and return the mean ... "return" ends 
                #   the function cachemean(), note
        }
        data <- x$get()        # we reach this code only if x$getmean() returned NULL
        inverse_matrix <- mean(data, ...)   # if m was NULL then we have to calculate the mean
        x$setmean(inverse_matrix)           # store the calculated mean value in x (see setmean() in makeVector
        inverse_matrix               # return the mean to the code that called this function        
        
        
        
        
        
        
}
