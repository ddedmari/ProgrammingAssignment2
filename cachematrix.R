## function name   : makeCacheMatrix
## function purpose: This function creates a special "matrix" object that can cache its inverse
## author : ddedmari
## created: 21 nov 2014


makeCacheMatrix <- function(input_matrix = matrix()) {

        inverse <- NULL  # inverse will be our inverse matrix and it's reset to NULL every time makeCacheMatrix is called
        
        get_matrix <- function() { input_matrix }   # this function returns the value of the original matrix
        
        set_inverse <- function(inverse_matrix) 
                {                               # this is called by cacheSolve() during the first cacheSolve()
                inverse <<- inverse_matrix      # access and it will store the value using superassignment
                }
                                                                                                       
        get_inverse <- function() { inverse }   # this will return the cached value to cacheSolve() on
                                                # subsequent accesses
        
        set_matrix <- function(new_matrix)
                {                               # takes an input vector
                input_matrix <<- new_matrix     # saves the input vector 
                inverse_matrix <<- NULL         # resets the inverse to NULL when a new object is generated.
                }
        
        list(get_matrix = get_matrix,           # list of the internal functions, accessed each time makeVector() is called        
             set_matrix = set_matrix,
             get_inverse = get_inverse,  
             set_inverse = set_inverse)          
}


## function name   : cacheSolve
## function purpose: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##                   If the inverse has already been calculated (and the matrix has not changed), then the 
##                   cachesolve should retrieve the inverse from the cache.
## author : ddedmari
## created: 21 nov 2014

cacheSolve <- function(my_obj, ...)
{   ## Return a matrix that is the inverse of 'my_obj'
        
        
        inverse <- my_obj$get_inverse() # accesses the object 'my_obj' and gets the value of the inverse
        
        if(!is.null(inverse))
                {                               # if inverse was already cached (not NULL) ...
                
                message("getting cached data")  # ... send this message to the console
                return(inverse)                 # ... and return the inverse 
               
                }
        data <- my_obj$get_matrix()        
        inverse <- solve(data, ...)     # if inverse was NULL then calculate the inverse
        my_obj$set_inverse(inverse)     # store the calculated inverse in my_obj
        inverse                         # return the inverse to the code that called this function                
}


############## here are the test cases/commands to test the functions above #################################

## source('~/ProgrammingAssignment2/cachematrix.R') -- source the file cachematrix.R

## mat1 <- matrix(1:4, 2,2)  -- create a 2*2 matrix called mat1

## matObj1 <- makeCacheMatrix(mat1) -- pass mat1 to makeCacheMatrix() to create matObj1

## cacheSolve(matObj1) -- No cache value, hence calculate inverse and return

## cacheSolve(matObj1) -- Cache hit, just return inverse

## mat2 <- matrix(c(2,2,3,2), nrow=2,ncol=2)  -- create another 2*2 matrix called mat2

## matObj2 <- makeCacheMatrix(mat2)  -- pass mat2 to makeCacheMatrix() to create matObj2

## cacheSolve(matObj2)  -- No cache value, hence calculate inverse and return

## cacheSolve(matObj2)  -- Cache hit, just return inverse

## matObj1 <- makeCacheMatrix(mat2)  -- replace the first input matrix mat1 by the second matrix mat2 in matObj1

## cacheSolve(matObj1)  -- No cache value, hence calculate inverse and return

## cacheSolve(matObj1)  -- Cache hit, just return inverse
