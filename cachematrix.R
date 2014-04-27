# Assignment: Caching the inverse of a matrix
# =====================================================================================================
# Description:
# ------------
# Matrix inversion is usually a costly computation and their may be some benefit to caching 
# the inverse of a matrix rather than compute it repeatedly (there are also alternatives to 
# matrix inversion that we will not discuss here). 

# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# Write the following functions:

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#             above. If the inverse has already been calculated (and the matrix has not changed), then 
#             the cachesolve should retrieve the inverse from the cache.

# NOTE:  Computing the inverse of a square matrix can be done with the solve function in R. For 
#        example, if X is a square invertible matrix, then solve(X) returns its inverse.

# ====================================================================================================
makeCacheMatrix <- function(mat = matrix())
{

  inv_mat   <- NULL
  flag     <<- FALSE                           # It indicates if the inverse matrix must be calculated or not    
  setmatrix    <- function(y)                  # it allows changing the matrix   
  {
    if (compare(y) == FALSE) inv_mat <<- NULL  # if matrix changes -> calculate inverse matrix again                          
    mat     <<- y
  }
  getmatrix    <- function() mat               # get matrix
  setinverse   <- function(inv)                # set inverse matrix  
  {                                            
    inv_mat <<- inv                            
    flag    <<- TRUE                            
  }  
  getinverse  <- function() inv_mat            # get inverse matrix 

  #This function compare if matrix changes
  compare <- function(y)              
  {
    flag   <<- TRUE                   
    if ((nrow(y) == nrow(mat)) && (ncol(y) == ncol(mat)))   
    {
      for (i in 1:nrow(y)) {                   
        for (j in 1:ncol(y)) {
          if (y[i,j] != mat[i,j]) ind <<- FALSE
        }
      }
    }
    else
    {
      flag <<- FALSE
    }
    flag
  } 
  
  list( setmatrix  = setmatrix, 
        getmatrix  = getmatrix, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

# =======================================================================================================
cacheSolve <- function(mat_spec, ...) 
{
  inv_mat <- mat_spec$getinverse                # get value of inverse matrix
  if (!is.null(inv_mat) && (flag == TRUE))      # If not Null or not changes 
  {
    message("getting cached inverse matrix")
  }                                         
  else 
  { 
    message("Calculating Inverse")
    data   <- mat_spec$getmatrix()              # Calculating inverse matrix 
    inv_mat <- solve(data)                     
    mat_spec$setinverse(inv_mat)              
  }  
  inv_mat                                       # return inverse matrix   
}

