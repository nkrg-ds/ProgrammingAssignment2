## makeCacheMatrix and cacheSolve functions allow you to save matrix and 
## the inverse of the matrix in a cache, so that you can retrieve the inverse matrix 
## any time you need subsequently without the need to re-calculate the inverse matrix. 
## Since, computing a reverse matrix is an expensive computation, cacheing calculated 
## inverse matrices this way will help reduce the processing and make your program faster
## in case you need the inverse of a matrix multiple times in your program.
## These functions also illustrate the lexical scoping concepts used in R.
## Note: matlib library is invoked at the beginning since the function to calculate 
## inverse of matrix comes from this library.
## Many thanks to Mr. Greski for his excellent article with explanation of makeVector and  
## cacheMean functions. 


library(matlib)

## makeCacheMatrix function will create an object with 2 data values, one for the matrix 
## whose inverse need to be calculated and another for the inverse calculated. Secondly, it 
## creates following 4 functions - 
## 1. set - This function will set the value of the input matrix and also initialize the inverse matrix.
##          This function is not called initially since when you create your cache object using 
##          makeCacheMatrix function, the value of your input function is set by passing it as a input 
##          argument and the inverse matrix value is initialized to NULL at beginning of the function.
##          This function can be used later to pass new input matrix to your object. When called this
##          way it will set the value of input matrix and initialized value of the inverse matrix
##          in the environment parent function using super assignment operator. This will make these 2 
##          data variable available to other 3 functions.
##          
## 2. get - This function will return the input matrix from cache.
## 3. setInverseMatrix - This function will save the calculated inverse matrix in the cache.
## 4. getInverseMatrix - This function will return the inverse matrix from the cache.

makeCacheMatrix <- function(ipMatrix = matrix()) {
    inverseMatrix <- NULL
    setMatrix <- function(i_ipMatrix) {
        ipMatrix <<- i_ipMatrix
        inverseMatrix <<- NULL
    }
    getMatrix <- function() ipMatrix
    setInverseMatrix <- function(i_inverseMatrix) inverseMatrix <<- i_inverseMatrix
    getInverseMatrix <- function() inverseMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve function will first try to get the inverse matrix funtion from object 
## created by makeCacheMatrix function. If it gets NULL value for the inverse matrix, 
## then it know to calculate the inverse for the matrix which is already saved in the cache.
## It will then read the input matrix using "get" function. It then checks if the matrxi 
## can be inversed by checking that its determinant is not zero. If the matrix can be calculated,
## then it will inverse the matrix using "inv" function from the "matlib" library. Otherwise, it 
## will save NA to inverse matrix variable so the calling program knows the matrix can not be 
## reversed. It then saves the value of calculated inverse matrix back in to the myMatrix object
## so that the inverse can be retrive later without having to compute it again. It then returns
## the value of inverse matrix to calling function.

cacheSolve <- function(myMatrix, ...) {
    ## Return a matrix that is the inverse of 'myMatrix'
    inverseMatrix <- myMatrix$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached inverse matrix")
        return(inverseMatrix)
    }
    #retrieve the matrxi saved in the object
    myMatrix_recall <- myMatrix$getMatrix()   
    #if the determinant is not zero then inverse matrix can be calculated
    if (det(myMatrix_recall) != 0) {    
        inverseMatrix = inv(myMatrix_recall)
    } 
    # else set the inverse matrix value to NA so your program will know the matrix 
    # can not be reversed
    else { 
        inverseMatrix = NA
    }
    myMatrix$setInverseMatrix(inverseMatrix)
    inverseMatrix
}