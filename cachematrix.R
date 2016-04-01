## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#example:
#x = rbind(c(1, -1/2), c(-1/2, 1))
#cx <- makeCacheMatrix(x)
#cx$get()
#     [,1] [,2]
#[1,]  1.0 -0.5
#[2,] -0.5  1.0
#cacheSolve(cx)
#          [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333
#cx$getinv()
#          [,1]      [,2]
#[1,] 1.3333333 0.6666667
#[2,] 0.6666667 1.3333333




#the function return a list and it has 4 subfunctions:
#set, store a new matrix and delete the old inverse matrix.
#get, return the value of the matrix.
#setinv, store de inverse matrix.
#getinv, return the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

#this function calculates the inverse with solve(), uses the subfunctions
#of makeCacheMatrix to get the matrix and store the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    #if there is inverse is no necessary recalculate it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #get the original matrix
    data <- x$get()
    #calculate the inverse matrix
    m <- solve(data, ...)
    #store the inverse matrix
    x$setinv(m)
    #disp the inverse matrix
    m
}
