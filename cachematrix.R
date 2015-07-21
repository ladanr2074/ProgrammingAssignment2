
This function creates a special 'matrix'object than can cache its inverse .

it will create a list that contains 4 member functions ,set ,get ,setInv , getInv

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL    }
        
        get <- function() x
        setInv <- function(inv) xinv <<- inv
        getInv <- function() xinv
        list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)

}

this function get the inversed matrix from object x ,it will be null if uncalculated ,if the inversion result is there will get the message "getting cached data "and return the calculated inversion ,
if not we do x$get to get the matrix object ,we solve it then set it to the object then return the solve result

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)      }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
  
}
