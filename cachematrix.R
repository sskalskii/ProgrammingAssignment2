###################################################################
# The purpose of this code is to demonstrate caching of structures#
# where computation time can ge rather large & it is advantageous #
# to store a computed object once rather than recompute it n times#
###################################################################


###################################################################
#                      makeCacheMatrix                            #
###################################################################
# makeCacheMatrix creates and returns a list of functions used to #
# process a matrix. In this case they are the set, get, setinv, & #
# getinv functions defined below. The input variable is matrix()  #
###################################################################
makeCacheMatrix <- function(x = matrix()) {
  ## input: x - a square invertible matrix
  ## return: - a list containing the following functions
  ##      set:     set the value of the vector
  ##      get:     get the value of the vector
  ##      setinv:  set the value of the inverse
  ##      getinv:  get the value of the inverse
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  
  ## list of functions returned
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}

###################################################################
#                        cacheSolve                               #
###################################################################
# cacheSolve returns a matrix that is the inverse of 'x'. If  the #
# getinv (i.e. x$getinv())does not find the value in cache then   #
# the inverse is computed and setinv() is called to place it into #
# the cache                                                       #
###################################################################
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("retrieving from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  
  # returns the inverse
  i
}
###################################################################
#                    outer block test case                        #
###################################################################
matrix1 <- matrix(c(1,2,3,4),2,2) # test matrix
fList <- makeCacheMatrix(matrix1) # create function list
cacheSolve(fList) #inverse returned after computation
cacheSolve(fList) #inverse returned from cache (msg printed)
