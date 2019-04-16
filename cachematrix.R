makeCacheMatrix <- function(x=matrix()){        # Creating function which takes in a matrix as argument
  i <- NULL                                     # i stands for inverse, initialising the variable as null
  
    # set function assigns the input argument to the x object which is stored in parent environment, and
    # again makes i variable equal to Null again, so to clear any prior values stored in i
    set <- function(y) {                          
    x <<- y
    i <<- NULL
    }
       
  get <- function() x                           # get function just retrieves the x value
  setinverse <- function(inverse) i <<- inverse # setinverse sets the inverse value to the variable i
  getinverse <- function() i                    # getinverse retrievve the value in i
  list(set = set, get = get,                    # Creates a named list of all the above functions
       setinverse = setinverse,
       getinverse = getinverse)
  
}

# cacheSolve function does 2 things - first checks if the matrix inverse is stored in the cach or not, if stored already - 
# it just retrieve the same value without any computation
# If the value is not found, i.e if the matrix is a new one, it then computes the inverse and assign the same to the -
# setinverse function of MakeCacheMatrix function

cacheSolve <- function(x, ...){                 # function takes a argument x which is the output of makeCacheMatrix function
  i <- x$getinverse()
  if(!is.null(i)) {                             # Checking if the inverse value is present in cache or not
    message("getting cached data")
    return(i)
  } 
  data <- x$get()                               
  i <- solve(data, ...)                         # Calculating inverse if value is not present in the cache
  x$setinverse(i)                               # setting the inverse value to the setinverse function
  i                                             # Printing the inverse value                 
}

