## Las siguientes funciones almacenan en caché la inversa de una matriz.

## Esta función crea una matriz especial, establece y obtiene su inversa.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##Esta función calcula la inversa de la matriz creada con la función anterior. Primero compruebasi ya se ha calculado, si es así, la obtiene de la memoria caché, 
##de lo contrario, calcula la inversa y la establece en la memoria caché a través de la función “setinverse”.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


x <- matrix(sample(1:9,size = 9), 
            nrow = 3, 
            ncol = 3)

y <- matrix(sample(1:9,size = 9), 
            nrow = 3, 
            ncol = 3)

mymatrix <- makeCacheMatrix(y)
mymatrix$getinverse()

cacheSolve(mymatrix)
mymatrix$getinverse()
