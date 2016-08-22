
#Programming Assignment 2

make_cache_Matrix <- function(x) {

## Initializing null inverse value

            cache_inv <- NULL

## setting matrix 
            set <- function(user_input = matrix()) {
                    x <<- user_input
                    cache_inv <<- NULL
            }

            get <- function() x

            set_inv <- function(solve) inv <<- solve
            get_inv <- function() cache_inv
            list(set = set, get = get,
                 set_inv = set_inv,
                 get_inv = get_inv)
    }


cache_solve_Matrix <- function(x, ...) {

## Checking to see if Matrix inverse has already been calculated and getting its value from the Cache

            calculated_inv <- x$getinv()
            if(!is.null(calculated_inv)) && is.matrix(calculated_inv {
                    message("Getting cached values of Inverse")
                    return(calculated_inv)
            }

	else {
		message("Inverse is already calculated")

		}

## If Matrix is not inverted, getting the matrix

            Matrix_to_invert <- x$get()
            calculated_inv <- solve(Matrix_to_invert, ...)

## Displaying the Final inverted matrix

	message("Inverse of your matrix")
            x$setinv(calculated_inv)
            calculated_inv
    }