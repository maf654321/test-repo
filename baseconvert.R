baseconvert <- function (x, base.output = 2, base.input = 10, int.if.possible = FALSE) {
        ## Terminology:
        ## ./Other/division_quotient_divisor_dividend1.gif
        # http://www.kwiznet.com/images/questions/grade3/division_quotient_divisor_dividend1.gif
        
	# Disable scientific notation
	options(scipen=999)
	
        # Prime arguments
        base <- base.output
        if (any(base > 36)) {
                stop("Base > 36 not supported")
        }
        
        # Convert non-decimal to decimal
        if (!identical(base.input, 10)) {
                x <- as.character(x)
		
		# Split number
                if (length(unlist(strsplit(x, "[.]"))) > 1) {
                        x <- unlist(strsplit(x, "[.]"))[1]
                        warning("x rounded to integer")
                }
                x <- unlist(strsplit(x, ""))
		
		# Convert LETTERS to numbers >= 10
                if (any(x %in% LETTERS)){
                        x[x %in% LETTERS] <- sapply(x[x %in% LETTERS], function (x) {
                                x <- match(x, LETTERS) + 9
                        })
                } else {}
                x <- as.numeric(x)
		
		# Validity check 1
                if (any(x >= base.input)) {
			stop("x not a valid number in base.input")
		}
		
		# Convert
		for (i in seq_along(x)) {
			x[i] <- x[i] * (base.input ^ (length(x) - i))
                }
		
		# Sum decimal result
                x <- round(sum(round(x)))
	
	# Validity check 2
	} else if (any (unlist(strsplit(x, "")) %in% LETTERS)) {
		stop("x not a valid number in base.input")
	}
        
        # Quick return decimal output
        if (identical(base, 10)) {
                if (int.if.possible) {
                        result <- as.integer(x)
                } else {
                        result <- as.character(x)
                }
                return(result)
        }
        
        # Check and format correct input
        if (as.numeric(x) != floor(as.numeric(x))) {
                x <- as.integer(x)
                warning("x rounded to integer")
        } else {
                x <- as.integer(x)
        }
        
        # Inputting divident
        div <- x
        
        # Priming number result
        num <- c()
        
        # Converting to base using quotient and remainder
        while (!identical(div, 0)) {
                quot <- floor(div/base)
                rem <- (div/base - floor(div/base))*base
                div <- quot
                num <- c(rem, num)
        }
        
        # Convert numbers >= 10 into LETTERS
        num <- as.integer(round(num))
        if (any(num >= 10)) {
                num[num >= 10] <- sapply(num[num >= 10] - 9, function(x) {
                        paste0(LETTERS[x], collapse="")
                })
                lettrs <- TRUE
        } else {
                lettrs <- FALSE
        }
        
        # Check if letters are present
        if (!lettrs & int.if.possible) {
                result <- as.integer(paste(num, collapse = ""))               
        } else {
                result <- paste(num, collapse = "")
        }
        
	# Re-enable scientific notation
	options(scipen = 0)
	
        # Return Result
        return(result)
}