baseconvert <- function (x, base.output = 2, base.input = 10, int.if.possible = FALSE) {
        ## Terminology:
        ## ./Other/division_quotient_divisor_dividend1.gif
        # http://www.kwiznet.com/images/questions/grade3/division_quotient_divisor_dividend1.gif
        
        # Disable scientific notation
        options(scipen=999)
        
        # Make base list
        # object_list_old <- objects()                          #pulls local evironment
        # y <- x; x <- c()                                      #exempts x from check
        object_list_old <- c("base.input", "base.output")       #explicitly defined
        
        # Checking base length
        if (!identical(base.input, base.input[1]) || !identical(base.output, base.output[1])) {
                object_list <- c()
                for (i in seq_along(object_list_old)) {        
                        if (!identical(get(object_list_old[i])[1], get(object_list_old))) {
                                object_list <- c(object_list, object_list_old[i])
                        }
                }
                for (i in seq_along(object_list)) {
                        assign(object_list[i], get(object_list[i])[1])
                }
                warning("only first element of vector used for base")
        }
        
        # Interpret character bases to numeric vector
        if (is.character(base.input) || is.character(base.output)) {
                object_list <- c()
                for (i in seq_along(object_list_old)) {
                        if (identical(class(get(object_list_old[i])), "character")) {
                                object_list <- c(object_list, object_list_old[i])
                        }
                }
                for (i in seq_along(object_list)) {
                        if (get(object_list[i]) %in% "binary") {
                                assign(object_list[i], 2)
                        } else if (get(object_list[i]) %in% "ternary") {
                                assign(object_list[i], 3)
                        } else if (get(object_list[i]) %in% "quaternary") {
                                assign(object_list[i], 4)
                        } else if (get(object_list[i]) %in% "quinary") {
                                assign(object_list[i], 5)
                        } else if (get(object_list[i]) %in% "senary") {
                                assign(object_list[i], 6)
                        } else if (get(object_list[i]) %in% "heximal") {
                                assign(object_list[i], 6)
                        } else if (get(object_list[i]) %in% "septenary") {
                                assign(object_list[i], 7)
                        } else if (get(object_list[i]) %in% "octal") {
                                assign(object_list[i], 8)
                        } else if (get(object_list[i]) %in% "nonary") {
                                assign(object_list[i], 9)
                        } else if (get(object_list[i]) %in% "decimal") {
                                assign(object_list[i], 10)
                        } else if (get(object_list[i]) %in% "undecimal") {
                                assign(object_list[i], 11)
                        } else if (get(object_list[i]) %in% "dozenal") {
                                assign(object_list[i], 12)
                        } else if (get(object_list[i]) %in% "duodecimal") {
                                assign(object_list[i], 12)
                        } else if (get(object_list[i]) %in% "tridecimal") {
                                assign(object_list[i], 13)
                        } else if (get(object_list[i]) %in% "tetradecimal") {
                                assign(object_list[i], 14)
                        } else if (get(object_list[i]) %in% "pentadecimal") {
                                assign(object_list[i], 15)
                        } else if (get(object_list[i]) %in% "hex") {
                                assign(object_list[i], 16)
                        } else if (get(object_list[i]) %in% "hexadecimal") {
                                assign(object_list[i], 16)
                        } else if (get(object_list[i]) %in% "vigesimal") {
                                assign(object_list[i], 20)
                        } else if (get(object_list[i]) %in% "quatrovigesimal") {
                                assign(object_list[i], 24)
                        } else if (get(object_list[i]) %in% "trigesimal") {
                                assign(object_list[i], 30)
                        } else if (get(object_list[i]) %in% "hexatridecimal") {
                                assign(object_list[i], 36)
                        } else {
                                assign(object_list[i], suppressWarnings(as.numeric(get(object_list[i]))))
                                if (anyNA(get(object_list[i]))) {
                                        stop("base not recognized")
                                }
                        }
                }
        }
        
        # Check if base > 36 or base not whole number
        if (any(round(base.input) > 36 || round(base.output) > 36)) {
                stop("base > 36 not supported")
        } else if (any(round(base.input) < 2 || round(base.output) < 2)) {
                stop("base < 2 not possible")
        } else if (!identical(base.input, round(base.input)) || !identical(base.output, round(base.output))) {
                object_list <- c()
                for (i in seq_along(object_list_old)) {
                        if (!identical(get(object_list_old[i]), round(get(object_list_old[i])))) {
                                object_list <- c(object_list, object_list_old[i])
                        }
                }
                for (i in seq_along(object_list)) {
                        assign(object_list[i], round(get(object_list[i])))
                }
                warning("base rounded to integer")
        }
        
        # Replace base, check x length
        # x <- y; rm(y)                 not necessary when explicily defining object_list_old
        base <- base.output
        if (!identical(x, x[1])) {
                x <- x[1]
                warning("only first element of vector used for x")
        }
        
        # Convert non-decimal to decimal
        if (!identical(base.input, 10)) {
                x <- toupper(as.character(x))
                
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
        } else if (any (unlist(strsplit(toupper(as.character(x)), "")) %in% LETTERS)) {
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
                rem <- (div/base - floor(div/base))*base
                div <- floor(div/base)
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