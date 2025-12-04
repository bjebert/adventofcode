binpad <- function(a, b) {  # Get padded binary representation of character/bigz inputs
    ac <- as.character(as.bigz(a), b = 2)
    bc <- as.character(as.bigz(b), b = 2)
    
    n_ac <- nchar(ac)
    n_bc <- nchar(bc)
    max_nc <- pmax(nchar(ac), nchar(bc))
    
    pad_a <- max_nc - n_ac
    pad_b <- max_nc - n_bc
    
    zero_a <- sapply(pad_a, function(x) paste(rep("0", x), collapse = ""))
    zero_b <- sapply(pad_b, function(x) paste(rep("0", x), collapse = ""))
    
    a_pad <- sprintf("%s%s", zero_a, ac)
    b_pad <- sprintf("%s%s", zero_b, bc)
    
    bin_c <- list(strsplit(a_pad, ""), strsplit(b_pad, ""))
    bin_n <- lapply(bin_c, function(x) lapply(x, as.numeric))
    
    return(bin_n)
}

bigAnd <- function(a, b) {
    stopifnot(length(a) == length(b))
    brep <- binpad(a, b)  
    
    do.call(c, sapply(1:length(a), function(i) {
        as.bigz(sprintf("0b%s", paste(as.numeric(brep[[1]][[i]] == 1 & brep[[2]][[i]] == 1), collapse = "")))
    }))
}

bigOr <- function(a, b) {
    stopifnot(length(a) == length(b))
    brep <- binpad(a, b)  
    
    do.call(c, sapply(1:length(a), function(i) {
        as.bigz(sprintf("0b%s", paste(as.numeric(brep[[1]][[i]] == 1 | brep[[2]][[i]] == 1), collapse = "")))
    }))
}

bigXor <- function(a, b) {  # Arbitrary-length bitwise XOR for R
    stopifnot(length(a) == length(b))
    brep <- binpad(a, b)  
    
    do.call(c, sapply(1:length(a), function(i) {
        as.bigz(sprintf("0b%s", paste(as.numeric(brep[[1]][[i]] != brep[[2]][[i]]), collapse = "")))
    }))
}

bigNot <- function(a) {   # differs to bitwNot as they assume 32-bit integer, pad out and set sign bit
    as <- strsplit(as.character(as.bigz(a), b = 2), "")[[1]]
    as.bigz(sprintf("0b%s", paste(as.numeric(as == "0"), collapse = "")))
}
