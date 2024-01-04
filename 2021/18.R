input <- readLines("18.txt")

eqn <- "2 * 3 + (4 * 5)"

resolve_nb_equation <- function(nb_eqn) {
    nums <- as.numeric(strsplit(nb_eqn, "\\+|\\*")[[1]])
    signs <- strsplit(gsub("[0-9]", "", nb_eqn), "")[[1]]
    
    if(length(nums) == 1) {
        return(nums[1])
    }
    
    lhs <- nums[1]

    
    for(i in 2:length(nums)) {
        if(signs[(i-1)] == "+") {
            lhs <- lhs + nums[i]
        } else if(signs[(i-1)] == "*") {
            lhs <- lhs * nums[i]
        }
    }
    
    return(lhs)
}

resolve_nb_equation_b <- function(nb_eqn) {
    nums <- as.numeric(strsplit(nb_eqn, "\\+|\\*")[[1]])
    signs <- strsplit(gsub("[0-9]", "", nb_eqn), "")[[1]]
    
    while(any("+" %in% signs) == TRUE) {
        si <- which(signs == "+")[1]
        
        nums[si] <- nums[si] + nums[si+1]
        nums <- nums[1:length(nums) != si + 1]
        signs <- signs[1:length(signs) != si]
    }
    
    if(length(nums) == 1) {
        return(nums[1])
    }
    
    lhs <- nums[1]
    for(i in 2:length(nums)) {
        if(signs[(i-1)] == "*") {
            lhs <- lhs * nums[i]
        }
    }
    
    return(lhs)
}


evaluate_eqn <- function(eqn) {
    eqn_trim <- gsub(" ", "", eqn)
    
    # Evaluate expressions inside brackets first
    
    while(grepl("\\(", eqn_trim) == TRUE) {
        open_i <- gregexpr("\\(", eqn_trim)[[1]]
        ci <- gregexpr("\\)", eqn_trim)[[1]][1]
        oi <- max(open_i[open_i < ci])
        
        nb_eqn <- substr(eqn_trim, oi + 1, ci - 1)
        eqn_trim <- sprintf("%s%s%s", substr(eqn_trim, 1, oi - 1), resolve_nb_equation_b(nb_eqn), substr(eqn_trim, ci + 1, nchar(eqn_trim)))
    }
    
    return(resolve_nb_equation_b(eqn_trim))
}

sum(sapply(input, evaluate_eqn))
