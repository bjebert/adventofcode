
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47", "\n")[[1]]  # Example

inp <- get_input("2024/05", parse = F, user = "bjebert", cache = F)

w <- which(inp == "")
rules <- inp[1:(w-1)]
pages <- inp[(w+1):length(inp)]

pages <- lapply(strsplit(pages, ","), as.numeric)


# part 1 ------------------------------------------------------------------

check_order <- function(p) {
    for(i in 1:(length(p)-1)) {
        for(j in (i+1):length(p)) {
            if(sprintf("%s|%s", p[j], p[i]) %in% rules) {
                return(list(i, j))
            }
        }
    }
    return(TRUE)
}

is_ordered <- sapply(pages, function(x) !is.list(check_order(x)))
corr <- pages[is_ordered]

idx <- sapply(corr, length) %/% 2 + 1
sum(sapply(1:length(corr), function(i) {
   corr[[i]][idx[i]] 
}))


# part 2 ------------------------------------------------------------------

p2 <- pages[!is_ordered]
p <- p2[[1]]

z <- lapply(p2, function(p) {
    while(TRUE) {
        res <- check_order(p)
        
        if(!is.list(res)) {
            break
        }
        
        # swap elements        
        i <- res[[1]]
        j <- res[[2]]
        
        p_new <- copy(p)
        p_new[i] <- p[j]
        p_new[j] <- p[i]
        p <- copy(p_new)
    }
    return(p)
})

idx <- sapply(z, length) %/% 2 + 1
sum(sapply(1:length(z), function(i) {
    z[[i]][idx[i]] 
}))


