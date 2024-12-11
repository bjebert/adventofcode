options(scipen = 99)
source("utilities/data.R")

# Helpful libraries

library(plot.matrix)
library(combinat)     # t(combinat::combn(1:10, 3))
library(gtools)       # gtools::permutations(v = letters[1:10], n = 10, r = 3)
library(openssl)      # openssl::md5


# Permutations/combinations -----------------------------------------------

comb <- function(set, n) t(combinat::combn(set, n))
perm <- function(set, n, repeats = FALSE) gtools::permutations(v = set, n = length(set), r = n, repeats.allowed = repeats)

# Matrix ------------------------------------------------------------------

rotate <- function(x) t(apply(x, 2, rev)) 

# Grid --------------------------------------------------------------------

move_map <- list(`^` = c(-1, 0), N = c(-1, 0), U = c(-1, 0), 
                 `>` = c(0, 1), E = c(0, 1), R = c(0, 1), 
                 `v` = c(1, 0), S = c(1, 0), D = c(1, 0), 
                 `<` = c(0, -1), W = c(0, -1), L = c(0, -1))

get_4nb <- function(pos) {
    list(c(pos[1] - 1, pos[2]),  # North
         c(pos[1] + 1, pos[2]),  # South
         c(pos[1], pos[2] + 1),  # East
         c(pos[1], pos[2] - 1))  # West
}

get_8nb <- function(pos) {
    list(
        c(pos[1] - 1, pos[2]),     # North
        c(pos[1] + 1, pos[2]),     # South
        c(pos[1], pos[2] + 1),     # East
        c(pos[1], pos[2] - 1),     # West
        c(pos[1] - 1, pos[2] + 1), # Northeast
        c(pos[1] - 1, pos[2] - 1), # Northwest
        c(pos[1] + 1, pos[2] + 1), # Southeast
        c(pos[1] + 1, pos[2] - 1)  # Southwest
    )
}


factors <- function(n) {
    n <- abs(n)
    f1 <- (1:floor(sqrt(n)))[sapply(1:floor(sqrt(n)), function(i) n %% i == 0)]
    unique(sort(c(f1, n / f1)))
}

str2pos <- function(str) as.numeric(strsplit(str, ",", fixed = TRUE)[[1]])
pos2str <- function(pos) paste0(pos, collapse = ",")

get_4nb_str_slow <- function(str) {
    sapply(get_4nb(str2pos(str)), pos2str)
}

get_8nb_str_slow <- function(str) {  # slow implementation
    sapply(get_8nb(str2pos(str)), pos2str)
}

get_4nb_str <- function(str) {  # fast implementation
    pos <- str2pos(str)
    return(sprintf("%d,%d",
        c(pos[1] + 1, pos[1] - 1, pos[1], pos[1]),
        c(pos[2], pos[2], pos[2] + 1, pos[2] - 1)
    ))
}

get_8nb_str <- function(str) {  # fast implementation
    pos <- str2pos(str)
    return(sprintf("%d,%d",
        c(pos[1] + 1, pos[1] - 1, pos[1], pos[1], pos[1] + 1, pos[1] + 1, pos[1] - 1, pos[1] - 1),
        c(pos[2], pos[2], pos[2] + 1, pos[2] - 1, pos[2] + 1, pos[2] - 1, pos[2] + 1, pos[2] - 1)
    ))
}
