
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- get_input("2024/25", parse = F, user = "bjebert", cache = F)
inp

inp <- strsplit("#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####", "\n")[[1]]  # Example

w <- c(which(inp == ""), length(inp) + 1)

locks <- list()
keys <- list()

for(x in w) {
    m <- inp2mat(inp[(x-7):(x-1)])
    heights <- apply(m, 2, function(x) sum(x == '#')) - 1
    
    if(all(m[1,] == "#")) {
        # lock
        locks[[length(locks) + 1]] <- heights
    } else if(all(m[7,] == "#")) {
        # key
        keys[[length(keys) + 1]] <- heights
    } else {
        stop(x)
    }
}

# max 5 between each lock and key

sum(sapply(locks, function(lock) {
    sum(sapply(keys, function(key) {
        all((lock + key) <= 5)
    }))
}))

# 6:51