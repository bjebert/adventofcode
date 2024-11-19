
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")
inp <- get_input("2015/03", parse = F, user = "blakeebets", cache = T)

# Problem -----------------------------------------------------------------

moves <- strsplit(inp, "")[[1]]
move_map <- list("^" = c(1, 0), 
                 "v" = c(-1, 0), 
                 ">" = c(0, 1), 
                 "<" = c(0, -1))

pos <- c(0, 0)
hist <- list()

for(m in moves) {
    hist <- c(hist, pos2coord(pos))
    pos <- pos + move_map[[m]]
}

length(unique(hist))

# 2:48 (2nd)

pos1 <- c(0, 0)
pos2 <- c(0, 0)
hist <- list()

for(m in moves[seq(1, length(moves), 2)]) {
    hist <- c(hist, pos2coord(pos1))
    pos1 <- pos1 + move_map[[m]]
}

for(m in moves[seq(2, length(moves), 2)]) {
    hist <- c(hist, pos2coord(pos2))
    pos2 <- pos2 + move_map[[m]]
}

length(unique(hist))

# 3:58 (1st)


