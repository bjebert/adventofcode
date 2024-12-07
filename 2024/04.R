
# Load --------------------------------------------------------------------

rm(list = ls())
source("utilities.R")

# Problem -----------------------------------------------------------------

inp <- strsplit("MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX", "\n")[[1]]  # Example

inp <- get_input("2024/04", parse = F, user = "bjebert", cache = F)

m <- inp2mat(inp)

# search horizontal
cnt <- 0
for(i in 1:nrow(m)) {
    for(j in 1:(ncol(m)-3)) {
        cnt <- cnt + as.numeric(paste(m[i,j:(j+3)], collapse = "") == "XMAS")
    }
    
    for(j in ncol(m):4) {
        cnt <- cnt + as.numeric(paste(m[i,j:(j-3)], collapse = "") == "XMAS")
    }
}


for(i in 1:ncol(m)) {
    for(j in 1:(nrow(m)-3)) {
        cnt <- cnt + as.numeric(paste(m[j:(j+3),i], collapse = "") == "XMAS")
    }
    
    for(j in nrow(m):4) {
        cnt <- cnt + as.numeric(paste(m[j:(j-3),i], collapse = "") == "XMAS")
    }
}

# search diagonal

map <- mat2map(m)

for(pos in names(map)) {
    p <- coord2pos(pos)
    
    # diagonal down right
    cnt <- cnt + tryCatch({
        as.numeric(paste(map[sapply(list(p, p + 1, p + 2, p + 3), pos2coord)], collapse = "") == "XMAS")
    }, error = function(x) 0)
    
    cnt <- cnt + tryCatch({
        as.numeric(paste(map[sapply(list(p, p - 1, p - 2, p - 3), pos2coord)], collapse = "") == "XMAS")
    }, error = function(x) 0)
    
    cnt <- cnt + tryCatch({
        as.numeric(paste(map[sapply(list(p, c(p[1] - 1, p[2] + 1), c(p[1] - 2, p[2] + 2),c(p[1] - 3, p[2] + 3)), pos2coord)], collapse = "") == "XMAS")
    }, error = function(x) 0)   
    
    cnt <- cnt + tryCatch({
        as.numeric(paste(map[sapply(list(p, c(p[1] + 1, p[2] - 1), c(p[1] + 2, p[2] - 2),c(p[1] + 3, p[2] - 3)), pos2coord)], collapse = "") == "XMAS")
    }, error = function(x) 0)
}

cnt


# 15..
# part 2 ------------------------------------------------------------------

map_a <- names(map)[map == "A"]
cnt <- 0
for(a in map_a) {
    p <- coord2pos(a)
    
    corners <- sapply(list(c(p[1]-1, p[2]-1), c(p[1]-1,p[2]+1), c(p[1]+1,p[2]-1), c(p[1]+1,p[2]+1)), pos2coord)
    
    if(all(corners %in% names(map))) {
        cnt <- cnt + as.numeric(paste(map[corners], collapse = "") %in% c("MSMS", "MMSS", "SSMM", "SMSM"))
    }
}

cnt



