get_input <- function(aoc_id, parse = F, user = "bjebert", cache = F) {
    f <- sprintf("2023/%s.txt", gsub("/", "-", aoc_id))
    if(cache && file.exists(f)) {
        return(readLines(f))
    }
    
    library(httr)
    
    aoc_split <- as.numeric(strsplit(aoc_id, "/")[[1]])
    address <- sprintf("https://adventofcode.com/%s/day/%s/input", aoc_split[1], aoc_split[2])
    sessions <- readLines("session.txt")
    
    user_cookie_map <- c("deesblake" = 1, "bjebert" = 2, "blakeebets" = 3)
    cookie <- c(`session` = sessions[user_cookie_map[[user]]])
    
    res <- content(GET(address, set_cookies(cookie)), encoding = 'UTF-8')
    lines <- strsplit(res, "\n")[[1]]
    
    writeLines(lines, f)
    
    if(!parse) {
        return(lines)
    }
    
    # Auto-parse input
    lines_num <- suppressWarnings(as.numeric(lines))
    if(all(!is.na(lines_num))) {
        return(lines_num)
    }
    
    nc <- unique(sapply(lines, nchar))
    if(length(nc) == 1 && length(lines) > 1) {
        m <- matrix(data = strsplit(paste(lines, collapse = ""), "")[[1]], nrow = length(lines), ncol = nc, byrow = T)
        return(m)
    } else {
        return(lines)
    }
}


inp2mat <- function(inp) {
    matrix(strsplit(paste(inp, collapse = ""), "")[[1]], nrow = length(inp), ncol = nchar(inp[1]), byrow = T)
}


mat2map <- function(mat) {
    setNames(c(mat), sprintf("%s,%s", rep(1:nrow(mat), times = ncol(mat)), rep(1:ncol(mat), each = nrow(mat))))
}


map2mat <- function(map, default = ".") {
    coords <- lapply(strsplit(names(map), ","), as.numeric)
    
    min_x <- min(sapply(coords, function(x) x[2]))
    max_x <- max(sapply(coords, function(x) x[2]))
    
    min_y <- min(sapply(coords, function(x) x[1]))
    max_y <- max(sapply(coords, function(x) x[1]))
    
    nr <- max_y - min_y + 1
    nc <- max_x - min_x + 1
    
    mat2 <- matrix(NA, nrow = nr, ncol = nc)
    
    for(i in 1:nr) {
        for(j in 1:nc) {
            nm <- sprintf("%s,%s", i+min_y-1, j+min_x-1)
            mat2[i, j] <- if(nm %in% names(map)) map[[nm]] else default
        }
    }
    
    mat2
}


get_4nb <- function(pos) {
    list(c(pos[1] + 1, pos[2]),
         c(pos[1] - 1, pos[2]),
         c(pos[1], pos[2] + 1),
         c(pos[1], pos[2] - 1))
}

get_8nb <- function(pos) {
    list(
        c(pos[1] + 1, pos[2]),     # North
        c(pos[1] - 1, pos[2]),     # South
        c(pos[1], pos[2] + 1),     # East
        c(pos[1], pos[2] - 1),     # West
        c(pos[1] + 1, pos[2] + 1), # Northeast
        c(pos[1] + 1, pos[2] - 1), # Northwest
        c(pos[1] - 1, pos[2] + 1), # Southeast
        c(pos[1] - 1, pos[2] - 1)  # Southwest
    )
}


factors <- function(n) {
    n <- abs(n)
    f1 <- (1:floor(sqrt(n)))[sapply(1:floor(sqrt(n)), function(i) n %% i == 0)]
    unique(sort(c(f1, n / f1)))
}

coord2pos <- function(coord) as.numeric(c(strsplit(coord, ",")[[1]][1], strsplit(coord, ",")[[1]][2]))
pos2coord <- function(pos) sprintf("%s,%s", pos[1], pos[2])

library(plot.matrix)
rotate <- function(x) t(apply(x, 2, rev))
