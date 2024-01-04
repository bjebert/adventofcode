rm(list=ls())
source("utilities.R")
inp <- get_input("2022/7", parse = F, deesblake = F, cache = F)

inp <- strsplit("$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k", "\n")[[1]]

i <- 2
dir <- "/"
fs <- list("/" = list())

while(i <= length(inp)) {
    line <- inp[i]
    ss <- strsplit(line, " ")[[1]]
    cmd <- ss[2]
    
    if(cmd == "cd") {
        cd_dir <- ss[3]
        
        if(cd_dir == "..") {
            ss <- strsplit(dir, "/")[[1]]
            if(length(ss) == 1) {
                dir <- "/"
            } else {
                dir <- paste(ss[1:(length(ss) - 1)], collapse = "/")
            }
        } else if(dir == "/" || cd_dir == "/") {
            dir <- cd_dir
        } else {
            dir <- sprintf("%s/%s", dir, cd_dir)
        }
        
        i <- i + 1
        
    } else if(cmd == "ls") {
        files <- character(0)
        while(TRUE) {
            i <- i + 1
            if(i > length(inp) || substr(inp[i], 1, 1) == "$") {
                break
            } else {
                path <- strsplit(inp[i], " ")[[1]]
                mod1 <- if(dir == "/") "" else dir
                mod2 <- if(dir == "/") "" else "/"
                files <- c(files, sprintf("%s %s%s%s", path[1], mod1, mod2, path[2]))
            }
        }
        
        fs[[dir]] <- files
    }
}


get_size <- function(dir) {
    size <- 0
    for(obj in fs[[dir]]) {
        ss <- strsplit(obj, " ")[[1]]
        if(ss[1] == "dir") {
            if(!(ss[2]) %in% names(sizes)) {
                stop(dir)
            }
            if(sizes[[ss[2]]] != -1) {
                size <- size + sizes[[ss[2]]]
            } else {
                size <- size + get_size(ss[2])
            }
        } else {
            size <- size + as.numeric(ss[1])
        }
    }
    
    sizes[[dir]] <<- size
    return(size)
}

sizes <- lapply(fs, function(x) -1)  # cache
res <- setNames(lapply(names(fs), get_size), names(fs))

sum(as.numeric(res[res < 100000]))


# Part 2 ------------------------------------------------------------------

amt <- sizes[["/"]] - 4e7
min(as.numeric(sizes)[(as.numeric(sizes) - amt) > 0])
