inp <- readLines("C:/Users/blake/PycharmProjects/AOC/2021/5.txt")

start <- lapply(strsplit(sapply(strsplit(inp, " -> "), function(x) x[1]), ","), function(x) as.numeric(x))
end <- lapply(strsplit(sapply(strsplit(inp, " -> "), function(x) x[2]), ","), function(x) as.numeric(x))

grid <- matrix(0, nrow = 1000, ncol = 1000)

for(i in 1:length(start)) {
    if(start[[i]][1] == end[[i]][1]) {
        grid[start[[i]][1], start[[i]][2]:end[[i]][2]] <- grid[start[[i]][1], start[[i]][2]:end[[i]][2]] + 1
    } else if(start[[i]][2] == end[[i]][2]) {
        grid[start[[i]][1]:end[[i]][1], start[[i]][2]] <- grid[start[[i]][1]:end[[i]][1], start[[i]][2]] + 1
    } else {
        diag_x <- start[[i]][1]:end[[i]][1]
        diag_y <- start[[i]][2]:end[[i]][2]
        
        for(j in 1:length(diag_x)) {
            grid[diag_x[j], diag_y[j]] <- grid[diag_x[j], diag_y[j]] + 1
        }
    }
}


print(sum(grid > 1))  # 4:25

print(sum(grid > 1))  # 5:42

