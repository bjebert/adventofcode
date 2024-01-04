inp <- readLines("2023/2.txt")

inp <- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

inp <- strsplit(inp, "\n")[[1]]

# 12 red, 13 green, 14 blue
# sum IDs

games <- strsplit(sapply(strsplit(inp, ":"), function(x) x[2]), ";")

sum(sapply(games, function(x) {
    rounds <- strsplit(x, ",")
    
    blues <- c()
    reds <- c()
    greens <- c()
    
    for(rnd in rounds) {
        rnd <- trimws(rnd)
        
        z <- strsplit(rnd, " ")
        
        for(y in z) {
            col <- y[2]
            amt <- as.numeric(y[1])
            
            if(col == "red") {
                reds <- c(reds, amt)
            } else if(col == "green") {
                greens <- c(greens, amt)
            } else if(col == "blue") {
                blues <- c(blues, amt)
            }
        }
    }
    
    return(max(blues) * max(reds) * max(greens))
}))

