rm(list=ls())
source("utilities.R")
inp <- get_input("2023/5", parse = F, deesblake = F, cache = T)

inp <- strsplit("seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4", "\n")[[1]]

inp


seeds <- as.numeric(strsplit(strsplit(inp[grepl("seeds:", inp)], ": ")[[1]][2], " ")[[1]])

map1 <- inp[(which(grepl("seed-to-soil map", inp)) + 1):(which(grepl("soil-to-fertilizer", inp)) - 2)]
map2 <- inp[(which(grepl("soil-to-fertilizer", inp)) + 1):(which(grepl("fertilizer-to-water", inp)) - 2)]
map3 <- inp[(which(grepl("fertilizer-to-water", inp)) + 1):(which(grepl("water-to-light", inp)) - 2)]
map4 <- inp[(which(grepl("water-to-light", inp)) + 1):(which(grepl("light-to-temperature", inp)) - 2)]
map5 <- inp[(which(grepl("light-to-temperature", inp)) + 1):(which(grepl("temperature-to-humidity", inp)) - 2)]
map6 <- inp[(which(grepl("temperature-to-humidity", inp)) + 1):(which(grepl("humidity-to-location", inp)) - 2)]
map7 <- inp[(which(grepl("humidity-to-location", inp)) + 1):length(inp)]

maps <- list(lapply(map1, function(x) as.numeric(strsplit(x, " ")[[1]])),
             lapply(map2, function(x) as.numeric(strsplit(x, " ")[[1]])),
             lapply(map3, function(x) as.numeric(strsplit(x, " ")[[1]])),
             lapply(map4, function(x) as.numeric(strsplit(x, " ")[[1]])),
             lapply(map5, function(x) as.numeric(strsplit(x, " ")[[1]])),
             lapply(map6, function(x) as.numeric(strsplit(x, " ")[[1]])),
             lapply(map7, function(x) as.numeric(strsplit(x, " ")[[1]])))

# source category -> destination category
# seed number (source) -> soil number (dest)

# each line: dest range START, source range START, length

# first line: dest=50, source=98, range=2

min(sapply(seeds, function(seed) {
    val <- seed
    
    for(i in 1:length(maps)) {
        m <- maps[[i]]
        for(map in m) {
            dest_start <- map[1]
            src_start <- map[2]
            map_len <- map[3]
            
            if(val >= src_start && val <= src_start + map_len - 1) {
                val <- val - (src_start - dest_start)
                break()
            }
        }
    }
    return(val)
}))


# part 2 ------------------------------------------------------------------

seed_starts <- seeds[seq(1, length(seeds), 2)]
seed_lens <- seeds[seq(2, length(seeds), 2)]
mins <- c()

# Track possible ranges
for(j in 1:length(seed_starts)) {
    seed_ranges <- list(c(seed_starts[j], seed_starts[j] + seed_lens[j] - 1))
    
    for(i in 1:length(maps)) {
        next_ranges <- list()
        m <- maps[[i]]
        
        for(map in m) {
            dest_start <- map[1]
            src_start <- map[2]
            map_len <- map[3]
            
            src_end <- src_start + map_len - 1
            
            for(sr in seed_ranges) {
                if(sr[1] <= src_end && sr[2] >= src_start) {  # chop range up into new possible ranges
                    
                    if(sr[1] >= src_start && sr[2] <= src_end) {  # whole range fits in
                        next_ranges[[length(next_ranges) + 1]] <- c(sr[1] - src_start + dest_start,
                                                                    sr[2] - src_start + dest_start)
                    } else {  # range can be split into two new ranges
                        
                        # part of the range that fits in:
                        if(sr[1] < src_start && sr[2] >= src_start) {
                            next_ranges[[length(next_ranges) + 1]] <- c(sr[1], src_start - 1)  # Unchanged for the part that doesn't fit
                            
                            next_ranges[[length(next_ranges) + 1]] <- c(dest_start, dest_start + sr[2] - src_start)  
                        } else if(sr[2] > src_end && sr[1] <= src_end) {
                            next_ranges[[length(next_ranges) + 1]] <- c(src_end + 1, sr[2])  # Unchanged for the part that doesn't fit
                            
                            # Map the part that does fit
                            next_ranges[[length(next_ranges) + 1]] <- c(sr[1] - src_start + dest_start, src_end - (src_start - dest_start))
                        } else {
                            stop()
                        }
                    }
                }
            }
        }
        
        if(length(next_ranges) > 0) {
            seed_ranges <- copy(next_ranges)
        }
    }
    
    mins <- c(mins, min(sapply(seed_ranges, min)))
}

min(mins)


