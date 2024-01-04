library(data.table)

inp <- strsplit("px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}", "\n")[[1]]

inp <- readLines("2023/19.txt")

w_blank <- which(inp == "")

workflow_names <- sapply(strsplit(inp[1:(w_blank-1)], "\\{"), function(x) x[1])
workflow_desc <- gsub("\\}", "", sapply(strsplit(inp[1:(w_blank-1)], "\\{"), function(x) x[2]))
workflows <- lapply(setNames(strsplit(workflow_desc, ","), workflow_names), function(x) {
    sapply(gsub(">", ":GT:", gsub("<", ":LT:", x)), function(flow) {
        strsplit(flow, "<|>|\\:")
    }, USE.NAMES = F)
})

# Part 2 ------------------------------------------------------------------


combine_bounds <- function(bounds, target_bounds) {
    lapply(target_bounds, function(tb) {
        setNames(lapply(names(tb), function(x) {
            c(max(c(tb[[x]][1], bounds[[x]][1])),
              min(c(tb[[x]][2], bounds[[x]][2])))
        }), names(tb))
    })
}


add_flow <- function(tree, flow, bounds = NULL) {
    if(is.null(bounds)) {
        bounds <- list("x" = c(1, 4000),
                       "m" = c(1, 4000),
                       "a" = c(1, 4000),
                       "s" = c(1, 4000))
    }
    
    if(length(flow[[1]]) == 1) {
        if(flow[[1]][1] == "A") {
            return(list(bounds))
        } else if(flow[[1]][1] == "R") {
            return(NULL)
        } else {
            target_bounds <- tree[[flow[[1]][1]]]
            return(combine_bounds(bounds, target_bounds))
        }
    }
    
    var <- flow[[1]][1]
    cond <- flow[[1]][2]
    tgt <- as.numeric(flow[[1]][3])
    state <- flow[[1]][4]
    
    bounds_success <- copy(bounds)
    bounds_failure <- copy(bounds)
    
    if(cond == "GT") {
        bounds_success[[var]][1] <- tgt + 1
        bounds_failure[[var]][2] <- tgt
    } else {
        bounds_success[[var]][2] <- tgt - 1
        bounds_failure[[var]][1] <- tgt
    }
    
    combos <- c(add_flow(tree, flow[[1]][4], bounds_success),
                add_flow(tree, flow[-1], bounds_failure))
    
    combos <- combos[!sapply(combos, is.null)]
    
    return(combos)
}


tree <- lapply(workflows, function(x) NULL)
remaining <- copy(workflows)
absorbent_states <- c("A", "R")


while(length(remaining) > 0) {
    print(length(remaining))
    
    absorbent <- names(remaining)[sapply(remaining, function(x) {
        all(sapply(x, function(z) tail(z, 1)) %in% absorbent_states)
    })]
    
    for(flow_name in absorbent) {
        tree[[flow_name]] <- add_flow(tree, workflows[[flow_name]])
    }
    
    remaining <- remaining[-which(names(remaining) %in% absorbent)]
    absorbent_states <- c(absorbent_states, absorbent)
}


sum_states <- function(tree, flow) {
    sum(sapply(tree[[flow]], function(x) prod(sapply(x, function(x) x[2] - x[1] + 1))))
}

sum_states(tree, "in")
