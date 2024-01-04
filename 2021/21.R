
input <- strsplit("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)", "\n")[[1]]

input <- readLines("21.txt")

ingredients <- strsplit(sapply(strsplit(input, " \\("), function(x) x[1]), " ")
allergies <- strsplit(gsub("\\)", "", sapply(strsplit(input, "contains "), function(x) x[2])), ", ")

all_ingredients <- unique(unlist(ingredients))
all_allergies <- unique(unlist(allergies))

allergy_map <- rep(list(all_allergies), length(all_ingredients))
names(allergy_map) <- all_ingredients

#' Each allergen is found in exactly one ingredient. 
#' Each ingredient contains zero or one allergen. 
#' Allergens aren't always marked; when they're listed (as in (contains nuts, shellfish) after an ingredients list), 
#' the ingredient that contains each listed allergen will be somewhere in the corresponding ingredients list. 
#' However, even if an allergen isn't listed, the ingredient that contains that allergen could still be present


for(ingredient in names(allergy_map)) {
    for(i in 1:length(ingredients)) {
        if(!(ingredient %in% ingredients[[i]])) {
            
            #' If there is a row where the ingredient does not appear, it can not possibly contain any of the
            #' allergies in that row, as each allergy can only be contained by one ingredient.
            
            allergy_map[[ingredient]] <- allergy_map[[ingredient]][!(allergy_map[[ingredient]] %in% allergies[[i]])]
        }
    }    
}

zero_ingredients <- names(allergy_map[sapply(allergy_map, function(x) length(x) == 0)])
sum(sapply(zero_ingredients, function(x) sum(x == unlist(ingredients))))

#' Part two: let's remove zero ingredients from the list to give us some clarity

ingredients <- lapply(ingredients, function(x) x[!(x %in% zero_ingredients)])
allergy_map <- allergy_map[sapply(allergy_map, length) > 0]

while(!(all(sapply(allergy_map, length) == 1))) {
    resolved <- unlist(allergy_map[sapply(allergy_map, length) == 1])
    unresolved <- names(allergy_map)[sapply(allergy_map, length) > 1]
    
    for(allergy in unresolved) {
        allergy_map[[allergy]] <- allergy_map[[allergy]][!(allergy_map[[allergy]] %in% resolved)]
    }
}

allergy_dt <- data.table(ingredient = names(allergy_map), allergy = unlist(allergy_map))
allergy_dt <- allergy_dt[order(allergy)]

allergy_dt[, paste(ingredient, collapse = ",")]
