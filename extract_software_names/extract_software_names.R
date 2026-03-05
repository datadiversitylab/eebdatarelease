sf <- readLines("software.txt")
sf = c("", sf)
spaces <- which(sf == "")

categories <- spaces[c(TRUE, FALSE)]+1
snames <- sf[categories]

s_c <- lapply(seq_along(categories), function(x){
    if(x != length(categories)){
    soft <- seq(categories[x]+2, categories[(x+1)]-2 )
    data.frame(category = sf[categories[x]], software = sf[soft])
    }else{
    soft <- seq(categories[x]+2, length(sf) )
    data.frame(category = sf[categories[x]], software = sf[soft])
    }
})

soft <- do.call("rbind", s_c)
soft2 <- aggregate(category ~ software, data = soft, FUN = function(x) paste(unique(x), collapse = ";"))
write.csv(soft2, "software.csv")
