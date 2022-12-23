# load packages
library(RCurl)
library(XML)
library(tidyverse)

# download html biosample from stains
cepas <- read_tsv("20203010_Assemblies_Efaecalis.tsv")

# html request to ncbi database in parallel
cl<- parallel::makeCluster(30, type = "FORK")
html.table <- parallel::parSapply(cl = cl, cepas$strain, function(s){
  df<- cepas[cepas$strain == s,]
    html <- getURL(paste0("https://www.ncbi.nlm.nih.gov/biosample/?term=",df$biosample), followlocation = F)
    html.table <- readHTMLTable(html)
    return(html.table)
}, simplify = F, USE.NAMES = T)
parallel::stopCluster(cl)

# Parse html to metadadta 
metadata <- lapply(html.table, function(h){
  if(length(h) > 0 ){
    metadata<- as.data.frame(h[[1]],stringsAsFactors = F)
  }else{
    metadata <- data.frame()
  }
    metadata <- rbind(metadata, setNames(data.frame(names(metadata)[1], names(metadata)[2]), names(metadata)))
  return(metadata)
})

metadata <- metadata[sapply(metadata, function(d) nrow(d) != 1)]
metadata <- lapply(names(metadata), function(s){
  setNames(metadata[[s]], c("Data", s))
})

# Parse metadata 
metadata2 <- lapply(metadata, function(t){
  t <- as.data.frame(apply(t,2, as.character), stringsAsFactors = F)
  df <- t[!t[[2]] %in% c("missing", "unknown", "not applicable", "Unknown", "Missing", "not collected", "Not Available", "Not Applicable", "not provided", "Not Applicable", "Unpublished" , "not determined"),]
  try(df[df[[1]] == "isolation source", 2] <- paste0(as.character(df[df[[1]] == "isolation source",2]), collapse = ", "))
  df[!duplicated(df[[1]]),]
})

# Create table
metadata.table <- Reduce(function(...) merge(..., by="Data", all=T), metadata2)
xlsx::write.xlsx(t(metadata.table), "metadata_Efecalis.xlsx", row.names = T, col.names = F, showNA = F)
