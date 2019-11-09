# geckos
## read data for each species

data_w <- read.csv("../data/western_banded_gecko.csv", stringsAsFactors=F, header=F, colClasses=rep("character"))
dim(data_w)

data_b <- read.csv("../data/bent-toed_gecko.csv", stringsAsFactors=F, header=F, colClasses=rep("character"))
dim(data_b)

data_l <- read.csv("../data/leopard_gecko.csv", stringsAsFactors=F, header=F, colClasses=rep("character"))
dim(data_l)


## calculate divergence between sequences of B and L

sites_total <- 0
sites_divergent <- 0 

for (i in 1:ncol(data_b)) {
  
  ### you need to discard SNPs within each species
  #Going through each column and if the alleles are only 1 then add them to a list 
  if (length(unique(data_b[,i]))==1 & length(unique(data_l[,i]))==1) {
    
    sites_total <- sites_total + 1
    
    ### if different, then it's a divergent site
    #If the sites are the same int hese two areas then they are not divergent 
    #If they are different then they are divergent 
    # The =! means not equal to so this translates to:
    # If the first allele in each column is not equal in the two sites put it 
    #as a divergent site in the sites_divergent and move on 
    if (data_b[1,i] != data_l[1,i]) sites_divergent <- sites_divergent + 1
    
  }
}
### divergence rate
#To recap, sites total is every site which and divergent is just those that diverge 
#calculate rate by dividing these 
div_rate_BL <- sites_divergent / sites_total


## calculate divergence between sequences of W and L

sites_total <- 0
sites_divergent <- 0

for (i in 1:ncol(data_w)) {
  
  ### you need to discard SNPs within each species
  if (length(unique(data_w[,i]))==1 & length(unique(data_l[,i]))==1) {
    
    sites_total <- sites_total + 1
    
    ### if different, then it's a divergent site
    if (data_w[1,i] != data_l[1,i]) sites_divergent <- sites_divergent + 1
    
  }
}
### divergence rate
div_rate_WL <- sites_divergent / sites_total


## calculate divergence between sequences of W and B

sites_total <- 0
sites_divergent <- 0

for (i in 1:ncol(data_w)) {
  
  ### you need to discard SNPs within each species
  if (length(unique(data_w[,i]))==1 & length(unique(data_b[,i]))==1) {
    
    sites_total <- sites_total + 1
    
    ### if different, then it's a divergent site
    if (data_w[1,i] != data_b[1,i]) sites_divergent <- sites_divergent + 1
    
  }
}
### divergence rate
div_rate_WB <- sites_divergent / sites_total


## from these divergence rates we can infer that W and B are close species while L is the outgroup

## estimate mutation rate per site per year
mut_rate <- div_rate_BL / (2 * 5e7)


## estimate divergence time
div_time <- div_rate_WB / (2 * mut_rate)

cat("\nThe two species have a divergence time of", div_time, "years.")
cat("\nThe most likely species tree is L:(W:B).")

