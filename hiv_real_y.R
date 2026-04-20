#setwd("~/Documents/northwestern/research/oatk")
source('./oatk.R')
source('./data.R')
source('./regression.R')
library(parallel)


###### Real benchmark with real y
# Load HIV data and set up params
set.seed(1000)

# remove rows with inf
dat = getHIVDataWithY()
dat = dat[!is.infinite(dat$Y), ]

y = dat$Y
y = y - mean(y)
n = length(y)
X = dat[, -1]
p = dim(X)[2]
mutations = colnames(X)
mutations = str_replace(mutations, 'X.', '')
X = scale(X) / sqrt(n-1)



# variable selection
rej_bh = sort(mutations[bh(y, X)$rej]) 
rej_bc = sort(mutations[bc(y, X)$rej]) 
rej_gm = sort(mutations[gm_low_d(y, X)$rej])  
rej_oatk = sort(mutations[oatk(y, X)$rej])


length(rej_oatk) 
length(rej_gm) 
length(rej_bc) 
length(rej_bh) 

# uniquely identified mutations by each method
paste0(sort(setdiff(rej_oatk, unique(c(rej_gm, rej_bc, rej_bh)))), collapse=', ')

paste0(sort(setdiff(rej_gm, unique(c(rej_oatk, rej_bc, rej_bh)))), collapse=', ')

paste0(sort(setdiff(rej_bc, unique(c(rej_oatk, rej_gm, rej_bh)))), collapse=', ')

paste0(sort(setdiff(rej_bh, unique(c(rej_oatk, rej_gm, rej_bc)))), collapse=', ')



