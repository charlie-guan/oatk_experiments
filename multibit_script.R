#setwd("~/Documents/northwestern/research/oatk")
source('./oatk.R')
source('./data.R')
source('./regression.R')
library(parallel)

# set data
n = 1000
p = 300
k = 30

nrep = 100L

set.seed(20241213)
beta = sampleNonnull(p, k)

dt.tests = rbind(data.table(expand.grid(c('power_decay', 'const_pos', 'const_neg'), 3:8, 1:nrep, c(1L, 5L, 10L, 15L, 20L,  25L))))
setnames(dt.tests, new=c('mode', 'amp', 'rep', 'M'))
dt.tests[, mode:=as.character(mode)]


lst.results = mclapply(split(dt.tests, 1:nrow(dt.tests)), function(dt.row) {
  mode = dt.row$mode
  amp = dt.row$amp
  M = dt.row$M
  
  X = getGaussianX(n, p, mode=mode)
  y = getY(X, beta, amp)
  lst.out = oatk_multiple(y, X, M=M)
  rej = lst.out$rej
  
  dt.out = data.table(fdp=calculateFDP(beta, rej), tdp=calculateTDP(beta, rej))
  return(cbind(dt.row, dt.out))
	}, mc.cores=128L)

dt.results = rbindlist(lst.results)
print(dt.results[, lapply(.SD, mean), keyby=.(mode, amp, M)])
saveRDS(dt.results, 'dt.resultsMultiple.RDS')
