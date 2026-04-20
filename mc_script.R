#setwd("~/Documents/northwestern/research/oatk")
source('./oatk.R')
source('./data.R')
source('./regression.R')
library(parallel)

# small n and p
n = 1000
p = 300
k = 30
nrep = 100L

set.seed(1876)
beta = sampleNonnull(p, k)

dt.tests = rbind(data.table(expand.grid(3:8, 1:nrep)))
setnames(dt.tests, new=c('amp', 'rep'))


lst.results = mclapply(split(dt.tests, 1:nrow(dt.tests)), function(dt.row) {
  amp = dt.row$amp
  
  X = getDMCX(n, p)
  y = getY(scale(X)/sqrt(n-1), beta, amp)

  rej_bh = bh(y, X)$rej
  rej_bc = bc(y, X)$rej
  rej_gm = gm_low_d(y, X)$rej
  rej_kdmc = knockDMC(y, X)$rej
  rej_oatk = oatk(y, X)$rej
  rej_oatkd = oatk_derandomized(y, X)$rej
  
  dt.out = rbind(data.table(method='BH', fdp=calculateFDP(beta, rej_bh), tdp=calculateTDP(beta, rej_bh)),
    data.table(method='BC', fdp=calculateFDP(beta, rej_bc), tdp=calculateTDP(beta, rej_bc)),
    data.table(method='GM', fdp=calculateFDP(beta, rej_gm), tdp=calculateTDP(beta, rej_gm)),
    data.table(method='knockoffDMC', fdp=calculateFDP(beta, rej_kdmc), tdp=calculateTDP(beta, rej_kdmc)),
    data.table(method='OATK', fdp=calculateFDP(beta, rej_oatk), tdp=calculateTDP(beta, rej_oatk)),
    data.table(method='OATK_derand', fdp=calculateFDP(beta, rej_oatkd), tdp=calculateTDP(beta, rej_oatkd)))
  return(cbind(dt.row, dt.out))
	}, mc.cores=128L)

dt.results = rbindlist(lst.results)
print(dt.results[, lapply(.SD, mean), keyby=.(amp, method)])
saveRDS(dt.results, 'dt.resultsMCSmall.RDS')



# large n and p
n = 2000
p = 1000
k = 30

nrep = 100L

set.seed(1876)
beta = sampleNonnull(p, k)

dt.tests = rbind(data.table(expand.grid(3:8, 1:nrep)))
setnames(dt.tests, new=c('amp', 'rep'))


lst.results = mclapply(split(dt.tests, 1:nrow(dt.tests)), function(dt.row) {
  amp = dt.row$amp
  
  X = getDMCX(n, p)
  y = getY(scale(X)/sqrt(n-1), beta, amp)

  rej_bh = bh(y, X)$rej
  rej_bc = bc(y, X)$rej
  rej_gm = gm_low_d(y, X)$rej
  rej_kdmc = knockDMC(y, X)$rej
  rej_oatk = oatk(y, X)$rej
  rej_oatkd = oatk_derandomized(y, X)$rej
  
  dt.out = rbind(data.table(method='BH', fdp=calculateFDP(beta, rej_bh), tdp=calculateTDP(beta, rej_bh)),
    data.table(method='BC', fdp=calculateFDP(beta, rej_bc), tdp=calculateTDP(beta, rej_bc)),
    data.table(method='GM', fdp=calculateFDP(beta, rej_gm), tdp=calculateTDP(beta, rej_gm)),
    data.table(method='knockoffDMC', fdp=calculateFDP(beta, rej_kdmc), tdp=calculateTDP(beta, rej_kdmc)),
    data.table(method='OATK', fdp=calculateFDP(beta, rej_oatk), tdp=calculateTDP(beta, rej_oatk)),
    data.table(method='OATK_derand', fdp=calculateFDP(beta, rej_oatkd), tdp=calculateTDP(beta, rej_oatkd)))
  return(cbind(dt.row, dt.out))
  }, mc.cores=128L)

dt.results = rbindlist(lst.results)
print(dt.results[, lapply(.SD, mean), keyby=.(amp, method)])
saveRDS(dt.results, 'dt.resultsMCLarge.RDS')
