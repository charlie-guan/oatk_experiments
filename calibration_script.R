#setwd("~/Documents/northwestern/research/oatk")
source('./oatk.R')
source('./data.R')
source('./regression.R')
library(parallel)

set.seed(2025)

k = 30

dt.tests = rbind(data.table(expand.grid(c('power_decay', 'const_pos', 'const_neg'), 6, 1:100)))
setnames(dt.tests, new=c('mode', 'amp', 'rep'))
dt.tests[, mode:=as.character(mode)]

# n=400
n = 200
p = 100
beta = sampleNonnull(p, k)

lst.results = mclapply(split(dt.tests, 1:nrow(dt.tests)), function(dt.row) {
  mode = dt.row$mode
  amp = dt.row$amp

  X = getGaussianX(n, p, mode=mode)
  y = getY(X, beta, amp)

  lst.oatk = oatk(y, X)
  rej_oatk = lst.oatk$rej
  rej_coatk = calibrateOATK(y, X, lst.oatk)$rej


  dt.out = rbind(data.table(method='OATK', fdp=calculateFDP(beta, rej_oatk), tdp=calculateTDP(beta, rej_oatk)),
    data.table(method='cOATK', fdp=calculateFDP(beta, rej_coatk), tdp=calculateTDP(beta, rej_coatk)))
  return(cbind(dt.row, dt.out))
  }, mc.cores=128L)

dt.results = rbindlist(lst.results)
saveRDS(dt.results, 'dt.resultsCalibration_n200.RDS')

# n=400
set.seed(2025)
n = 400
p = 200
beta = sampleNonnull(p, k)

lst.results = mclapply(split(dt.tests, 1:nrow(dt.tests)), function(dt.row) {
  mode = dt.row$mode
  amp = dt.row$amp
  
  X = getGaussianX(n, p, mode=mode)
  y = getY(X, beta, amp)

  lst.oatk = oatk(y, X)
  rej_oatk = lst.oatk$rej
  rej_coatk = calibrateOATK(y, X, lst.oatk)$rej

  
  dt.out = rbind(data.table(method='OATK', fdp=calculateFDP(beta, rej_oatk), tdp=calculateTDP(beta, rej_oatk)),
    data.table(method='cOATK', fdp=calculateFDP(beta, rej_coatk), tdp=calculateTDP(beta, rej_coatk)))
  return(cbind(dt.row, dt.out))
  }, mc.cores=128L)

dt.results = rbindlist(lst.results)
saveRDS(dt.results, 'dt.resultsCalibration_n400.RDS')


# set data
set.seed(2025)
n = 1000
p = 300
beta = sampleNonnull(p, k)


lst.results = mclapply(split(dt.tests, 1:nrow(dt.tests)), function(dt.row) {
  mode = dt.row$mode
  amp = dt.row$amp
  
  X = getGaussianX(n, p, mode=mode)
  y = getY(X, beta, amp)
  
  lst.oatk = oatk(y, X)
  rej_oatk = lst.oatk$rej
  rej_coatk = calibrateOATK(y, X, lst.oatk)$rej
  
  
  dt.out = rbind(data.table(method='OATK', fdp=calculateFDP(beta, rej_oatk), tdp=calculateTDP(beta, rej_oatk)),
                 data.table(method='cOATK', fdp=calculateFDP(beta, rej_coatk), tdp=calculateTDP(beta, rej_coatk)))
  return(cbind(dt.row, dt.out))
}, mc.cores=128L)

dt.results = rbindlist(lst.results)
saveRDS(dt.results, 'dt.resultsCalibration_n1000.RDS')
