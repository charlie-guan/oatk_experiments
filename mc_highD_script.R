source('./oatk.R')
source('./data.R')
source('./regression.R')
library(parallel)
library(oatk)
library(data.table)
library(foreach)
library(GM)
library(stabs)
library(glmnet)

library(knockoff)
library(MASS)
library(stringr)
library(stats)
library(utils)
library(Matrix)
library(selectiveInference)


# high-dimensional
n = 300
p = 1000
k = 30
nrep = 50L

set.seed(1876)
beta = sampleNonnull(p, k)

dt.tests = rbind(data.table(expand.grid(4:8, 1:nrep)))
#dt.tests = rbind(data.table(expand.grid(7, 1:nrep)))
setnames(dt.tests, new=c('amp', 'rep'))

lst.results = lapply(split(dt.tests, 1:nrow(dt.tests)), function(dt.row) {
    amp = dt.row$amp
    
    X = getDMCX(n, p)
    X_scaled = scale(X)/sqrt(n-1)
    y = getY(X_scaled, beta, amp)

    
    rej_oatk_ridge = oatk_screen_ridge_2(y, X_scaled)$rej
    rej_oatk_lasso = oatk_screen_lasso_2(y, X_scaled)$rej
    rej_kdmc = knockDMC(y, X)$rej
    rej_gm = gm(y, X_scaled, ncores=4L)$gm_selected
    
    half_sample = sample(1:n, floor(n/2))
    lst.lasso = lasso_prescreen(X_scaled[half_sample, ], y[half_sample])
    coef.lasso = lst.lasso$beta
    R = sum(coef.lasso!=0)
    # rej_ss = stabsel(X, y, fitfun=glmnet.lasso, args.fitfun=list(family="gaussian"), 
    #                  cutoff=0.9, PFER=floor(R*0.1),
    #                  mc.cores=2)$selected
    rej_ss = stabsel(X, y, fitfun=glmnet.lasso, args.fitfun=list(family="gaussian"), 
                     cutoff=0.9, q=R,
                     mc.cores=2)$selected
    lst.lasso = lasso_prescreen(X_scaled, y)
    rej_bh_lasso = bh_lasso(y, X, 0.1, lst.lasso)$rej

    dt.out = rbind(data.table(method='GM', fdp=calculateFDP(beta, rej_gm), tdp=calculateTDP(beta, rej_gm)),
                   data.table(method='OATK_ridge', fdp=calculateFDP(beta, rej_oatk_ridge), tdp=calculateTDP(beta, rej_oatk_ridge)),
                   data.table(method='OATK_lasso', fdp=calculateFDP(beta, rej_oatk_lasso), tdp=calculateTDP(beta, rej_oatk_lasso)),
                   data.table(method='SS', fdp=calculateFDP(beta, rej_ss), tdp=calculateTDP(beta, rej_ss)),
                   data.table(method='knockoffDMC', fdp=calculateFDP(beta, rej_kdmc), tdp=calculateTDP(beta, rej_kdmc)),
                   data.table(method='BH_lasso', fdp=calculateFDP(beta, rej_bh_lasso), tdp=calculateTDP(beta, rej_bh_lasso)))
    return(cbind(dt.row, dt.out))
})

dt.results = rbindlist(lst.results)
saveRDS(dt.results, 'dt.resultsMarkovChainHighD.RDS')



