library(data.table)
library(ggplot2)
library(ggh4x)

### Gaussian low d
dt.results = readRDS('dt.resultsGaussianSmall.RDS')
dt.results[, method:=factor(method, c('BH', 'BC', 'GM', 'OATK', 'OATK_derand'),c('BH', 'KF', 'GM', 'OATK', 'OATK\nderand'))]
dt.results[, mode:=factor(mode, c('power_decay', 'const_pos', 'const_neg'),
                         labels= c('Power Decay', 'Constant Positive', 'Constant Negative'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('mode', 'amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value), sd=sd(value)), keyby=.(mode, method, variable, amp)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
  facet_grid2(mode ~ variable, scales='free_y', independent='y') + 
  geom_point() + geom_line() + xlab('Signal Amplitude') +
  geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
  theme_bw() + theme(axis.title.y=element_blank()) + 
  geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
  labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_small_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)

dt.scatter = dt.sub[amp==5L]
dt.scatter[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDP', 'TDP'))]

plt = ggplot(dt.scatter, 
             aes(x=method, y=value, color=method)) + geom_boxplot() +
  facet_grid2(mode ~ variable, scales='free_y', independent='y') + theme_bw() + 
  theme(axis.title.y=element_blank()) +xlab('Method') + labs(color='Method') + 
  theme(legend.position = "none") +   geom_hline(data=data.table(y=0.1, variable='FDP'), mapping=aes(yintercept=y), linetype='dashed')
plt  
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_small_box.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)

### Gaussian high d
dt.results = readRDS('dt.resultsGaussianLarge.RDS')
dt.results[, method:=factor(method, c('BH', 'BC', 'GM', 'OATK', 'OATK_derand'),c('BH', 'KF', 'GM', 'OATK', 'OATK\nderand'))]
dt.results[, mode:=factor(mode, c('power_decay', 'const_pos', 'const_neg'),
                          labels= c('Power Decay', 'Constant Positive', 'Constant Negative'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('mode', 'amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value), sd=sd(value)), keyby=.(mode, method, variable, amp)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
  facet_grid2(mode ~ variable, scales='free_y', independent='y') + 
  geom_point() + geom_line() + xlab('Signal Amplitude') +
  geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
  theme_bw() + theme(axis.title.y=element_blank()) + 
  geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
  labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_large_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)

dt.scatter = dt.sub[amp==5L]
dt.scatter[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDP', 'TDP'))]

plt = ggplot(dt.scatter, 
             aes(x=method, y=value, color=method)) + geom_boxplot() +
  facet_grid2(mode ~ variable, scales='free_y', independent='y') + theme_bw() + 
  theme(axis.title.y=element_blank()) +xlab('Method') + labs(color='Method') + 
  theme(legend.position = "none") +   geom_hline(data=data.table(y=0.1, variable='FDP'), mapping=aes(yintercept=y), linetype='dashed')
plt  
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_large_box.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)


### multi bit plots

dt.results = readRDS('dt.resultsMultiple.RDS')
dt.results[, method:=paste0('OATK_', M)]
dt.results[, method:=factor(method,
           levels=c('OATK_1', 'OATK_5', 'OATK_10', 'OATK_15', 'OATK_20', 'OATK_25', 'OATK_30'),
           labels=c('OATK_1', 'OATK_5', 'OATK_10', 'OATK_15', 'OATK_20', 'OATK_25', 'OATK_30'))]
dt.results[, exp:=factor(mode, c('power_decay', 'const_pos', 'const_neg'),
                         labels= c('Power Decay', 'Constant Positive', 'Constant Negative'))]
dt.sub = melt(dt.results[, .(exp, amp, method, fdp, tdp)], 
              id.vars=c('exp', 'amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value), sd=sd(value)), keyby=.(exp, method, variable, amp)]
dt.mean[variable=='fdp', variable:='fdr']
dt.mean[, variable:=factor(variable, c('fdr', 'tdp'), labels=c('FDR', 'TDP'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='TDP', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK_1']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)


plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
  facet_grid2(exp ~ variable, scales='free_y', independent='y') + 
  geom_point() + geom_line() + xlab('Signal Amplitude') +
  geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
  theme_bw() + theme(axis.title.y=element_blank()) + 
  geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
  labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/multi_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)

dt.scatter = dt.sub[amp==5L]
dt.scatter[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDP', 'TDP'))]
dt.scatter[, method:=factor(method,
                            levels=c('OATK_1', 'OATK_5', 'OATK_10', 'OATK_15', 'OATK_20', 'OATK_25', 'OATK_30'),
                            labels=c('1', '5', '10', '15', '20', '25', '30'))]
plt = ggplot(dt.scatter, 
             aes(x=method, y=value, color=method)) + geom_boxplot() +
  facet_grid2(exp ~ variable, scales='free_y', independent='y') + theme_bw() + 
  theme(axis.title.y=element_blank()) +xlab('M') + labs(color='Method') + 
  theme(legend.position = "none") +   geom_hline(data=data.table(y=0.1, variable='FDP'), mapping=aes(yintercept=y), linetype='dashed')
plt  
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/multi_box.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)



########### Markov Chain
dt.results = rbindlist(list(small=readRDS('dt.resultsMCSmall.RDS')
, large=readRDS('dt.resultsMCLarge.RDS')
), idcol='mode')


dt.results[, method:=factor(method, c('BH', 'BC', 'GM', 'knockoffDMC', 'OATK', 'OATK_derand'),c('BH', 'KF', 'GM', 'Knockoff\nDMC', 'OATK', 'OATK\nderand'))]
dt.results[, mode:=factor(mode, c('small', 'large'),
                          labels= c('Small p and n', 'Large p and n'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('mode', 'amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value), sd=sd(value)), keyby=.(mode, method, variable, amp)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
  facet_grid2(mode ~ variable, scales='free_y', independent='y') + 
  geom_point() + geom_line() + xlab('Signal Amplitude') +
  geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
  theme_bw() + theme(axis.title.y=element_blank()) + 
  geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
  labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/mc_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)

dt.scatter = dt.sub[amp==5L]
dt.scatter[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDP', 'TDP'))]

plt = ggplot(dt.scatter, 
             aes(x=method, y=value, color=method)) + geom_boxplot() +
  facet_grid2(mode ~ variable, scales='free_y', independent='y') + theme_bw() + 
  theme(axis.title.y=element_blank()) +xlab('Method') + labs(color='Method') + 
  theme(legend.position = "none") +   geom_hline(data=data.table(y=0.1, variable='FDP'), mapping=aes(yintercept=y), linetype='dashed')
plt  
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/mc_box.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)


### HIV
dt.results = readRDS('dt.resultsHIV.RDS')


dt.results[, method:=factor(method, c('BH', 'BC','GM',  'OATK', 'OATK_derand'),c('BH', 'KF','GM',  'OATK', 'OATK\nderand'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value), sd=sd(value)), keyby=.(method, variable, amp)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
  facet_grid2(. ~ variable, scales='free_y', independent='y') + 
  geom_point() + geom_line() + xlab('Signal Amplitude') +
  geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
  theme_bw() + theme(axis.title.y=element_blank()) + 
  geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
  labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/hiv_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=10)


### Conditional calibration
dt.results = rbindlist(list(`200`=readRDS('dt.resultsCalibration_n200.RDS'),
                            `400`=readRDS('dt.resultsCalibration_n400.RDS'),
                            `1000`=readRDS('dt.resultsCalibration_n1000.RDS')), idcol='n')
dt.results[, mode:=factor(mode, c('power_decay', 'const_pos', 'const_neg'),
                          labels= c('Power Decay', 'Constant Positive', 'Constant Negative'))]
dt.results[, method:=factor(method, c('OATK', 'cOATK'))]
dt.results[, n:=as.numeric(n)]
dt.sub = melt(dt.results[, -c("rep", 'amp')], id.vars=c('n', 'mode', 'method'))

dt.mean = dt.sub[, .(value=mean(value), sd=sd(value)), keyby=.(mode, method, variable, n)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(n=200L, value=0.20)]
dt.anchor[variable=='Power', `:=`(n=200L, value=1.0)]
dt.anchor[, method:='OATK']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(n=200L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=n, y=value, color=method, shape=method)) + 
  facet_grid2(mode ~ variable, scales='free_y', independent='y') + 
  geom_point() + geom_line() + xlab('n') +
  geom_point(data=dt.anchor, mapping=aes(x=n, y=value), size=0, alpha=0) +
  #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
  theme_bw() + theme(axis.title.y=element_blank()) + 
  geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
  labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom") + xlim(200, 1000)
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/calibration_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)


# Toeplitz lowd small
dt.results = readRDS('dt.resultsToeplitzLowD_small.RDS')
dt.results = dt.results[method!='OATK_ridge_derand']
dt.results[, method:=factor(method, 
                            c('BH', 'BC', 'GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso'),
                            c('BH', 'KF', 'GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value, na.rm=T), sd=sd(value, na.rm=T)), 
                 keyby=.(method, variable, amp)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK_ridge']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
    facet_grid2(. ~ variable, scales='free_y', independent='y') + 
    geom_point() + geom_line() + xlab('Signal Amplitude') +
    geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
    #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
    theme_bw() + theme(axis.title.y=element_blank()) + 
    geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
    labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_toeplitz_lowd_small_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)

# Toeplitz lowd large
dt.results = readRDS('dt.resultsToeplitzLowD_large.RDS')
dt.results = dt.results[method!='OATK_ridge_derand']
dt.results[, method:=factor(method, 
                            c('BH', 'BC', 'GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso'),
                            c('BH', 'KF', 'GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value, na.rm=T), sd=sd(value, na.rm=T)), 
                 keyby=.(method, variable, amp)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK_ridge']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
    facet_grid2(. ~ variable, scales='free_y', independent='y') + 
    geom_point() + geom_line() + xlab('Signal Amplitude') +
    geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
    #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
    theme_bw() + theme(axis.title.y=element_blank()) + 
    geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
    labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_toeplitz_lowd_large_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)

# combine toeplitz small and large
dt.results1 = readRDS('dt.resultsToeplitzLowD_small.RDS')
dt.results1 = dt.results1[method!='OATK_ridge_derand']
dt.results1[, method:=factor(method, 
                            c('BH', 'BC', 'GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso'),
                            c('BH', 'KF', 'GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso'))]
dt.sub1 = melt(dt.results1[, -"rep"], id.vars=c('amp', 'method'))

dt.mean1 = dt.sub1[, .(value=mean(value, na.rm=T), sd=sd(value, na.rm=T)), 
                 keyby=.(method, variable, amp)]
dt.mean1[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.results2 = readRDS('dt.resultsToeplitzLowD_large.RDS')
dt.results2 = dt.results2[method!='OATK_ridge_derand']
dt.results2[, method:=factor(method, 
                             c('BH', 'BC', 'GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso'),
                             c('BH', 'KF', 'GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso'))]
dt.sub2 = melt(dt.results2[, -"rep"], id.vars=c('amp', 'method'))

dt.mean2 = dt.sub2[, .(value=mean(value, na.rm=T), sd=sd(value, na.rm=T)), 
                  keyby=.(method, variable, amp)]
dt.mean2[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.mean = rbindlist(list('Small p and n'=dt.mean1, 'Large p and n'=dt.mean2), idcol='param')
dt.mean[, param:=factor(param, c('Small p and n', 'Large p and n'))]

dt.anchor = unique(dt.mean[, .(param, variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK_ridge']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

dt.mean = dt.mean[amp>3]

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
    facet_grid2(param ~ variable, scales='free_y', independent='y') + 
    geom_point() + geom_line() + xlab('Signal Amplitude') +
    geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
    #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
    theme_bw() + theme(axis.title.y=element_blank()) + 
    geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
    labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_toeplitz_lowd_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=18)

# Toeplitz high d
dt.results = readRDS('dt.resultsToeplitzHighD.RDS')
dt.results = dt.results[method!='OATK_ridge_derand']
dt.results[, method:=factor(method, 
                            c('GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso', 'OATK_ridge_2', 'OATK_lasso_2'),
                            c('GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso', 'OATK_ridge_2', 'OATK_lasso_2'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value, na.rm=T), sd=sd(value, na.rm=T)), 
                 keyby=.(method, variable, amp)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK_ridge']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
    facet_grid2(. ~ variable, scales='free_y', independent='y') + 
    geom_point() + geom_line() + xlab('Signal Amplitude') +
    geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
    #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
    theme_bw() + theme(axis.title.y=element_blank()) + 
    geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
    labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_toeplitz_highd_large_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)

# Power decay high d
dt.results = readRDS('dt.resultsPowerDecayHighD.RDS')
dt.results = dt.results[method!='OATK_ridge_derand' & amp>3]
dt.results[, method:=factor(method, 
                            c('GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso', 'OATK_ridge_2', 'OATK_lasso_2'),
                            c('GM', 'SS', 'BH_lasso', 'OATK_ridge', 'OATK_lasso', 'OATK_ridge_2', 'OATK_lasso_2'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('amp', 'method'))

dt.mean = dt.sub[, .(value=mean(value, na.rm=T), sd=sd(value, na.rm=T)), 
                 keyby=.(method, variable, amp)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK_ridge']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
    facet_grid2(. ~ variable, scales='free_y', independent='y') + 
    geom_point() + geom_line() + xlab('Signal Amplitude') +
    geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
    #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
    theme_bw() + theme(axis.title.y=element_blank()) + 
    geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
    labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/gaussian_power_decay_highd_large_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=12)


# all the high d covaraince benchmarks
# Power decay high d
dt.results = rbindlist(list(toeplitz=readRDS('dt.resultsToeplitzHighD.RDS'), 
                            power_decay=readRDS('dt.resultsPowerDecayHighD.RDS'),
                            const_pos=readRDS('dt.resultsConstPosHighD.RDS'),
                            const_neg=readRDS('dt.resultsConstNegHighD.RDS'),
                            markov_chain=readRDS('dt.resultsMarkovChainHighD.RDS')),
                       idcol='test')
dt.results[, test:=factor(test, c('toeplitz', 'power_decay', 'const_pos', 'const_neg', 'markov_chain'),
                          c('Toeplitz', 'Power Decay', 'Constant Pos.', 'Constant Neg.', 'Markov Chain'))]
dt.results = dt.results[method!='OATK_ridge_derand' & amp>3]
dt.results[, method:=factor(method, 
                            c('GM', 'SS', 'BH_lasso', 'knockoffDMC', 'OATK_ridge', 'OATK_lasso', 'OATK_ridge_2', 'OATK_lasso_2'),
                            c('GM', 'SS', 'BH_lasso', 'knockoffDMC', 'OATK_ridge', 'OATK_lasso', 'OATK_ridge_2', 'OATK_lasso_2'))]
dt.sub = melt(dt.results[, -"rep"], id.vars=c('amp', 'method', 'test'))

dt.mean = dt.sub[, .(value=mean(value, na.rm=T), sd=sd(value, na.rm=T)), 
                 keyby=.(method, variable, amp, test)]
dt.mean[, variable:=factor(variable, c('fdp', 'tdp'), labels=c('FDR', 'Power'))]

dt.anchor = unique(dt.mean[, .(variable, test)])
dt.anchor[variable=='FDR', `:=`(amp=4L, value=0.20)]
dt.anchor[variable=='Power', `:=`(amp=4L, value=1.0)]
dt.anchor[, method:='OATK_lasso']
dt.anchor2 = copy(dt.anchor)
dt.anchor2[, `:=`(amp=4L, value=0)]
dt.anchor = rbind(dt.anchor, dt.anchor2)

plt = ggplot(dt.mean, aes(x=amp, y=value, color=method, shape=method)) + 
    facet_grid2(test ~ variable, scales='free_y', independent='y') + 
    geom_point() + geom_line() + xlab('Signal Amplitude') +
    geom_point(data=dt.anchor, mapping=aes(x=amp, y=value), size=0, alpha=0) +
    #geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=.1, position=position_dodge(.05)) +
    theme_bw() + theme(axis.title.y=element_blank()) + 
    geom_hline(data=data.table(y=0.1, variable='FDR'), mapping=aes(yintercept=y), linetype='dashed') +
    labs(color='Method', shape='Method', linetype='Method')  + theme(legend.position="bottom")
plt
ggsave('/Users/charl/Documents/northwestern/research/oatk_plots/combined_highd_large_line.pdf',
       plt, device=cairo_pdf, dpi=1200, units='cm', width=15, height=24)

