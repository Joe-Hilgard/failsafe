# is failsafe N good for anything?
  # or does stronger pub-bias and stronger QRP make it go up, rather than down?

library(metafor)
library(truncdist)

# Pub bias: 50% of published results are significant ----
# set simulation parameters
k = 30 # number of studies
d.true = 0 # true effect size
bias = .5 # minimum proportion of published effects that are significant

# choose sample sizes
n.cell = rtrunc(k, spec = "pois", a = 12, b = 200, lambda = 30)
df = n.cell*2 - 2
# hist(n.cell, breaks = 10)

# generate observed studies
tcrit = qt(.975, df) # critical value for significance
sig = rbinom(k, size = 1, prob = bias) # coin toss for whether study must be sig.
t = rtrunc(k, "t", a = ifelse(sig == 1, tcrit, -Inf), b = Inf,
           df = df, ncp = sqrt(n.cell/2)*d.true) 
#convert to effect size etc.
d=2*t/sqrt(2*n.cell)
d_v = ( (2*n.cell / (n.cell^2)) + d^2 / (2*df) ) * (2*n.cell / df)
d_se = sqrt(d_v)
p=2*pt(t, df=df, lower.tail = F)

# meta-analysis
meta.5 =  rma(yi = d, vi = d_v)
funnel(meta.5)
# proportion significant
table(p<.05)
# fail-safe N
fsn(yi = d, vi = d_v, type = "Rosenthal")
fsn(yi = d, vi = d_v, type = "Orwin")
fsn(yi = d, vi = d_v, type = "Rosenberg")

# Pub bias: 90% of published results are significant ----
# set simulation parameters
k = 30 # number of studies
d.true = 0 # true effect size
bias = .9 # minimum proportion of published effects that are significant

# choose sample sizes
n.cell = rtrunc(k, spec = "pois", a = 12, b = 200, lambda = 30)
df = n.cell*2 - 2
# hist(n.cell, breaks = 10)

# generate observed studies
tcrit = qt(.975, df) # critical value for significance
sig = rbinom(k, size = 1, prob = bias) # coin toss for whether study must be sig.
t = rtrunc(k, "t", a = ifelse(sig == 1, tcrit, -Inf), b = Inf,
           df = df, ncp = sqrt(n.cell/2)*d.true) 
#convert to effect size etc.
d=2*t/sqrt(2*n.cell)
d_v = ( (2*n.cell / (n.cell^2)) + d^2 / (2*df) ) * (2*n.cell / df)
d_se = sqrt(d_v)
p=2*pt(t, df=df, lower.tail = F)

# meta-analysis
meta.9 =  rma(yi = d, vi = d_v)
funnel(meta.9)
# proportion significant
table(p<.05)
# fail-safe N
fsn(yi = d, vi = d_v, type = "Rosenthal")
fsn(yi = d, vi = d_v, type = "Orwin")
fsn(yi = d, vi = d_v, type = "Rosenberg")
