# is failsafe N good for anything?
  # or does stronger pub-bias and stronger QRP make it go up, rather than down?

library(metafor)
library(truncdist)

# Pub bias: 5% of nonsig results are published ----
## You can see that publication bias alone is not likely to account for distorted literature
## "Nobody has 19 studies in a file-drawer for every positive study."
k = 100
d.true = 0
null.pub.prob = .05

# choose sample sizes
n.cell = rtrunc(k, spec = "pois", a = 12, b = 150, lambda = 30)
df = n.cell*2 - 2

# generate all studies
t = rt(k, df, ncp = sqrt(n.cell/2)*d.true) 
#convert to effect size etc.
d=2*t/sqrt(2*n.cell)
d_v = ( (2*n.cell / (n.cell^2)) + d^2 / (2*df) ) * (2*n.cell / df)
d_se = sqrt(d_v)
p=2*pt(t, df=df, lower.tail = F)

# apply statistical significance threshold
pub = rbinom(k, size = 1, prob = null.pub.prob)
pub[p < .05] = 1

# meta-analysis
meta.2 =  rma(yi = d[pub == 1], vi = d_v[pub == 1])
meta.2
funnel(meta.2)
# proportion significant
table(p[pub == 1] < .05)
# fail-safe N
fsn(yi = d[pub == 1], vi = d_v[pub == 1], type = "Rosenthal")
fsn(yi = d[pub == 1], vi = d_v[pub == 1], type = "Orwin")
fsn(yi = d[pub == 1], vi = d_v[pub == 1], type = "Rosenberg")


# QRP: Collect three measures, report one ----
k = 100
d.true = 0
null.pub.prob = .05

# choose sample sizes
n.cell = rtrunc(k, spec = "pois", a = 12, b = 150, lambda = 30)
df = n.cell*2 - 2

# prepare holster
result = data.frame("d" = NULL, "d_v" = NULL, "d_se" = NULL, "p" = NULL)

for (i in 1:k) {
  n.iter = n.cell[i]
  df.iter = df[i]
  dat = data.frame("x" = rep(0:1, each=n.iter),
                   "y1" = rnorm(n.iter*2) + d.true*rep(0:1, each=n.iter),
                   "y2" = rnorm(n.iter*2) + d.true*rep(0:1, each=n.iter),
                   "y3" = rnorm(n.iter*2) + d.true*rep(0:1, each=n.iter)
                   )
  mod1 = lm(y1 ~ x, dat)
  mod2 = lm(y2 ~ x, dat)
  mod3 = lm(y3 ~ x, dat)
  p1 = summary(mod1)$coefficients[2,4] 
  p2 = summary(mod2)$coefficients[2,4]
  p3 = summary(mod3)$coefficients[2,4]
  t1 = summary(mod1)$coefficients[2,3] 
  t2 = summary(mod2)$coefficients[2,3]
  t3 = summary(mod3)$coefficients[2,3]
  if (p1 < .05) {
    t = t1
    p = p1
    test = 1
  } else if (p2 < .05) {
    t = t2
    p = p2
    test = 2
  } else if (p3 < .05) {
    t = t3
    p = p3
    test = 3
  } else {
    t = t1
    p = p1
    test = 1
  }
    out = data.frame("d" = 2*t/sqrt(2*n.iter), "test" = test)
    out$d_v = ( (2*n.iter / (n.iter^2)) + out$d^2 / (2*df.iter) ) * (2*n.iter / df.iter)
    out$d_se = sqrt(out$d_v)
    out$p = p
# append
    result = rbind(result, out)
}

# apply statistical significance threshold
result$pub = rbinom(k, size = 1, prob = null.pub.prob)
result$pub[result$p < .05 & result$d > 0] = 1

# meta-analysis
meta.3dv =  rma(yi = d, vi = d_v, data = result, subset = pub == 1)
meta.3dv
funnel(meta.3dv)
# proportion significant
table(result$p < .05)
table(result$pub, result$p < .05)
# fail-safe N
fsn(yi = d, vi = d_v, data = result, subset = pub == 1, type = "Rosenthal")
fsn(yi = d, vi = d_v, data = result, subset = pub == 1, type = "Orwin")
fsn(yi = d, vi = d_v, data = result, subset = pub == 1, type = "Rosenberg")

# QRP: Collect three measures, report one; p-peek every 10 subjects ----
k = 100
d.true = 0
null.pub.prob = .05

# choose sample sizes
n.cell = rtrunc(k, spec = "pois", a = 12, b = 150, lambda = 30)
df = n.cell*2 - 2

# prepare holster
result = data.frame("d" = NULL, "d_v" = NULL, "d_se" = NULL, "p" = NULL)

for (i in 1:k) {
  sig = F
  n.iter = n.cell[i]
  df.iter = df[i]
  dat = data.frame("x" = rep(0:1, each=n.iter),
                   "y1" = rnorm(n.iter*2) + d.true*rep(0:1, each=n.iter),
                   "y2" = rnorm(n.iter*2) + d.true*rep(0:1, each=n.iter),
                   "y3" = rnorm(n.iter*2) + d.true*rep(0:1, each=n.iter)
  )
  while (sig == F) {
    mod1 = lm(y1 ~ x, dat)
    mod2 = lm(y2 ~ x, dat)
    mod3 = lm(y3 ~ x, dat)
    p1 = summary(mod1)$coefficients[2,4] 
    p2 = summary(mod2)$coefficients[2,4]
    p3 = summary(mod3)$coefficients[2,4]
    t1 = summary(mod1)$coefficients[2,3] 
    t2 = summary(mod2)$coefficients[2,3]
    t3 = summary(mod3)$coefficients[2,3]
    if (p1 < .05) {
      t = t1
      p = p1
      test = 1
    } else if (p2 < .05) {
      t = t2
      p = p2
      test = 2
    } else if (p3 < .05) {
      t = t3
      p = p3
      test = 3
    } else if (n.iter < 150) {
      n.iter = n.iter + 5
      df.iter = df.iter + 5
      dat = rbind(dat, 
                  data.frame("x" = rep(0:1, each=5),
                             "y1" = rnorm(5*2) + d.true*rep(0:1, each=5),
                             "y2" = rnorm(5*2) + d.true*rep(0:1, each=5),
                             "y3" = rnorm(5*2) + d.true*rep(0:1, each=5))
      )
    } else {
      t = t1
      p = p1
      test = 1
    }
  }
  out = data.frame("d" = 2*t/sqrt(2*n.iter), "test" = test, "n" = n.iter)
  out$d_v = ( (2*n.iter / (n.iter^2)) + out$d^2 / (2*df.iter) ) * (2*n.iter / df.iter)
  out$d_se = sqrt(out$d_v)
  out$p = p
  # append
  result = rbind(result, out)
}

# apply statistical significance threshold
result$pub = rbinom(k, size = 1, prob = null.pub.prob)
result$pub[result$p < .05 & result$d > 0] = 1

# meta-analysis
meta.3dv =  rma(yi = d, vi = d_v, data = result, subset = pub == 1)
meta.3dv
funnel(meta.3dv)
# proportion significant
table(result$p < .05)
table(result$pub, result$p < .05)
# fail-safe N
fsn(yi = d, vi = d_v, data = result, subset = pub == 1, type = "Rosenthal")
fsn(yi = d, vi = d_v, data = result, subset = pub == 1, type = "Orwin")
fsn(yi = d, vi = d_v, data = result, subset = pub == 1, type = "Rosenberg")


# Pub bias: 50% of published results are significant ----
# set simulation parameters
k = 15 # number of studies
d.true = 0 # true effect size
bias = .5 # minimum proportion of published effects that are significant

# choose sample sizes
n.cell = rtrunc(k, spec = "pois", a = 12, b = 150, lambda = 30)
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
n.cell = rtrunc(k, spec = "pois", a = 12, b = 150, lambda = 30)
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
