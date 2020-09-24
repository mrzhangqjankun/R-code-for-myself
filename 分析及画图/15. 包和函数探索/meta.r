
library(meta)
?meta

##二变量
# Calculate odds ratio and confidence interval for a single study
#
metabin(10, 20, 15, 20, sm = "OR")

# Different results (due to handling of studies with double zeros)
#
metabin(0, 10, 0, 10, sm = "OR")
metabin(0, 10, 0, 10, sm = "OR", allstudies = TRUE)

# Use subset of Olkin (1995) to conduct meta-analysis based on
# inverse variance method (with risk ratio as summary measure)
#
data(Olkin95)
m1 <- metabin(event.e, n.e, event.c, n.c,
              data = Olkin95, subset = c(41, 47, 51, 59),
              method = "Inverse")
summary(m1)

# Use different subset of Olkin (1995)
#
m2 <- metabin(event.e, n.e, event.c, n.c,
              data = Olkin95, subset = year < 1970,
              method = "Inverse", studlab = author)
summary(m2)
forest(m2)
?forest

# Meta-analysis with odds ratio as summary measure
#
m3 <- metabin(event.e, n.e, event.c, n.c,
              data = Olkin95, subset = year < 1970,
              sm = "OR", method = "Inverse", studlab = author)
# Same meta-analysis result using 'update.meta' function
m3 <- update(m2, sm = "OR")
summary(m3)

# Meta-analysis based on Mantel-Haenszel method (with odds ratio as
# summary measure)
#
m4 <- update(m3, method = "MH")
summary(m4)

# Meta-analysis based on Peto method (only available for odds ratio
# as summary measure)
#
m5 <- update(m3, method = "Peto")
summary(m5)

## Not run: 
# Meta-analysis using generalised linear mixed models (only if R
# packages 'metafor' and 'lme4' are available)
#
if (suppressMessages(require(metafor, quietly = TRUE, warn = FALSE)) &
    require(lme4, quietly = TRUE)) {
  
  # Logistic regression model with (k = 4) fixed study effects
  # (default: model.glmm = "UM.FS")
  #
  m6 <- metabin(event.e, n.e, event.c, n.c,
                data = Olkin95, subset = year < 1970,
                method = "GLMM")
  # Same results:
  m6 <- update(m2, method = "GLMM")
  summary(m6)
  
  # Mixed-effects logistic regression model with random study effects
  # (warning message printed due to argument 'nAGQ')
  #
  m7 <- update(m6, model.glmm = "UM.RS")
  #
  # Use additional argument 'nAGQ' for internal call of 'rma.glmm'
  # function
  #
  m7 <- update(m6, model.glmm = "UM.RS", nAGQ = 1)
  summary(m7)
  
  # Generalised linear mixed model (conditional
  # Hypergeometric-Normal) (R package 'BiasedUrn' must be available)
  #
  if (require(BiasedUrn, quietly = TRUE)) {
    m8 <- update(m6, model.glmm = "CM.EL")
    summary(m8)
  }
  
  # Generalised linear mixed model (conditional Binomial-Normal)
  #
  m9 <- update(m6, model.glmm = "CM.AL")
  summary(m9)
  
  # Logistic regression model with (k = 70) fixed study effects
  # (about 18 seconds with Intel Core i7-3667U, 2.0GHz)
  #
  m10 <- metabin(event.e, n.e, event.c, n.c,
                 data = Olkin95, method = "GLMM")
  summary(m10)
  
  # Mixed-effects logistic regression model with random study effects
  # - about 50 seconds with Intel Core i7-3667U, 2.0GHz
  # - several warning messages, e.g. "failure to converge, ..."
  #
  summary(update(m10, model.glmm = "UM.RS"))
  
  # Conditional Hypergeometric-Normal GLMM
  # - long computation time (about 12 minutes with Intel Core
  #   i7-3667U, 2.0GHz)
  # - estimation problems for this very large dataset:
  #   * warning that Choleski factorization of Hessian failed
  #   * confidence interval for treatment effect smaller in random
  #     effects model compared to fixed effect model
  #
  if (require(BiasedUrn, quietly = TRUE)) {
    system.time(m11 <- update(m10, model.glmm = "CM.EL"))
    summary(m11)
  }
  
  # Generalised linear mixed model (conditional Binomial-Normal)
  # (less than 1 second with Intel Core i7-3667U, 2.0GHz)
  #
  summary(update(m10, model.glmm = "CM.AL"))
}

## End(Not run)



###连续变量
data(Fleiss93cont)

# Meta-analysis with Hedges' g as effect measure
#
m1 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c,
               data = Fleiss93cont, sm = "SMD")
m1
forest(m1)

# Use Cohen's d instead of Hedges' g as effect measure
#
update(m1, method.smd = "Cohen")

# Use Glass' delta instead of Hedges' g as effect measure
#
update(m1, method.smd = "Glass")

# Use Glass' delta based on the standard deviation in the experimental group
#
update(m1, method.smd = "Glass", sd.glass = "experimental")

# Calculate Hedges' g based on exact formulae
#
update(m1, exact.smd = TRUE)

data(amlodipine)
m2 <- metacont(n.amlo, mean.amlo, sqrt(var.amlo),
               n.plac, mean.plac, sqrt(var.plac),
               data = amlodipine, studlab = study)
summary(m2)
forest(m2)
# Use pooled variance
#
summary(update(m2, pooledvar = TRUE))

# Meta-analysis of response ratios (Hedges et al., 1999)
#
data(woodyplants)
m3 <- metacont(n.elev, mean.elev, sd.elev,
               n.amb, mean.amb, sd.amb,
               data = woodyplants, sm = "ROM")
summary(m3)
summary(m3, backtransf = FALSE)
forest(m3)

##偏倚检验。漏斗图越对称越好
m3
funnel(m3)
metabias(m3)

##敏感性分析：排除异常结果后重新进行meta分析
data(Fleiss93)
m1 <- metabin(event.e, n.e, event.c, n.c,
              data = Fleiss93, studlab = study,
              sm = "RR", method = "I")
m1
metainf(m1)
metainf(m1, pooled = "random")

forest(metainf(m1))
forest(metainf(m1), layout = "revman5")
forest(metainf(m1, pooled = "random"))

metainf(m1, sortvar = study)
metainf(m1, sortvar = 7:1)

m2 <- update(m1, title = "Fleiss93 meta-analysis",
             backtransf = FALSE)
metainf(m2)

data(Fleiss93cont)
m3 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c,
               data = Fleiss93cont, sm = "SMD")
metainf(m3)

##剪补法。去掉或增加样本后的效果
data(Fleiss93)
m1 <- metabin(event.e, n.e, event.c, n.c, data = Fleiss93, sm = "OR")
tf1 <- trimfill(m1)
summary(tf1)
funnel(tf1)
funnel(tf1, pch = ifelse(tf1$trimfill, 1, 16),
       level = 0.9, comb.random = FALSE)
#
# Use log odds ratios on x-axis
#
funnel(tf1, backtransf = FALSE)
funnel(tf1, pch = ifelse(tf1$trimfill, 1, 16),
       level = 0.9, comb.random = FALSE, backtransf = FALSE)

trimfill(m1$TE, m1$seTE, sm = m1$sm)


##累积meta。时间序列
data(Fleiss93)
m1 <- metabin(event.e, n.e, event.c, n.c,
              data = Fleiss93, studlab = study,
              sm = "RR", method = "I")
m1
metacum(m1)
metacum(m1, pooled = "random")

forest(metacum(m1))
forest(metacum(m1, pooled = "random"))

metacum(m1, sortvar = study)
metacum(m1, sortvar = 7:1)

m2 <- update(m1, title = "Fleiss93 meta-analysis",
             backtransf = FALSE)
metacum(m2)

data(Fleiss93cont)
m3 <- metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c,
               data = Fleiss93cont, sm = "SMD")
metacum(m3)

##无对照的meta
metaprop