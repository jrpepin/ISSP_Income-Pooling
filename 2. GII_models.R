library(nnet)
m1 <- multinom(pool ~ marst + relinc, data = data)
m2 <- multinom(pool ~ marst + relinc | country, data = data)

# https://stackoverflow.com/questions/21082396/multinomial-logistic-multilevel-models-in-r
library(brms)

m1 <- brm (pool ~ (1 | index), # this doesn't work
           data=data, family="categorical",
           prior=c(set_prior ("normal (0, 8)")))

m2 <- brm (pool ~ relinc + (1 | country),
           data=data, family="categorical",
           prior=c(set_prior ("normal (0, 8)")))

m3 <- brm (pool ~ marst + (1 | country),
           data=data, family="categorical",
           prior=c(set_prior ("normal (0, 8)")))

library(mlogit)
m <- mlogit(pool ~ relinc + marst | index, data = data)


# rpl <- mlogit(mode ~ price+ catch | income, Fishing, varying = 2:9,
  #            shape = 'wide', rpar = c(price= 'n', catch = 'n'),
   #           correlation = TRUE, halton = NA,
    #          R = 10, tol = 10, print.level = 0)