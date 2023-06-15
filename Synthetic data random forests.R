library(randomForest)

ACS_origdata = read.table(file="ACS_origdata.txt",sep=",")
# change all data to categorical
for (i in 1:14){
  ACS_origdata[,i] <- as.factor(ACS_origdata[,i])
}

# Random Forest Parameters
num.trees <- 500
node.size <- 1

m <- 5 # number of datasets to generate
syn.data <- list()

n = length(ACS_origdata[,1]) # number of observations
X1.prop = c(sum(ACS_origdata[,1]==1), sum(ACS_origdata[,1]==2))

set.seed(10)
for (t in 1:m){
  new.ACS = ACS_origdata
  # Synthesizing first variable
  X1.rep = rep(NA, n)
  for (i in 1:n){
    draw = rmultinom(1,1,X1.prop)
    for (k in 1:2){
      if (draw[k] == 1){
        X1.rep[i] = k
      }
    }
  }
  X1.rep = as.factor(X1.rep)
  
  # Replace X1 with synthetic data
  new.ACS[1] = X1.rep
  
  # Synthesize the rest of the variables
  for (p in 2:14){
    rf.ACS = randomForest(ACS_origdata[,p]~., data=ACS_origdata[,c(1:p)], 
                          ntree=num.trees, nodesize=node.size)
    rf.ACS
    rf.pred =predict(rf.ACS, new.ACS, type ="vote")
    
    X.rep = rep(NA, n)
    levels = length(rf.pred[1,])
    for (i in 1:n){
      draw = rmultinom(1,1,rf.pred[i,])
      for (k in 1:levels){
        if (draw[k] == 1){
          X.rep[i] = k
        }
      }
    }
    X.rep = as.factor(X.rep)
    
    new.ACS[p] = X.rep
  }
  syn.data[[t]] = new.ACS
}


# average the joint counts
joint_tables = list()
for (t in 1:m){
  joint_tables[[t]] = table(syn.data[[t]][,9], syn.data[[t]][,4])/n
}
sum.joint_table = joint_tables[[1]]
for (t in 2:m){
  sum.joint_table = sum.joint_table + joint_tables[[t]]
}
mean.joint_table = sum.joint_table/m

# get b_m
sum.sq.res = (joint_tables[[1]] - mean.joint_table)^2
for (t in 2:m){
  sum.sq.res = sum.sq.res + (joint_tables[[t]] - mean.joint_table)^2
}
b_m = sum.sq.res/(m-1)

# get ubar_m
sum.u = joint_tables[[1]]*(1-joint_tables[[1]])/n
for (t in 2:m){
  sum.u = sum.u + joint_tables[[t]]*(1-joint_tables[[t]])/n
}
ubar_m = sum.u/m

# get T_f, an estimate of the variance of qbar_m
T_f = (1+1/m)*b_m - ubar_m

tabsize = length(T_f)
varian = T_f
for (x in 1:tabsize){
  if (T_f[x] < 0){
    varian[x] = ubar_m[x]
  } 
}

# Obtaining a Univariate Summary
syn.univ = c()
for (p in 1:14){ # for each variable
  tab = table(syn.data[[1]][p])
  facs = length(tab)
  for (i in 1:facs){ # for each level
    values = c()
    for (k in 1:5){ # for each synthetic dataset
      values = append(values, table(syn.data[[k]][p])[i])
    }
    syn.univ = append(syn.univ, mean(values))
  }
}

orig.univ = c()
for (p in 1:14){ # for each variable
  tab = table(ACS_origdata[,p])
  facs = length(tab)
  for (i in 1:facs){ # for each level
    orig.univ = append(orig.univ, table(ACS_origdata[,p])[i])
  }
}

# Plotting 
plot(syn.univ, orig.univ, xlab="Synthetic Univariate Count",
     ylab = "Original Univariate Count", 
     main="Univariate Distribution Comparison (Forward synthesis order)")
abline(0,1,lty=2)

# Obtaining a Bivariate Summary
syn.biv = c()
for (p in 1:13){ # for each variable
  for (q in (p+1):14){ # for each other variable
    if (p != q){
      tab = table(syn.data[[1]][,p], syn.data[[1]][,q])
      facs = length(tab)
      for (i in 1:facs){ # for each level
        values = c()
        for (k in 1:5){ # for each synthetic dataset
          tab = table(syn.data[[k]][,p], syn.data[[k]][,q])
          values = append(values, tab[i])
        }
        syn.biv = append(syn.biv, mean(values))
      }
    }
  }
}

orig.biv = c()
for (p in 1:13){ # for each variable
  for (q in (p+1):14){ # for each other variable
    if (p != q){
      tab = table(ACS_origdata[,p], ACS_origdata[,q])
      facs = length(tab)
      for (i in 1:facs){ # for each level
        orig.biv = append(orig.biv, tab[i])
      }
    }
  }
}

# Plotting 
plot(syn.biv, orig.biv, xlab="Synthetic Bivariate Count",
     ylab = "Original Bivariate Count", 
     main="Bivariate Distribution Comparison (Forward synthesis order)")
abline(0,1,lty=2)

# Picking out the variable pairs with AGEP or RACE1P (2nd & 3rd variables)
syn.biv2 = c()
for (p in 2:3){ # for each variable
  for (q in (p+1):14){ # for each other variable
    if (p != q){
      tab = table(syn.data[[1]][,p], syn.data[[1]][,q])
      facs = length(tab)
      for (i in 1:facs){ # for each level
        values = c()
        for (k in 1:5){ # for each synthetic dataset
          tab = table(syn.data[[k]][,p], syn.data[[k]][,q])
          values = append(values, tab[i])
        }
        syn.biv2 = append(syn.biv2, mean(values))
      }
    }
  }
}

orig.biv2 = c()
for (p in 2:3){ # for each variable
  for (q in (p+1):14){ # for each other variable
    if (p != q){
      tab = table(ACS_origdata[,p], ACS_origdata[,q])
      facs = length(tab)
      for (i in 1:facs){ # for each level
        orig.biv2 = append(orig.biv2, tab[i])
      }
    }
  }
}

# Plotting 
lines(syn.biv2, orig.biv2, xlab="Synthetic Bivariate Count",
     ylab = "Original Bivariate Count", 
     main="Bivariate Distribution Comparison (Forward synthesis order)", 
     col="red", type="p")
abline(0,1,lty=2)

save(syn.data, file = "synthetic-forward.Rdata")

########################## Reverse order
set.seed(50)

for (t in 1:m){
  new.ACS = ACS_origdata
  # Synthesizing last variable
  X14.rep = rep(NA, n)
  X14.prop = table(ACS_origdata[,14])
  for (i in 1:n){
    draw = rmultinom(1,1,X14.prop)
    for (k in 1:2){
      if (draw[k] == 1){
        X14.rep[i] = k
      }
    }
  }
  X14.rep = as.factor(X14.rep)
  
  # Replace X14 with synthetic data
  new.ACS[14] = X14.rep
  
  # Synthesize the rest of the variables
  for (p in 13:1){
    rf.ACS = randomForest(ACS_origdata[,p]~., data=ACS_origdata[,c(p:14)], 
                          ntree=num.trees, nodesize=node.size)
    rf.ACS
    rf.pred =predict(rf.ACS, new.ACS, type ="vote")
    
    X.rep = rep(NA, n)
    levels = length(rf.pred[1,])
    for (i in 1:n){
      draw = rmultinom(1,1,rf.pred[i,])
      for (k in 1:levels){
        if (draw[k] == 1){
          X.rep[i] = k
        }
      }
    }
    X.rep = as.factor(X.rep)
    
    new.ACS[p] = X.rep
  }
  syn.data[[t]] = new.ACS
}


# average the joint counts
joint_tables = list()
for (t in 1:m){
  joint_tables[[t]] = table(syn.data[[t]][,9], syn.data[[t]][,4])/n
}
sum.joint_table = joint_tables[[1]]
for (t in 2:m){
  sum.joint_table = sum.joint_table + joint_tables[[t]]
}
mean.joint_table = sum.joint_table/m

# get b_m
sum.sq.res = (joint_tables[[1]] - mean.joint_table)^2
for (t in 2:m){
  sum.sq.res = sum.sq.res + (joint_tables[[t]] - mean.joint_table)^2
}
b_m = sum.sq.res/(m-1)

# get ubar_m
sum.u = joint_tables[[1]]*(1-joint_tables[[1]])/n
for (t in 2:m){
  sum.u = sum.u + joint_tables[[t]]*(1-joint_tables[[t]])/n
}
ubar_m = sum.u/m

# get T_f, an estimate of the variance of qbar_m
T_f = (1+1/m)*b_m - ubar_m

tabsize = length(T_f)
varian = T_f
for (x in 1:tabsize){
  if (T_f[x] < 0){
    varian[x] = ubar_m[x]
  } 
}

# Obtaining a Univariate Summary
syn.univ = c()
for (p in 1:14){ # for each variable
  tab = table(syn.data[[1]][p])
  facs = length(tab)
  for (i in 1:facs){ # for each level
    values = c()
    for (k in 1:5){ # for each synthetic dataset
      values = append(values, table(syn.data[[k]][p])[i])
    }
    syn.univ = append(syn.univ, mean(values))
  }
}

orig.univ = c()
for (p in 1:14){ # for each variable
  tab = table(ACS_origdata[,p])
  facs = length(tab)
  for (i in 1:facs){ # for each level
    orig.univ = append(orig.univ, table(ACS_origdata[,p])[i])
  }
}

# Plotting 
plot(syn.univ, orig.univ, 
     xlab="Synthetic Univariate Count",
     ylab = "Original Univariate Count", 
     main="Univariate Distribution Comparison (Reverse synthesis order)")
abline(0,1,lty=2)

# Obtaining a Bivariate Summary
syn.biv = c()
for (p in 1:13){ # for each variable
  for (q in (p+1):14){ # for each other variable
    if (p != q){
      tab = table(syn.data[[1]][,p], syn.data[[1]][,q])
      facs = length(tab)
      for (i in 1:facs){ # for each level
        values = c()
        for (k in 1:5){ # for each synthetic dataset
          tab = table(syn.data[[k]][,p], syn.data[[k]][,q])
          values = append(values, tab[i])
        }
        syn.biv = append(syn.biv, mean(values))
      }
    }
  }
}

orig.biv = c()
for (p in 1:13){ # for each variable
  for (q in (p+1):14){ # for each other variable
    if (p != q){
      tab = table(ACS_origdata[,p], ACS_origdata[,q])
      facs = length(tab)
      for (i in 1:facs){ # for each level
        orig.biv = append(orig.biv, tab[i])
      }
    }
  }
}

# Plotting 
plot(syn.biv, orig.biv, 
     xlab="Synthetic Bivariate Count",
     ylab = "Original Bivariate Count", 
     main="Bivariate Distribution Comparison (Reverse synthesis order)")
abline(0,1,lty=2)

save(syn.data, file = "synthetic-reverse.Rdata")