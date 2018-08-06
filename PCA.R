# PCA
library(MASS)
data(Boston)
?Boston
head(Boston)
str(Boston)

apply(Boston, 2, mean)
apply(Boston, 2, var)

pr.out <- prcomp(Boston, scale = TRUE)
pr.out$center
pr.out$rotation
biplot(pr.out, scale = 0)

#variance explained by each PC
pr.var <- pr.out$sdev^2
#proportion of variance explained by each PC
pve <- pr.var/sum(pr.var)
pve

#plot PVE
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     ylim = c(0,1), type = 'b')
#plot cumulative PVE
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1),
     type = 'b')
