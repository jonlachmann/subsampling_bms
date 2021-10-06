library(irls.sgd)

# Set the number of variables
var_count <- 16

# Calculate the true model using glm.fit (i.e. Full IRLS)
irls_res <- glm.fit(mill_x_g[,1:var_count], mill_y_l1M, family=binomial())

# Calculate the model using BSGD
sgd_res <- glm.sgd(mill_x_g[,1:var_count], mill_y_l1M, binomial(),
            sgd.ctrl=list(subs=0.0005, maxit=35000, alpha=0.15, decay=1-(1e-6), histfreq=10))

# Calculate the model using S-IRLS-SGD
sgdirls_res <- irls.sgd(mill_x_g[,1:var_count], mill_y_l1M, binomial(),
            irls.control=list(subs=0.0005, maxit=75, tol=1e-7, cooling = c(3,0.9,0.95), expl = c(3,1.5,1)),
            sgd.control=list(subs=0.0005, maxit=0, alpha=0.05, decay=0.99, histfreq=10), T)


get_deviance(sgd_res$coefficients, mill_x_g[,1:var_count], mill_y_l1M, binomial())
get_deviance(sgdirls_res$coefficients, mill_x_g[,1:var_count], mill_y_l1M, binomial())
get_deviance(irls_res$coefficients, mill_x_g[,1:var_count], mill_y_l1M, binomial())

# Create a plot showing the convergence of S-IRLS-SGD
par(mfrow=c(4,4),oma = c(0,0,2,0), mar=c(4,5.1,2,2.1))
for (i in 1:var_count) {
  multiplot(cbind(sgd_res$xhist[,i], irls_res$coefficients[i]),
            frame.plot=F, cex.lab=1.3,
            ylab=bquote(beta[.(i-1)]), xlab="Iteration")
}
mtext("Convergence of parameter estimates in S-IRLS-SGD for a logistic model", outer = TRUE, cex = 1.5)
