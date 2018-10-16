# R code and output
mung.data = read.table("~/Desktop/Stats 6910/HW_4_and_5/mung.bean.txt", header=TRUE)
model1 = aov(Length ~ factor(Trtmt), data=mung.data)
# Compute predicted values, residuals, standardized residuals, normal scores
mung.data = within(mung.data, {
  # Compute predicted, residual, and standardized residual values
  ypred = fitted(model1); e = resid(model1); z = e/sd(e);
  # Compute Blom’s normal scores
  n = length(e); q = rank(e); nscore = qnorm((q-0.375)/(n+0.25)) })
# Display first 3 lines of mung.data, 4 digits per variable
print(head(mung.data, 3), digits=4)


# Generate residual plots
plot(z ~ Trtmt, data=mung.data, ylab="Standardized Residuals", las=1)
abline(h=0)  # Horizontal line at zero
plot(z ~ Order, data=mung.data, ylab="Standardized Residuals", las=1)
abline(h=0)
plot(z ~ ypred, data=mung.data, ylab="Standardized Residuals", las=1)
abline(h=0)
plot(z ~ nscore, data=mung.data, ylab="Standardized Residuals", las=1)
qqline(mung.data$z)  # Line through 1st and 3rd quantile points
# A simpler way to generate the normal probability plot
qqnorm(mung.data$z); qqline(mung.data$z)





# Compute sample means and variances and their natural logs by trtmt
MeanLnth = by(spaghetti.data$, spaghetti.data$trtmt, mean) # Sample means
VarLnth = by(mung.data$Length, mung.data$Trtmt, var) # Sample variances
LnMean = log(MeanLnth)  # Column of ln sample means
LnVar = log(VarLnth)  # Column of ln sample variances
Trtmt = c(1:6)  # Column of trtmt levels
stats = cbind(Trtmt, MeanLnth, VarLnth, LnMean, LnVar) # Column bind
stats  # Display the stats data







spaghetti.sauce.data <- as.data.frame(spaghetti.sauce.data)
spaghetti.sauce.data$trtmt <- as.factor(spaghetti.sauce.data$trtmt)


spaghetti.model <- aov(weight ~ trtmt , data = spaghetti.sauce.data)
anova(spaghetti.model)

# Get fitted values from the model
spaghetti.fitted <- fitted(spaghetti.model); spaghetti.fitted

# Raw Residuals
spaghetti.raw.residuals <- resid(spaghetti.model); spaghetti.raw.residuals

# Standardize residuals 
spaghetti.stand.residuals <- rstandard(spaghetti.model); spaghetti.stand.residuals 


par(mfrow = c(2,2))
#Raw Residuals vs. Frying/ Grilling fat content 
plot(as.numeric(spaghetti.sauce.data$trtmt) , spaghetti.raw.residuals, 
     xlab = "Codes", ylab = "Raw Residuals", xaxt = "n",
     lwd = 1.5)
axis(1, at = 1:6, labels = c("1","2","3","4","5","6"))
abline(h=0, col = "red", lty = 2)

#Raw Residuals vs. Fitted Values
plot(spaghetti.fitted , spaghetti.raw.residuals, 
     xlab = "Fitted Values", ylab = "Raw Residuals",
     lwd = 1.5)
abline(h=0, col = "red", lty = 2)

#Standardized Residuals vs. Fitted Values
plot(spaghetti.fitted , spaghetti.stand.residuals, 
     xlab = "Fitted Values", ylab = "Standardize Residuals",
     lwd = 1.5)
abline(h=0, col = "red", lty = 2)

#Norma Probability Plot
qqnorm(spaghetti.raw.residuals, main= "Normal Q-Q Plot of Raw Residuals")
qqline(weights.raw.resid, col = "red")



CI_tau1_minus_tau2 <- CI_Satterthwaite(Y_i_hat[1],Y_i_hat[2],
                                       Y_i_var[1],Y_i_var[2],3,6)
CI_tau1_minus_tau2

CI_tau3_minus_tau4 <- CI_Satterthwaite(Y_i_hat[3],Y_i_hat[4],
                                       Y_i_var[3],Y_i_var[4],3,6)
CI_tau3_minus_tau4

CI_tau5_minus_tau6 <- CI_Satterthwaite(Y_i_hat[5],Y_i_hat[6],
                                       Y_i_var[5],Y_i_var[6],3,6)
CI_tau5_minus_tau6

CI_tau1_minus_tau5 <- CI_Satterthwaite(Y_i_hat[1],Y_i_hat[5],
                                       Y_i_var[1],Y_i_var[5],3,6)
CI_tau1_minus_tau5

CI_tau1_minus_tau3 <- CI_Satterthwaite(Y_i_hat[1],Y_i_hat[3],
                                       Y_i_var[1],Y_i_var[3],3,6)
CI_tau1_minus_tau3
CI_tau3_minus_tau5 <- CI_Satterthwaite(Y_i_hat[3],Y_i_hat[5],
                                       Y_i_var[3],Y_i_var[5],3,6)
CI_tau3_minus_tau5




n_1 <- nrow(Post_grams); n_1
v_1 <- length(unique(Post_grams$Code)); v_1
rs_1 <- tapply(rep(1,n_1) , Post_grams$Code, sum); rs_1
r_i.vector_1 <- rep(rs_1,time =rs_1); r_i.vector_1



anova(Post_weight_model)[2,"Mean Sq"]


#MSE <- anova(Post_weight_model)[2,"Mean Sq"]; MSE
#std.residuals <- weights.raw.resid/(sqrt(MSE * (1-1/r_i.vector_1))); std.residuals
