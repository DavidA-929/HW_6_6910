survival <- read.table("~/Desktop/Stats 6910/HW_4_and_5/survival.txt", header = TRUE)


react.data = read.table("~/Desktop/Stats 6910/HW_4_and_5/reaction.time.txt", header=T)
react.data = head(react.data, 14) # Keep first 14 observations
head(react.data, 3)


# Create trtmt combo vbl TC and factors fTC, fA, and fB within data set
react.data = within(react.data,
                    {TC = 10*A + B; fTC = factor(TC); fA = factor(A); fB = factor(B)})

summary(react.data)
summary(react.data[,c("fA","fB","fTC","y")])

# ANOVA
options(contrasts = c("contr.sum", "contr.poly"))
modelAB = aov(y ~ fA + fB + fA:fB, data = react.data)
anova(modelAB) # Type I ANOVA
drop1(modelAB,  ̃., test = "F") # Type III ANOVA
modelTC = aov(y  ̃ fTC, data = react.data)
anova(modelTC) # Model F-test
# Contrasts: estimates, CIs, tests
library(lsmeans)
# Main-effect-of-B contrast: B1-B2
lsmB = lsmeans(modelAB,  ̃ fB)
summary(contrast(lsmB, list(B12=c( 1,-1, 0))), infer=c(T,T))
# AB-interaction contrast: AB11-AB13-AB21+AB23
lsmAB = lsmeans(modelAB,  ̃ fB:fA) # Using "fB:fA" yields AB lex order
lsmAB # Display to see order of AB combos for contrast coefficients
summary(contrast(lsmAB, list(AB=c( 1 ,0,-1,-1, 0, 1))), infer=c(T,T))
# Multiple comparisons: B
confint(lsmB, level=0.99) # lsmeans for B and 99% CIs
# Tukey’s method
summary(contrast(lsmB, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.99)
# Dunnett’s method
summary(contrast(lsmB, method="trt.vs.ctrl", adj="mvt", ref=1),
        infer=c(T,T), level=0.99)