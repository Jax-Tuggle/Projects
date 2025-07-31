cleanacgr19 <- read.csv(file.choose())
View(cleanacgr19)
reporting_categories <- c("GF", "GM","RA", "RB", "RD","RF","RH","RI","RP","RT","RW")
bp <- boxplot(cleanacgr19$Regular.HS.Diploma.Graduates..Rate.~cleanacgr19$ReportingCategory, main= "2019 Graduation Rates by Demographic", xlab= "Reporting Category", ylab= "Graduation Rates", subset = cleanacgr19$ReportingCategory %in% reporting_categories)
bp$stats
group_data <- split(cleanacgr19$Regular.HS.Diploma.Graduates..Rate., cleanacgr19$ReportingCategory)
standard_deviations <- sapply(group_data, sd)
acgr19StCount<- xtabs(cleanacgr19$CohortStudents~cleanacgr19$ReportingCategory)
acgr19StCount
cleanacgr19$Grad.Students <- round(cleanacgr19$Regular.HS.Diploma.Graduates..Rate./(100)*cleanacgr19$CohortStudents)
G1 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ 1, 
                                     family = "binomial",
                                     data = cleanacgr19)
summary(G1)
gender_categories <- c("GF", "GM")
G2 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory, 
                    family = "binomial",
                    data = cleanacgr19, 
                    subset = cleanacgr19$ReportingCategory %in% gender_categories)
summary(G2)
G3 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ Media.Household.Income, 
                           family = "binomial",
                           data = cleanacgr19)
summary(G3)
G4 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+Media.Household.Income, 
             family = "binomial",
             data = cleanacgr19, 
             subset = cleanacgr19$ReportingCategory %in% gender_categories)
summary(G4)
G5 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ TAYS, 
                          family = "binomial",
                          data = cleanacgr19)
summary(G5)
G6 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+TAYS, 
                          family = "binomial",
                          data = cleanacgr19, 
                          subset = cleanacgr19$ReportingCategory %in% gender_categories)
summary(G6)
G7 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+Media.Household.Income+TAYS, 
          family = "binomial",
          data = cleanacgr19, 
          subset = cleanacgr19$ReportingCategory %in% gender_categories)
G8 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+cleanacgr19$ReportingCategory:Media.Household.Income, 
                    family = "binomial",
                    data = cleanacgr19, 
                    subset = cleanacgr19$ReportingCategory %in% gender_categories)
summary(G8)
G9 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+cleanacgr19$ReportingCategory:TAYS, 
                     family = "binomial",
                     data = cleanacgr19, 
                     subset = cleanacgr19$ReportingCategory %in% gender_categories)
summary(G9)
G10 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+cleanacgr19$Media.Household.Income+cleanacgr19$TAYS+cleanacgr19$ReportingCategory:Media.Household.Income+cleanacgr19$ReportingCategory:TAYS+cleanacgr19$Media.Household.Income+cleanacgr19$TAYS, 
                       family = "binomial",
                       data = cleanacgr19, 
                       subset = cleanacgr19$ReportingCategory %in% gender_categories)
summary(G10)
g <- c(14401,14404,14428,14432,15457,15459,15472,72317,76990,77002)
min_gaic <- min(g)
delta_gaic <- (g -min(g))
gakaike_weights <- exp(-0.5 * delta_gaic) / sum(exp(-0.5 * delta_gaic))
library(sjPlot)
plot_model(G10, title = "Graduation Chances on Gender")
tab_model(G10)

R1 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ 1, 
          family = "binomial",
          data = cleanacgr19)
summary(R1)
race_categories <- c("RA", "RB", "RD","RF","RH","RI","RP","RT","RW")
R2 <- glm(formula = cbind(cleanacgr19$Grad.Students, cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory, 
                    family = "binomial", 
                    data = cleanacgr19, 
                    subset = cleanacgr19$ReportingCategory %in% race_categories)
summary(R2)
R3 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ Media.Household.Income, 
          family = "binomial",
          data = cleanacgr19)
summary(R3)
R4 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+Media.Household.Income, 
                     family = "binomial",
                     data = cleanacgr19, 
                     subset = cleanacgr19$ReportingCategory %in% race_categories)
summary(R4)
R5 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ TAYS, 
                     family = "binomial",
                     data = cleanacgr19)
summary(R5)
R6 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+TAYS, 
                     family = "binomial",
                     data = cleanacgr19, 
                     subset = cleanacgr19$ReportingCategory %in% race_categories)
summary(R6)
R7 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+Media.Household.Income+TAYS, 
          family = "binomial",
          data = cleanacgr19, 
          subset = cleanacgr19$ReportingCategory %in% race_categories)
summary(R7)
R8 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+cleanacgr19$ReportingCategory:Media.Household.Income, 
          family = "binomial",
          data = cleanacgr19, 
          subset = cleanacgr19$ReportingCategory %in% race_categories)
summary(R8)
R9 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+cleanacgr19$ReportingCategory:TAYS, 
          family = "binomial",
          data = cleanacgr19, 
          subset = cleanacgr19$ReportingCategory %in% race_categories)
summary(R9)
R10 <- glm(formula = cbind(cleanacgr19$Grad.Students,cleanacgr19$CohortStudents) ~ cleanacgr19$ReportingCategory+cleanacgr19$Media.Household.Income+cleanacgr19$TAYS+cleanacgr19$ReportingCategory:Media.Household.Income+cleanacgr19$ReportingCategory:TAYS, 
                       family = "binomial",
                       data = cleanacgr19, 
                       subset = cleanacgr19$ReportingCategory %in% race_categories)
summary(R10)
r <- c(19469,19594,19770,19772,20494,20674,20675,72317,76990,77002)
min_raic <- min(r)
delta_raic <- (r -min(r))
rakaike_weights <- exp(-0.5 * delta_raic) / sum(exp(-0.5 * delta_raic))

dp <- plot(density(cleanacgr19$Regular.HS.Diploma.Graduates..Rate.),main= "",xlab="Graduation Rate")
hg <- hist(cleanacgr19$Regular.HS.Diploma.Graduates..Rate.)
density_est <- density(cleanacgr19$Regular.HS.Diploma.Graduates..Rate.)
plot(density_est, main = "", xlab = "Graduation Rate")
mean_density <- sum(density_est$x * density_est$y)


coefficients1_table <- summary(logitgenga19)$coefficients
coefficients1_df <- as.data.frame(coefficients1_table)
View(coefficients1_df)
cleanacgr19$ReportingCategory <- factor(cleanacgr19$ReportingCategory)
library(xtable)
latex_table <- xtable(coefficients1_df)
plot_model(logitracega19, title = "Graduation Chances on Race/Ethnicity")
tab_model(logitracega19)
coefficients2_table <- summary(logitracega19)$coefficients
coefficients2_df <- as.data.frame(coefficients2_table)
View(coefficients2_df)
cleanacgr19$ReportingCategory <- factor(cleanacgr19$ReportingCategory)
latex_table <- xtable(coefficients2_df)
MHIvTAYS <- lm(cleanacgr19$TAYS ~ cleanacgr19$Media.Household.Income)

cumacgr <- read.csv(file.choose())
cumacgr$Grad.Students <- round(cumacgr$Regular.HS.Diploma.Graduates..Rate./(100)*cumacgr$CohortStudents) 
C1 <- glm(formula = cbind(cumacgr$Grad.Students,cumacgr$CohortStudents) ~ 1, 
                       family = "binomial",
                      data = cumacgr)
summary(C1)
cumacgr$COVIDEra <- factor(cumacgr$COVIDEra)
C2 <- glm(formula = cbind(cumacgr$Grad.Students,cumacgr$CohortStudents) ~ cumacgr$COVIDEra, 
                     family = "binomial",
                     data = cumacgr)
summary(C2)
C3 <- glm(formula = cbind(cumacgr$Grad.Students,cumacgr$CohortStudents) ~ ifelse(cumacgr$COVIDEra == "Pre",1,0), data=cumacgr, family="binomial")
summary(C3)
C4 <- glm(formula = cbind(cumacgr$Grad.Students,cumacgr$CohortStudents) ~ ifelse(cumacgr$COVIDEra == "Peak",1,0), data=cumacgr, family="binomial")
summary(C4)
C5 <- glm(formula = cbind(cumacgr$Grad.Students,cumacgr$CohortStudents) ~ ifelse(cumacgr$COVIDEra == "Post",1,0), data=cumacgr, family="binomial")
summary(C5)
c <- c(435590,435603,436358,436593,436784)
min_caic <- min(c)
delta_caic <- (c -min(c))
cakaike_weights <- exp(-0.5 * delta_caic) / sum(exp(-0.5 * delta_caic))

C6 <- glm(formula = cbind(cumacgr$Grad.Students,cumacgr$CohortStudents) ~ factor(cumacgr$AcademicYear), 
                     family = "binomial",
                     data = cumacgr)
 summary(C6)
