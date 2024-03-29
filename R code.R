####### GROUP WORK ASSIGNMENT #######
# --- IACCARINO FRANCESCO 3170051 ---
# --- D'ERCOLE COSTANZA   3159923 ---
# --- ROMANO DAVIDE       3164081 ---


rm(list = ls())

# Install required libraries
if (!require("readxl")) install.packages("readxl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
if (!require("car")) install.packages("car")
if (!require("ggfortify")) install.packages("ggfortify")
if (!require("tseries")) install.packages("tseries")

# Load required libraries
library(readxl)
library(ggplot2)
library(plotly)
library(car)
library(ggfortify)
library(tseries)


# Read in data from Excel file
path <- file.choose()
data <- read_excel(path)

# View the first few rows of the data
head(data)

# Extract relevant variables from the data
year <- data$Year
interest_rate <- data$`Short term interest rate`
GDP <- data$`Nominal GDP`
logGDP<- data$`Log Nominal GDP`
Inflation <- data$`Actual inflation`
Outgap <- data$`Output gap`
Target <- data$`Inflation target`
Inflationgap <- Inflation - Target
unemployment <- data$`Unemployment rate`

# Perform linear regression of interest rate on output gap and inflation gap
fit <- lm(interest_rate ~ Outgap + Inflationgap, data = data)
summary(fit)
residualPlot(fit)
influencePlot(fit)
autoplot(fit)

# Perform linear regression of interest rate on output gap and inflation gap and unemployment rate
fit2 <- lm(interest_rate ~ Outgap + Inflationgap + unemployment, data = data)
summary(fit2)
residualPlot(fit2)
autoplot(fit2)


# Plot interest rates over time
plot_interest <- ggplot(data, aes(x = year, y = interest_rate)) +
  geom_point() +
  labs(title = "Brazil Interest Rates 1996-2021",
       x = "Year",
       y = "Interest Rate") +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
plot_interest

# Plot interest rates over time
plot_interest <- ggplot(data, aes(x = unemployment, y = interest_rate)) +
  geom_point() +
  labs(title = "Brazil correlation Interest Rates unemployment 1996-2021",
       x = "unemployment",
       y = "Interest Rate") +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
plot_interest
cor(unemployment, interest_rate)

# Plot unemploymet rates over time
plot_interest <- ggplot(data, aes(x = year, y = unemployment)) +
  geom_point() +
  labs(title = "Brazil unemployment 1996-2021",
       x = "Year",
       y = "Interest Rate") +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
plot_interest



# Plot GDP over time
plot_gdp <- ggplot(data, aes(x = year, y = GDP)) +
  geom_point() +
  labs(title = "Brazil GDP 1996-2021",
       x = "Year",
       y = "GDP") +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
plot_gdp

# Plot logGDP over time
plot_loggdp <- ggplot(data, aes(x = year, y = logGDP)) +
  geom_point() +
  labs(title = "Brazil logGDP 1996-2021",
       x = "Year",
       y = "logGDP") +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
plot_loggdp

# Plot output gap over time
plot_outgap <- ggplot(data, aes(x = year, y = Outgap)) +
  geom_point() +
  labs(title = "Brazil Output Gap 1996-2021",
       x = "Year",
       y = "Output Gap") +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
plot_outgap

# Plot inflation gap over time
plot_inflationgap <- ggplot(data, aes(x = year, y = Inflationgap)) +
  geom_point() +
  labs(title = "Brazil Inflation Gap 1996-2021",
       x = "Year",
       y = "Inflation Gap") +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
plot_inflationgap

# Plot inflation rate over time
plot_inflation <- ggplot(data, aes(x = year, y = Inflation)) +
  geom_point() +
  labs(title = "Brazil Inflation Rates 1996-2021",
       x = "Year",
       y = "Inflation Rate") +
  stat_smooth(method = "lm", formula = y ~ x, alpha = 0.2)
plot_inflation

# Run OLS regression using taylor rule
taylor_reg <- lm(interest_rate ~ Outgap + Inflationgap, data = data)

# Print summary of regression results

# Interpretation of coefficients and fitted values
cat("\nInterpretation of coefficients:\n")
cat(paste0("Intercept: ", round(coef(taylor_reg)[1], 3), "\n"))
cat(paste0("Outgap: ", round(coef(taylor_reg)[2], 3), "\n"))
cat(paste0("Inflationgap: ", round(coef(taylor_reg)[3], 3), "\n"))

cat("\nInterpretation of fitted values:\n")
cat("The fitted values represent the predicted values of the interest rate based on the Outgap and Inflationgap values in the data.\n")


# Draw fitted values vs real values
ggplot(data) +
  aes(x = year) +
  geom_point(aes(y = interest_rate, color = "Interest Rate"), size = 3) +
  geom_point(aes(y = fitted(taylor_reg), color = "Fitted Value"), size = 3) +
  scale_color_manual(name = "Legend", values = c("Interest Rate" = "blue", "Fitted Value" = "red")) +
  labs(x = "Year", y = "Interest Rate") +
  theme_minimal()

# Run tests on the OLS assumptions using residuals
residuals <- residuals(taylor_reg)
residuals_df <- data.frame(residuals, year = 1996:2021)
residuals_df
# TEST FOR LINEARITY

# run Reset Test
cat("\n\nTEST FOR LINEARITY:\n")
reset_result <- resettest(taylor_reg, power = 2, type = "fitted")
cat(paste0("Reset Test (Power 2): p-value = ", round(reset_result$p.value, 3), "\n"))
reset_result <- resettest(taylor_reg, power = 3, type = "fitted")
cat(paste0("Reset Test (Power 3): p-value = ", round(reset_result$p.value, 3), "\n"))
reset_result <- resettest(taylor_reg, power = 4, type = "fitted")
cat(paste0("Reset Test (Power 4): p-value = ", round(reset_result$p.value, 3), "\n"))

# TEST FOR LINEARITY
# run Reset Test
resettest(taylor_reg, power = 2:3, type = "fitted")
resettest(taylor_reg, power = 3, type = "fitted")
resettest(taylor_reg, power = 4, type = "fitted")

# at a 5% significance level, we do not reject the null hypothesis that
# the relationship is linear (for the three power tested).

# TEST FOR NORMALITY OF RESIDUALS
# Plot distribution of residuals
ggplot(data.frame(residuals = residuals(taylor_reg))) +
 aes(x = residuals) +
 geom_histogram(aes(y = stat(count) / sum(count)), colour = "black", fill = "white", bins = 10) +
 geom_density(alpha = .3, fill = "#FF6666", size = 1) +
 labs(
   title = "Residuals",
   x = "Residual Value",
   y = "Relative Frequency"
 )

# Run Jarque Bera Test
jarque.bera.test(residuals(taylor_reg))
# at a 5% significance level, we do reject the null hypothesis that
# the residuals are normally distributed.

