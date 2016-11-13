library(tidyverse)
library(apaTables)

# KEEPING THE POPULATION IN MIND

## We will conduct a regression on a sample drawn from a population. Then we will compare our sample results to the population level results.
## In practice, this can never be done - because we don't have the population data. 
## This exercise will allow you to learn to always remember how the sample regression results may deviate from the population regression results. 



# OUR TOPIC

## We are interested in examining how IQ predicts academic performance (i.e., first year average) for first year uni students in Ontario.
## In this hypothetical data set, we have 2 variables: performance and IQ



# LOADING THE POPULATION

## We will load the population data and obtain a sample from the population
population_data <- read_csv("population_data.csv")

## We can glimpse the population to determine the variables and number of people
glimpse(population_data)
  ### You can see there are 2 variables (performance, IQ) collected for 10,000 people



# SAMPLE 1

## We can obtain a sample of 200 people from the population with the sample_n command.
## If you want your results to match mine exactly also type the set.seed command. Generally, you can skip the set.seed command unless you are matching your output to mine.

set.seed(1)
sample1_analytic_data <- sample_n(population_data, size=200)

glimpse(sample1_analytic_data)



# CONDUCTING THE REGRESSION

## We conduct a regression with the lm command (lm stands for linear model).
## We put the criterion (i.e., DV on the left). We use the tilde (~) symbol to indicate "is predicted by". Then we use IQ as the predictor.
## Implicit in this equation is the fact a constant will be used. 
## If we were to be explicit about the constant we would have writen the equation as performance~IQ+1). 
## But, we don't need to do so - the +1 is included automatically even if we don't do it.

sample1_lm_results <- lm(performance~IQ, data=sample1_analytic_data)

## We can briefly view the regression results using the summary command.
## Notice how this command gives the unstandardized regression coefficient (b) and many but not all of the regression stats typically reported.
## Notably, standardized regression coefficients (beta), CIs, and correlations witht he predictor are not reported.

summary(sample1_lm_results)

## But, it's more helpful to get detailed output using apa.reg.table from the apaTables package. 
## This output has standardized regression coefficients, CIs and correlations for predictor(s).

apa.reg.table(sample1_lm_results)




# INTERPRETING THE REGRESSION

## B/c be only have the data in sample 1, we don't know the unstandardized slope (i.e., b) in the population...
  ### BUT, the CI on b gives us a plausible range for the population value.
## Likewise, if you prefer standardized slopes, we don't know the standardized slope (i.e., beta) in the population...
  ### BUT, the CI on beta gives us a plausible range for the population value.

## We can do something unusual to demonstrate the impact of sample error. We conduct a regression on the population.
pop_results <- lm(performance~IQ, data=population_data)
summary(pop_results)

  ### Compare the IQ slope for the population (.20) to the IQ slope for sample 1 in the previous output. Are they similar?
  ### Compare the IQ slope for the population (.20) to CI for the IQ slope in sample 1 in the previous output (i.e., [.19, .29]). 
      #### You can see that the IQ slope CI for sample 1 captures the population IQ slope.




# CI: PREDICTED VALUE FOR Y FOR A GIVEN SINGLE VALUE ON X

## We can get a predicted value for y (i.e., performance) for a given x (i.e., IQ) as per below.
## Here we want to know the predicted value of performance for an IQ of 120. 
## The predicted value is presented under the column fit.
## This output also gives us a 95% CI for this value.
### The predicted value (i.e., the value in fit column) represents the mean of a distribution of scores on Y (i.e., performance) located at a value of 120 on the x-axis.

x_axis_range <- data.frame(IQ = c(120)) # Creating IQ column / data set

CI_data <- predict(sample1_lm_results, newdata = x_axis_range, interval = "confidence", level = 0.95) # Predicting y with CIs

CI_data <- as.data.frame(cbind(x_axis_range, CI_data)) # Adding IQ column into the data set just created when predicting y 

print(CI_data)





# CI: PREDICTED VALUE Y FOR A GIVEN RANGE OF VALUES ON X

## If we want to plot the CI on a graph, we would need to know the predicted y value (and CI) for a range of x-values.
## Here's how to calculate those values...

### Calculate the minimum and maximum values of X (i.e., min and max IQ values)
min_predictor <- min(sample1_analytic_data$IQ)
max_predictor <- max(sample1_analytic_data$IQ)

### We use .5 to generate a large number of steps between the min and max IQ values.
### You will need to adjust the step size (i.e., by size) for each data set
x_axis_range <- data.frame(IQ = seq(min_predictor, max_predictor, by=0.5))

### Calculate the CI
CI_data <- predict(sample1_lm_results, newdata = x_axis_range, interval = "confidence", level = 0.95)

CI_data <- as.data.frame(cbind(x_axis_range, CI_data)) # We will use CI_data later when we graph!

print(CI_data)





# PI: PREDICTED VALUE FOR Y FOR A GIVEN VALUE OF X 

x_axis_range <- data.frame(IQ = c(120))

PI_data <- predict(sample1_lm_results, newdata = x_axis_range, interval = "prediction", level = 0.95)

PI_data <- as.data.frame(cbind(x_axis_range, PI_data))

print(PI_data)





# PI: PREDICTED VALUE FOR Y FOR A GIVEN RANGE OF VALUES ON X

## Calculate the minimum and maximum values of x (i.e., min and max IQ values)
min_predictor <- min(sample1_analytic_data$IQ)
max_predictor <- max(sample1_analytic_data$IQ)

### We use .5 to generate a large number of steps between the min and max IQ values.
### You will need to adjust the step size (i.e., by size) for each data set
x_axis_range <- data.frame(IQ = seq(min_predictor, max_predictor, by=0.5)) # Creating data frame of min and max IQ scores in increments of 0,5

# Calculate the PI
PI_data <- predict(sample1_lm_results, newdata = x_axis_range, interval = "prediction", level = 0.95)

PI_data <- as.data.frame(cbind(x_axis_range, PI_data)) # We will use PI_data later when we graph!

print(PI_data)





# GRAPHING

## We can create a graph with a regression line using the ggplot2 code below. 
## Note that in reality it would take several iterations to create this graph.
## I would have to create the graph & view it before setting the axis properties using coord_cartesian and scale_x_continuous.


## REGRESSION LINE ONLY 

reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance)) # Create empty graph with X and Y axis labelled
reg_plot <- reg_plot + geom_smooth(method = "lm", se = FALSE, color = "black") # Add line
reg_plot <- reg_plot + geom_point() # Add data points and adjusts line
reg_plot <- reg_plot + theme_classic() # White background with no grid lines
reg_plot <- reg_plot + coord_cartesian(xlim=c(50,150), ylim = c(0,100))
reg_plot <- reg_plot + scale_x_continuous(breaks=seq(50,150,by=10))
print(reg_plot)


## REGRESSION LINE & CI

### This is the same graph code as the code above but we simply used se=TRUE in the geom_smooth command.
### Note: This approach does not use the CI data we created.

reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance)) # Create empty graph with X and Y axis labelled
reg_plot <- reg_plot + geom_smooth(method = "lm", se = TRUE, color = "black") # Add line
reg_plot <- reg_plot + geom_point() # Add data points and adjusts line
reg_plot <- reg_plot + theme_classic() # White background with no grid lines
reg_plot <- reg_plot + coord_cartesian(xlim=c(50,150), ylim = c(0,100))
reg_plot <- reg_plot + scale_x_continuous(breaks=seq(50,150,by=10))
print(reg_plot)

### An alternative approach is below. 
### This approach will use the CI data we created as the basis for creating PIs later

reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_smooth(data = CI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat = "identity")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim=c(50,150), ylim = c(0,100))
reg_plot <- reg_plot + scale_x_continuous(breaks=seq(50,150,by=10))
print(reg_plot)


## REGRESSION LINE & PI 

## Unfortunately, there isn't an easy way to put PI on a graph in ggplot2 - we have to use the PI data we created. 

reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_smooth(data = PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat = "identity")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim=c(50,150), ylim = c(0,100))
reg_plot <- reg_plot + scale_x_continuous(breaks=seq(50,150,by=10))
print(reg_plot)


## REGRESSION LINE WITH CI & PI

reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_smooth(data = CI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat = "identity") # SEE BELOW FOR SHORT CUT
reg_plot <- reg_plot + geom_smooth(data = PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat = "identity")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + coord_cartesian(xlim=c(50,150), ylim = c(0,100))
reg_plot <- reg_plot + scale_x_continuous(breaks=seq(50,150,by=10))
print(reg_plot)

### CI line short cut... reg_plot <- reg_plot + geom_smooth(method="lm", se=TRUE)


## REGRESSION LINE WITH PI AND POPULATION DATA

### We can see how the PI captures 95% of the individuals in the population by plotting all of the population data on the graph.
### You would never do this in an actual analysis since you would never have the population data.
### This graphic is just to help you understand the nature of PIs.

reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
#reg_plot <- reg_plot + geom_smooth(method="lm", se=FALSE, color="black")
reg_plot <- reg_plot + geom_point(data = population_data, aes(x=IQ, y=performance), colour="gray40")
reg_plot <- reg_plot + geom_smooth(data = CI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat = "identity")
reg_plot <- reg_plot + geom_smooth(data = PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat = "identity")
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_bw()
reg_plot <- reg_plot + coord_cartesian(xlim=c(40,160), ylim=c(0,100))
reg_plot <- reg_plot + scale_x_continuous(breaks = seq(40,160,by=20))
print(reg_plot)

# DONE! :)