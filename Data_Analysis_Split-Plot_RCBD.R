# Loading libraries

library(ggplot2)
library(agricolae)
library(dplyr)

rm(list = ls()) # Cleaning my global environment

################################################################################
## Analysis of experimental data with R: Example of a split-plot with main
## plots arranged as Randomized Complete Block Design (RCBD)

## Treatments are bacterial inoculation (inoculated versus not inoculated with
## pathogen Erwinia carotovora) and in-row spacing between sugarbeet plants
## (4, 6, 12 and 18 inches)

## Bacterial inoculation was assigned to main plots and plant spacing to subplots

## Response variable is sugarbeet yield (lb/acre)
################################################################################


################################################################################
## STEP 1: CREATING DATAFRAME WITH EXPERIMENT DATA
################################################################################

# Treatments
inoculation <- c(1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,0,0,0,1,
            1,1,1,0,0,0,0,1,1,1,1,0,0,0,0)

spacing <- c(4,6,12,18,4,6,12,18,4,6,12,18,4,6,12,18,4,6,12,18,4,6,12,18,
           4,6,12,18,4,6,12,18,4,6,12,18,4,6,12,18,4,6,12,18,4,6,12,18)

# Blocks
block <- c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,
           5,5,5,5,5,5,5,6,6,6,6,6,6,6,6)

# Response variable
yield <- c(17.4,17.3,16.3,12.5,20.1,20.2,21.8,20.0,16.4,17.0,14.9,12.1,17.9,
           19.6,21.1,20.1,16.5,19.1,16.6,12.5,18.2,20.8,18.6,19.3,16.8,16.3,
           16.1,14.7,18.4,21.1,22.8,21.4,17.2,19.8,16.8,12.9,17.9,21.2,22.3,
           22.0,16.8,17.6,16.1,13.1,21.0,22.0,22.9,23.1)


# Creating original data frame where all variables are numeric

sugarbeet_df <- data.frame(inoculation, spacing, block, yield)


# Creating second data frame where 'inoculation' is a factor variable

sugarbeet_df2 <- sugarbeet_df
  
sugarbeet_df2$inoculation <-  as.factor(inoculation)


# Adding new block and plant spacing variables, which are factor instead of numeric
sugarbeet_df2$block2  <- as.factor(block)
sugarbeet_df2$spacing2 <- as.factor(spacing)

# Checking data structure
str(sugarbeet_df)
str(sugarbeet_df2)

print(sugarbeet_df2)

################################################################################
## STEP 2: DATA VISUALISATION
################################################################################

# Create a scatterplot with *numeric* plant spacing variable (spacing)
# This is necessary to plot fitted lines with loess method

scatterpl <- ggplot(sugarbeet_df2, aes(x= spacing, y= yield,
                                      colour= inoculation)) +
  geom_point() +
  scale_color_brewer(palette = "Set2")

scatterpl +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95) +
  ggtitle("Effect of bacterial vascular necrosis on sugarbeet yield") +
  xlab("In-row plant spacing (in)") +
  ylab("Yield (lb/acre)") +
  labs(colour = "Bacterial\ninoculation") +
  theme_classic()



################################################################################
## STEP 3: SUMMARY STATISTICS
################################################################################

sugarbeet_df3 <- sugarbeet_df2 %>%
  group_by(spacing2, inoculation) %>%
  summarise(no = n(),
            treat_mean = mean(yield),
            se = sd(yield, na.rm = TRUE)/
                 sqrt(n()))
sugarbeet_df3

sugarbeet_df3$grouping <- c("b", "cd", "a", "bc", "a", "d", "a", "e")

# Create a histogram

ggplot(sugarbeet_df3, aes(x= spacing2,
                          y= treat_mean,
                          fill= inoculation)) +
  geom_bar(position= position_dodge(width= 0.9), stat = "identity",
           col= "black") +
#  geom_col(position= "dodge", col= "black") +
  geom_errorbar(aes(ymin= treat_mean - se, ymax= treat_mean + se),
 #               group = inoculation,
                position= position_dodge(width=.9),
                width= 0.2
                ) +
  scale_y_continuous(limit=c(0,25)) +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Effect of bacterial vascular necrosis on sugarbeet yield") +
  xlab("In-row plant spacing (in)") +
  ylab("Yield (lb/acre)") +
  labs(fill = "Bacterial\ninoculation") +
  geom_text(aes(label= grouping, y= treat_mean + se),  # group = inoculation
            vjust= -.3,
            size= 5,
            position = position_dodge(width= 0.9)) +
  theme_classic()


################################################################################
## STEP 4: ANALYSIS OF VARIANCE WITH R PACKAGE AGRICOLAE
################################################################################

# Method 1: Performing analysis with *aov* function using all explanatory and
# block variables as factor variables

model1 <- aov(yield ~ block2 + inoculation*spacing2 + Error(block2/inoculation),
                   data= sugarbeet_df2)
summary(model1)

################################################################################
## Notes on aov output summary
## Top table: Sum of Squares for block term; no hypothesis testing
##
## Middle table: hypothesis testing for main plot effect (inoculation), using 
## [block*main plot] interaction as the error term (df=5). [block*main plot] 
## estimate is shown as "Residuals".
##
## Bottom table: hypothesis testing for subplot main effect (spacing) and
## interaction term [inoculation*spacing]. They are tested using the
## subplot error term (df=30), shown as "Residuals"
################################################################################



# Method 2: Performing analysis with *sp.plot* function using all explanatory
# variables as factor variables, and block variable as numeric variable

model2 <- with(sugarbeet_df2, sp.plot(block, inoculation, spacing2, yield)) 

summary(model2)

################################################################################
## Notes on sp.plot output summary
## Eb is the subplot error term (df=30), not used to test main plot effect
## Ea is the [block*main plot] error term (df=5), used to test
## main plot and block effects
##
## Observation: F and P values for the block term are incorrect, because Eb is
## used as error term (proper error term should be Ea). In this example, the
## F value for the block term should be 1.41 rather than 4.15
################################################################################


################################################################################
## STEP 5: MEAN COMPARISON USING LSD TEST
################################################################################

# Get values from data frame and sp.plot output

a <- n_distinct(sugarbeet_df2$inoculation) # number of main plot levels (inoculation)
b <- n_distinct(sugarbeet_df2$spacing2) # number of subplot levels (spacing)
a_x_b <- a*b
r <- n_distinct(sugarbeet_df2$block2) # number of blocks

gla <- model2$gl.a # degrees of freedom [block*main plot] error term
glb <- model2$gl.b # degrees of freedom subplot error term
mse_a <- model2$Ea # [block*main plot] error term
mse_b <- model2$Eb # subplot error term


# Mean comparisons for main effects (inoculation and spacing) are not really 
# needed in this example, because of their significant interaction.
# However, we proceed to calculate them anyway to estimate the critical
# values of t (t_a and t_b)

mean_comp_a <- with(sugarbeet_df2,
                    LSD.test(yield,
                             inoculation,
                             gla,
                             mse_a,
                             console = TRUE))

mean_comp_a

t_a <- mean_comp_a$statistics$t.value

mean_comp_b <- with(sugarbeet_df2,
                    LSD.test(yield,
                             spacing2,
                             glb,
                             mse_b,
                             console = TRUE))

mean_comp_b

t_b <- mean_comp_b$statistics$t.value



# Manually calculate weighted values (MSE_mix, t_mix and LSD_mix) for mixed comparisons

mse_mix <- ((b - 1)*mse_b + mse_a)/(b)

t_mix <- ((b - 1)*t_b*mse_b+t_a*mse_a)/((b - 1)*mse_b + mse_a)

lsd_mix <- t_mix*sqrt((2*mse_mix)/r)


# LSD test for treatment combinations using custom MSerror calculated previously

mean_comp_ab <- with(sugarbeet_df2,
                     LSD.test(yield,
                              inoculation:spacing2,
                              DFerror = a_x_b, # 2 inoculation x 4 spacing = 8
                              MSerror = mse_mix))

mean_comp_ab

