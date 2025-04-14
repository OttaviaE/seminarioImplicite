# modelli linear a effetti misti per ottenere stime di tipo Rasch dai dati IAT -----
# install.packages("lme4")
# install.packages("car")
# install.packages("ggplot2")

library(lme4) # Fit the linear mixed-effects models
library(car)  # recode the variables
library(ggplot2) # plot the results 

my_url = "https://ottaviae.github.io/DscoreApp/raceAPP.csv"
iat_data = read.csv(url(my_url))
# The code from line 250 to line 284 deals with a peculiar regex in Italian and prepares the 
# labels of the of the stimuli for being translated in English and prepare the dataset for the analysis
temp = iat_data[iat_data$stimoli %in% "felicit\xe0", ]
iat_data = iat_data[!iat_data$stimoli %in% "felicit\xe0", ]
temp$stimoli = "felic"
iat_data = rbind(iat_data, temp)

iat_data = iat_data[, c("participant", "blockcode", "stimoli", "latency", "correct")]
# renames the columns containing the conditions and stimuli labels
colnames(iat_data)[2:3] = c("condition", "stimuli")
# recode the labels of the condition
iat_data$condition = car::recode(iat_data$condition,
                                 "'Whitegood' = 'WGBB';
                              'Whitebad' = 'BGWB'")
# ACCURACY MODELS (IAT DATA) -----
# M1: random intercepts of the respondents (1|participant) and of the stimuli (1|stimuli) 
iat_accuracy1 = glmer(correct ~ 0 + condition  + (1|participant) + (1|stimuli),
                      data = iat_data,
                      family = "binomial")
summary(iat_accuracy1)
# obtain the expected proportions of correct responses in the two conditions
exp(fixef(iat_accuracy1))/(1+ exp(fixef(iat_accuracy1)))
# M2: random intercepts of the respondents (1|participant), random slopes of the stimuli
# in the conditions (0 + condition|stimuli)
iat_accuracy2 = glmer(correct ~ 0 + condition + (1|participant) + (0 + condition|stimuli),
                      data = iat_data,
                      family = "binomial")
summary(iat_accuracy2)
# obtain the expected proportions of correct responses in the two conditions
exp(fixef(iat_accuracy2))/(1+ exp(fixef(iat_accuracy2)))
# M3: random intercepts of the stimuli (1|stimuli), random slopes of the respondents
# in the conditions (0 + condition|respondent)
iat_accuracy3 = glmer(correct ~ 0 + condition + (0 + condition|participant) + (1|stimuli),
                      data = iat_data,
                      family = "binomial")
summary(iat_accuracy3)
# obtain the expected proportions of correct responses in the two conditions
exp(fixef(iat_accuracy3))/(1+ exp(fixef(iat_accuracy3)))
# compare accuracy models ------
anova(iat_accuracy1, iat_accuracy2, iat_accuracy3)
# MODEL 2 IS CHOSEN AS THE BEST FITTING ONE BECAUSE IT HAS LOWER DEVIANCE, AIC
# THAN MODELS 1 AND 3. MODEL 1 HAS LOWER BIC.

# RASCH-LIKE ESTIMATES (IAT DATA) ----
# overall ability estimates ----
ab_iat = data.frame(sbj = rownames(coef(iat_accuracy2)$participant), # store the respondents IDs in the sbj column 
                    theta = coef(iat_accuracy2)$participant[,1]) # store the BLUP of the respondents in the theta column
# the code from line 320 to line 323 can be used for plotting the theta estimates of 
# the respondents 
graph_theta_iat = ggplot(ab_iat, # dataset with the ability estimates
                         aes(x = theta)) + # theta estimates on the x axis
  geom_density() # plot the density of the theta distribution
graph_theta_iat  # print the plot
# the code from line 331 to line 339 results in the plot represented in Figure 10a in the manuscript
graph_theta_iat + # object containing the starting plot
  geom_density(fill = mycol[1], # change the color of the filling of the density distribution
               alpha = .40) + 
  theme_light() + # define the theme of the plot
  theme(legend.position =  "none", # remove the legend
        axis.title.y = element_blank(), # remove the title of the y-axis
        axis.title.x = element_text(size = 30, face = "bold"), # increase the size of x-axis title and set it to bold
        axis.text = element_text(size = 26, face = "bold")) + # increase the size of x-axis text and set it to bold
  xlab(expression(theta)) # change the label of the x-axis

# condition--specific easiness (IAT DATA) ----
# extract condition-specific BLUP for each stimulus to obtain the condition specific 
# easiness estimates by summing them to the fixed effect of the conditions
blup_easiness_iat = as.data.frame(ranef(iat_accuracy2,   # BLUP of the stimuli
                                        condVar = TRUE)) # variance of the random effects
# extract the fixed effects
fixef_accuracy_iat = data.frame(term = names(fixef(iat_accuracy2)), # assign the labels of the conditions to the term column
                                est = (fixef(iat_accuracy2))) # assign the estimates of the fixed effects to the est column
# merge together the fixed effects and stimuli BLUP with their variances
easiness_iat = merge(blup_easiness_iat, fixef_accuracy_iat)
# change the column names ----
colnames(easiness_iat) = c("Condition", "grpvar", "stimuli",
                           "blup", "se", "fix.est")
# compute easiness estimates as the sum of the fixed effects + BLUP
easiness_iat$b = with(easiness_iat, fix.est + blup)
# the code from line 358 to line 365 can be used for plotting the condition-specific stimuli estimates 
# with their confidence intervals 
graph_b_ci = ggplot(easiness_iat, # dataset with the easiness estimates
                    aes(x = b, # easiness estimates on the x-axis
                        y = stimuli, # labels of the stimuli on the y axis
                        color = Condition)) + # change the color according to the condition
  geom_point() + # draw points to represent the easiness of each stimulus
  geom_errorbarh(aes(xmin = b - 1.96*se, # draw the error bars to represent the confidence intervals  (CI)
                     xmax= b + 1.96*se)) # In this case, a 95% CI is represented (1.96). Change this value accordingly
graph_b_ci # print the plot
# the code from line 368 to line 375 results in the plot represented in Figure 10b in the manuscript
graph_b_ci +  # object containing the starting plot
  geom_point(size = 6) + # increase the size of the points
  geom_errorbarh(aes(xmin = b - 1.96*se, xmax= b + 1.96*se)) + 
  theme_light() +  # define the theme of the plot
  scale_color_manual(values = mycol) + # change the colors of the conditions 
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 32, face = "bold"), # increase the size of x-axis title and set it to bold
        axis.text = element_text(size = 20, face = "bold"), # increase the axes text and set it to bold
        axis.title.y = element_blank()) # remove the title of the y-axis
# LOG-TIME MODELS (IAT data) ----
# M1: random intercepts of the respondents (1|participant) and of the stimuli (1|stimuli) 
iat_time1 = lmer(log(latency) ~ 0 + condition + (1|participant) + (1|stimuli),
                 REML = FALSE,
                 data = iat_data)
summary(iat_time1)
# M2: random intercepts of the respondents (1|participant), random slopes of the stimuli
# in the conditions (0 + condition|stimuli)
iat_time2 = lmer(log(latency) ~ 0 + condition + (1|participant) + (0 + condition |stimuli),
                 REML = FALSE,
                 data = iat_data)
summary(iat_time2)
# M3: random intercepts of the stimuli (1|stimuli), random slopes of the respondents
# in the conditions (0 + condition|respondent)
iat_time3 = lmer(log(latency) ~ 0 + condition + (0 + condition |participant) + (1|stimuli),
                 REML = FALSE,
                 data = iat_data)
summary(iat_time3)
# compare accuracy models ------
anova(iat_time1, iat_time2, iat_time3)
# MODEL 3 SHOWS LOWEST AIC BIC DEVIANCE AND HIGHEST LOGLIKELIHOOD. IT IS CHOSEN FOR EXTRACTING THE LOG-NOTMAL MODEL ESTIMATES

# condition-specific speed estimates (IAT DATA) ----
speed_iat = coef(iat_time3)$participant[,-1] # store the speed estimates of the respondents
# reshape the dataset with the speed estimates from wide to long 
sp_iat = stack(speed_iat)
# rename the columns
colnames(sp_iat) = c("tau", "condition")
# the code from line 405 to line 409  can be used for plotting the condition-specific speed estimates 
graph_speed_iat = ggplot(sp_iat, # dataset with the condition-specific speed estimates in long format
                         aes(x = tau, # speed estimates on the x-axis
                             fill = condition)) + # change color according to the condition
  geom_density() # density distributions of the speed estimates in the two conditions
graph_speed_iat # print the plot
# the code from line 411 to line 459 can be used for customizing the plot 
graph_speed_iat + # object containing the starting plot
  geom_density(alpha = .40) + # change the saturation of the filling
  theme_light() + # define the theme of the plot
  theme(legend.position =  "none", # remove the legend
        axis.title.y = element_blank(), # remove the title of the y-axis
        axis.title.x = element_text(size = 30, face = "bold"), # increase the size of x-axis title and set it to bold
        axis.text = element_text(size = 26, face = "bold")) + # increase the size of x-axis text and set it to bold
  xlab(expression(tau)) + # change the label of the x-axis
  scale_fill_manual(values = mycol) + # change the color of the filling of the density distribution
  annotate("text", # add the text with the label "BGWB"
           label = "BGWB",
           x =7.2,
           y =  2,
           size = 14,
           color = mycol[1],
           fontface = "bold",
           alpha = .60
  ) +
  geom_curve( # add the arrow pointing from the label "BGWB" to the corresponding distribution
    aes(x =7.2,
        y = 1.85,
        xend = 6.98,
        yend = 1.05
    ),
    colour = mycol[1],
    alpha =.60,
    size = 1.5,
    curvature = -0.2,
    arrow = arrow(length = unit(0.02, "npc"))) + 
  annotate("text",
           label = "WGBB",
           x =6.3,
           y =  2.8,
           size = 14,
           color = mycol[2],
           fontface = "bold",
           alpha = .60
  ) + geom_curve(
    aes(x =6.27,
        y = 2.65,
        xend = 6.42,
        yend = 2
    ),
    colour = mycol[2],
    alpha =.60,
    size = 1.5,
    curvature = 0.2,
    arrow = arrow(length = unit(0.02, "npc")),
  )

# overall time intensity parameter (IAT DATA) -----
intensity_iat = data.frame(stimulus = rownames(coef(iat_time3)$stimuli), # store the stimuli labels in the stimulus column 
                           intensity = coef(iat_time3)$stimuli[,1]) # store the BLUP of the stimuli in the intensity column
# the code from line 466 to line 470 can be used for plotting the theta estimates of 
# the respondents 
graph_delta_iat = ggplot(intensity_iat, # dataset with the intensity estimates
                         (aes(x = intensity, # intensity estimates on the x-axis
                              y = stimulus))) + # stimulus labels on the y-axis
  geom_point() # draw points to represent the intensity of each stimulus
graph_delta_iat # print the plot
# the code from line 471 to line 482 customizes the plot 
ggplot(intensity_iat,
       (aes(x = intensity,
            y = reorder(stimulus, intensity)))) +# reorder the stimuli labels according to the value of delta
  geom_point(size = 6) + # increase size of the points 
  theme_light() + # define the theme of the plot
  geom_vline(xintercept = 0, linetype = 2) + # draw a line to represent the mean of the time intensity estimates
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 30, face = "bold"),
        axis.text = element_text(size = 24, face = "bold")) + 
  xlab(expression(delta)) +
  xlim(c(-1,1)) # set the limits of the x-axis
