# Meetings

# Thoughts ----------------------------------------------------------------

# 1. General questions on cpt.mean function

# cpt.mean vs cpt.var or cpt.meanvar
# method - bcp? bayesian changepoint?
# ?penalty values, pen.value, minseglen

# 2. Graph derivative function

# geom_smooth?

# 3. adjust for serving

# weight points

# 4. Self Exciting?

# split into runs, not sure if this fits? How can we assess affect of other points


# Big Picture Ideas -------------------------------------------------------

# find change points
# compile change points into data set, assess what they look like
# using assessment predict future change points?





# Meeting with Dr. Sturdivant 4/15 ----------------------------------------
# question: assumptions for logistic regression?

# Meeting with Dr. Sturdivant 4/10 ----------------------------------------
# results
  # different effect sizes by sex (!!)
    # forgot to include unforced errors! Results are not meaningful,
    # do slightly impact effect sizes of other covariates
  # two verification - 1) backward momentum on future points
  # 2) backward momentum on future momentum

# conclusion & introduction - can I send to him?
  # talk through discussion big picture
  # the rest I'd like to send to him - wordings

# references
  # generally how does this work? I need a work cited?
  # in text citations of R packages? Or at the end? overall R.
  # any other methods that I would need to cite? linear model assumptions, etc.
  # citations for derivative, linear, assumptions
  # can I cite myself (conclusion)

# last question: bolding variables?

# Future work for me:
## Thesis Document
  # edit document 
  # tidy up Appendix
  # combine document
  # add citations
  # format
  # goal is to send thesis to readers by April 15, so readers have 9 days to read it

## Presentations
  # I present for Honors Week on Tuesday, April 16 (can we meet on Monday normal time 11am? May have some questions)
  # Present for undergraduate conference on Saturday, April 20
  # Defense is Wednesday, April 24 at 10:45 (can we meet on Monday 11am to discuss defense?)

# Meeting with Dr. Sturdivant 3/27 ----------------------------------------

# with Sturdivant
# questions:
# assumptions: leverage? autocorrelation? full and reduced model?
# add sub model?
# schedule defense

# Meeting with Dr. Sturdivant 3/18 ----------------------------------------

# with Sturdivant
# questions: table of models - combine the two? Is it ok to have two predictors?
# add sub model?

# Meeting with Dr. Sturdivant 3/11 ----------------------------------------

# with Sturdivant
# 1. big picture thoughts on write-ups so far
#   i.e. too many figures? location of figures? references?
# 2. thoughts on future work section

# future for Caleb
# 1. work on linear model (model assumptions, residuals, qqplot)
# add plots with interactions
# 2. start conclusions/future work
# 3. appendix edit and intro
# 4. edit Literature Review (with more context of my work)
# 5. edit Data Chapter (potentially remove unimportant covariates/add others)
# 6. Abstract
# 7. Introduction
# 8. Formatting!

# Meeting with Dr. Sturdivant 2/19 ----------------------------------------

# updates
# 1. betting odds transformation
# 2. randomize players
# -> impact results
# 3. question on S curve
# 4. added appendix and worked on methodology
# -> questions about appendix and methodology
# 5a. future... send in logistic and methodology
# 5b. work on linear and start future work

# Meeting with Dr. Sturdivant 2/5 -----------------------------------------
# 1. ideas for linear model
# 2. big picture worry

# dominance in match
#    |              |
#    |               |
#    |                |
# winning (cov) ---> momentum



# 3. show results for each linear model and next steps?
# 3b. betting odds transformation? (to implied win probability)
# 4. does momentum exist write up
# 5. show introductory momentum analysis? t test for props?
# 6. citing code? In tables/graphs?

# Meeting with Dr. Sturdivant 1/22 ----------------------------------------

# 1. Organize with 2 Questions (exists and causes)
# 1a. exist - logistic model (does momentum exist)
# 1b. causes - linear model (what causes momentum)
#    - thoughts

# 2. Control variable - which to use
#    - in data page, logistic regression, ps has lowest AIC

# 3. Methodology section layout
#   - visualizing a match (check)
#   - defining momentum (as derivative, how to calculate)
#   - splits: 2 different types of momentum (Momentum Entering a Point and Momentum after a point)
#   - explain exponential smoothing, seasonality, and derivative methods for these
#   - then, explain both models
#   - what does depth of this look like?
#   - after this onto results/conclusions/future work!

# 4. Table Labels - tables in general

# Meeting with Dr. Sturdivant 10/2 ----------------------------------------

# 1. Thoughts on derivative, way to standardize lamba

# 2. First model; predicted is derivative, predictor is lag, covariates?

# covariate ideas - serve, rally length, break saved, break converted, interruption/change ends, tiebreak victory, pressure point
# we need to find a way to adjust for player rank - betting odds would be ideal.

# Meeting with Dr. Sturdivant 9/18------------------------------------------------

# 1. Updated Derivative Graph, other three options

# 2. Covariates, colored, and ideas (control - ranking)

# 3. lit review, findings and thoughts: existence of momentum is debated,
# strategic vs psychological, when: interruptions lessen, winning previous point inc,
# from a tiebreak

# 6. Next steps: look at model, write lit review, organize findings

# 7. Grad School - letter of recommendation and ideas on third person (Becca, Dr. Green, Shannon Clark)

# Future Work:

# 1. Fix Derivative using true smoother: cubic bspline bases, functional data analysis class

# color points based on potential co-variates

# Meeting with Dr. Sturdivant 9/1------------------------------------------------

# 1. Data Set - ~1.4 million obs, combined tournaments, removed matches with missing points

# 2. Overall - done a lot of things (functions to visualize a match), and found out a lot of cool things (set by set), but I am seeking a bit of direction

# 2a. margin visualizations - normal vs serve adjusted

# 2b. derivative graph, ran into issues

# 3. Change point - demonstrate, mean vs var, method (bayesian), idea to extract change points and assess, problems- like to incorporate other variables

# 4. Difficulty with Hawkes Process - runs, display visualization, all in function form

# 5. General walk through of paper

# 6. Next steps (when to meet and what to take on next)... derivative graph, one match vs all?, dividing, descriptive background, literature review
# we need to find covariates. Some of my ideas: new set, break of serve, holding after saving break point, win tiebreak, serving for set or to stay in set
# some players are more volatile?

# 7. Grad School - letter of recommendation and school list



# Meeting with Dr. Sturdivant 12/3 ----------------------------------------

# two research questions
# 1. does momentum impact future - i.e does it matter? How much? Minorly, does momentum exist?
# backward differentiation with logistic model to predict winner of next point

# 2. what causes momentum?
# options:
# first,
# use complete differentiation (spline), because we want an instantaneous assessment of derivative?

# What we want...
# do covariates at point t-n (probably n = 1 or 2) impact momentum at point t.
# This will help us to determine if certain covariates "cause" momentum

# However, with both backward differentiation or splines, I am skeptical that this is unbiased
# In both derivative estimating methods, the derivative itself is impacted by the previous results.
# If we are estimating effect of breaking or saving a break point, or winning a set, etc. in
# point t-1 on derivative in point t, there will be an artificial impact, because winning any point in t-1
# (regardless of situation) directly increases our derivative in t (see visualizing derivative)

# My idea, and I think this fits with what we want
# we evaluate covariates on point t-1 on derivative at point t using a forward differentiation method
# We'd have to flip the exponential smoothing and find the forward differentiation,
# but that wouldn't take very long
# What it would do: We'd have covariates of point t-1 and we'd test how this
# impacts a players' future success at point t.

# My biggest caveat:
# we may need to find a way to adjust for the serve, because the future momentum
# could be higher or lower depending on who is serving the next few points.
# I think we would need to adjust by increasing value of winning return points (by sex, tournament, and year)
# this wouldn't be perfect and I think a future method could be to adjust further with a model that predicts
# the serve win percentage in a match based on players pre-match stats
# second way to adjust would be to add serve as a covariate of point t to predict the derivative at point t.
# Hopefully, this serve coviariate is not statistically significant.
# This would show that we accounted for it correctly in the first adjustment

# Question about writing: I or we
# also, variables: bold? in exact form?

