# Who'w Making News
These R Shiny scripts and re-formatted data files are in support the amazing project, Who's Making News: https://www.whoismakingnews.com/

The initial intent of these scripts was to (1) teach myself Shiny, and (2) make some interesting analyses/visualizations for looking at data and support the cause.  On the Shiny page, I bring in other data sources and work through the following analysis methods:

1. Linear regression to predict today's crime rate.
2. Time Series decomposition to predict today's crime rate.
3. Exponential Smoothing to predict today's crime rate.
4. Group by category and sub-category for data exploration.
5. Breakdown of criminals by gendered name into "male" names, "female" names, and uncommon names (to play with word clouds).
6. Compare crime rates by state in terms of parameters such as demographics, employment, policy and politics by linear regression.
7. Compare relative crime rates by state to other crime statistics using Principal Component and K-means Clustering.

Other standard analyses of these data could be found elsewhere (rates per capita, etc.), but what struck me as interesting after a quick look through the data was the preponderance of male names associated with sex crimes against children.  I wanted to be able to emphasize this information and play with how to best represent this.
