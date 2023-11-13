library(shiny)
library(tidyverse)
#library(ggplot2)
library(ggmosaic)
library(DT)
#library(rdrop2)
#library(googledrive)
library(gargle)
library(fable)
library(tsibble)

# for wordcloud
library(RColorBrewer)
#library(wordcloud)
library(wordcloud2)
library(NLP)
library(tm)
library(stringr)
library(packcircles)

# pca stuff
library(ggfortify)
library(cluster)
library(factoextra) # clustering algorithms & visualization

# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = TRUE,
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = "/.secrets"
# )

conditionalpanel_style_controls = paste("background: #EEEEEE", "padding: 15px", sep=";")
conditionalpanel_style_text = paste("background: #FFFFEE", "padding: 15px", sep=";")
wellpanel_style = "background: #FFFFEE"
detail_style_text = paste("background: #EEEEEE", "padding: 15px", sep=";")
HR_style = "border-top: 2px solid black;"

##################################################

# if an error occurs, try to do it gracefully
css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
  visibility: visible;
  content: 'An error occurred! Please contact the admin.'; }
}
"

ui <- fluidPage(
  # HTML('<link rel="icon", href="favicon.png", type="image/png">'
  # ),
  
  list(
    tags$head(
      tags$style(
        HTML(" 
        .navbar-default .navbar-brand {color: #FFFFDD;}
        .navbar-default .navbar-brand {color: #FFFFDD;}
        .navbar-default .navbar-nav > li > a:hover {color: #FFFFDD;text-decoration:underline;}
             ")
      ),
    )
  ),
  
  navbarPage(title = "Exploring the Data: Who's Making News", 
             inverse = TRUE,
             
             tabPanel("The Last 7 days",
                      fluidRow(
                        column(4,
                               wellPanel(
                                 h4(HTML("<b>Exploring the Data:</b> Who's Making News<br /><span><font size=-0.5>for Sex Crimes Involving Children?</font></span>")),
                                 h4(HTML("Data from: <a href='https://www.whoismakingnews.com/', target='_blank'>www.whoismakingnews.com</a>")),
                                 h4(htmlOutput("lastdate"))
                                 #h5(HTML("<b>Note:</b> As of 10 AM 11/4/23, WMN LOST data availability functionality. They are busily fixing the issue and new data will be presented here when they resolve the issue."))
                               ),
                                 wellPanel(style = wellpanel_style, 
                                           h4(HTML("Here you will find some methods of data exploration for the 'Who's Making News' data set, from the amazing work done by Kristen Browde.<br /><br />All code used here and additional data files (see individual tabs) are available here: <a href='https://github.com/egraham/WMN', target='_blank'>github.com/egraham/WMN</a>")),
                                           #h4(HTML("Direct link to the Shinyapps hosting site: <a href='https://erksome.shinyapps.io/WMN_Analysis/', target='_blank'>erksome.shinyapps.io/WMN_Analysis</a>")),
                                           
                                 ),
                               wellPanel(
                                 checkboxGroupInput("includebubble", "Include in the plot:", 
                                                    choiceNames = list("'Not Listed'"),
                                                    choiceValues =list("include_notlistedbubble")
                                 )
                               ),
                                 h5(style = detail_style_text, HTML("The motivation for this page is to create some interactive data exploration methods and alternative visualizations.  It also includes some simple supervised and unsupervised machine learning models. I thought it would be fun to also combine the data with other online sources.<br /><br />More thorough analyses of these data can be found elsewhere (for example, per capita analysis here: DataViz.fyi).")),
                                 h5(style = detail_style_text, HTML("I also clean up the data for some of these analyses, rather than leaving it completelly 'as-is'.  For example, for the 'genderized' names, I remove unnamed values and I clean up some typos ('babsyitter' for babysitter') and such.<br /><br />Thus, the totals reported here may not be exactly the same as reported on the WMN website, but they are very close.")),
                                 h5(style = detail_style_text, HTML("This is a work in progress.  If you see any errors and care to let me know, drop me a line: 'egraham.cens' at gmail.  Thanks!"))
                               ),
                               column(8,
                                      plotOutput(
                                        outputId = "weekbubble", width = "100%", height = 600,
                                        hover = hoverOpts(
                                          id = "bubbleplot_hover",
                                          delay = 200,
                                          delayType = "debounce",
                                          clip = TRUE,
                                          nullOutside = TRUE
                                        )
                                      ),
                                      div(style = "position:relative",
                                          uiOutput("bubbleplot_info", style = "pointer-events: none")
                                      ),
                                      h5(style = detail_style_text, HTML("Above is a basic circle packing plot (a circle version of a treemap), with the size of the circle depending on the number of crimes and colors are based on larger groups (see the 'Groups of People' tab).")
                                      ),
                                      h5(style = detail_style_text, HTML("Hover your mouse over the bubbles for more info (found in the 'Relation' category of the data set; <u>Note</u>: not all entries haved information stored in 'Relation'). The data are presented 'as-is', with no filtering or cleaning up.")
                                      )
                               )
                        )
                      ),
                      
                      ################################################
                      tabPanel("Predicting Today",
                               fluidRow(
                                 column(4,
                                        wellPanel(
                                          radioButtons("modelselecter", "Pick the model used for prediction:",
                                                       c("Linear regression" = "linearmodel",
                                                         "Day of the week (ETS)" = "daymodel",
                                                         "Time series decomposition" = "decompmodel"
                                                       ), 
                                                       selected = "linearmodel", inline = FALSE),
                                        ),
                                        wellPanel(style = wellpanel_style, 
                                                  h4("Predicting trends or when the next crime will occur is always strange (betting on human behavior), but we can use some simple tools to guess at how things are going."),
                                                  
                                        ),
                                        conditionalPanel(condition="input.modelselecter == 'linearmodel'",
                                                         h5(style = detail_style_text, HTML("There are more sophisticated ways to model data, but here we start with a simple line drawn through the last month of data and extend it one day into the future (today)."))),
                                        conditionalPanel(condition="input.modelselecter == 'daymodel'", 
                                                         h5(style = detail_style_text, HTML("Another way we can do it is to apply some time series forecast modeling of the time series (ETS, Exponential Smoothing) to predict future values."))),
                                        conditionalPanel(condition="input.modelselecter == 'decompmodel'",
                                                         h5(style = detail_style_text, HTML("And we can also do a time series decomposition (see next tab, 'About That Line', for more info on time series decomposition) and model the week days as regularly repeating.")))
                                 ),
                                 column(8,
                                        conditionalPanel(condition="input.modelselecter == 'linearmodel'",
                                                         plotOutput(
                                                           outputId = "plotcumsum", width = "90%", height = 500,
                                                         ),
                                                         h5(htmlOutput("cumsumlineartext"), style=detail_style_text)
                                        ),
                                        conditionalPanel(condition="input.modelselecter == 'daymodel'",
                                                         plotOutput(
                                                           outputId = "ets", width = "90%", height = 500,
                                                         ),
                                                         h5(style = detail_style_text, HTML("Predicted crimes using ETS time series forecast modeling, with black lines as the last month's data and red is the future prediction. Here, ETS computes a weighted average over the last 30 days of observations.<br /><br />(no hover data on this plot)"))
                                        ),
                                        conditionalPanel(condition="input.modelselecter == 'decompmodel'",
                                                         plotOutput(
                                                           outputId = "decomp", width = "90%", height = 500,
                                                         ),
                                                         h5(style = detail_style_text, HTML("Predicted crimes using time series decomposition, with a black line as the day of the week data and red is the prediction for today. The 'Seasonal' component after decomposition and the mean 'Trend' values for the last month were used.<br /><br />(no hover data on this plot)"))
                                        )
                                 )
                               )
                      ),
                      ################################################
                      tabPanel("About That Line",
                               fluidRow(
                                 column(4,
                                        wellPanel(
                                          radioButtons("dataselecter", "Pick your output:",
                                                       c("Raw data" = "rawdata",
                                                         "Time series" = "timeseries"
                                                       ), 
                                                       selected = "rawdata", inline = FALSE)
                                        ),
                                        wellPanel(style = wellpanel_style, 
                                                  h4("The data collected for 'Who's Making News' has some interesting daily and longer-term trends.")
                                        ),
                                        conditionalPanel(condition="input.dataselecter == 'rawdata'",
                                                         h5(style = detail_style_text, HTML("There is an obvious cycle to the data, even as the maxiumum (and minimum) number of events per week grows.<br /><br />Select 'time series' above for an analysis of this regular pattern."))
                                        ),
                                        conditionalPanel(condition="input.dataselecter == 'timeseries'",
                                                         h5(style = detail_style_text, HTML("Data in the 'Observed' plot at the top are the raw data.<br /><br />The 'Seasonal' plot shows a highly regular periodicity to the data, with fewer crimes <b>reported</b> on Sundays.<br /><br />Are criminals taking the day off?"))
                                        )
                                        
                                 ),
                                 column(8,
                                        conditionalPanel(condition="input.dataselecter == 'rawdata'",
                                                         plotOutput(
                                                           outputId = "linedataraw", 
                                                           width = "90%", 
                                                           height = 500,
                                                           dblclick = "plot_dblclick",
                                                           brush = brushOpts(
                                                             id = "zoom_brush", 
                                                             resetOnNew = TRUE, 
                                                             delay=500, 
                                                             delayType = "debounce"),
                                                           # tooltips
                                                           hover = hoverOpts(
                                                             id = "lineplot_hover",
                                                             delay = 200,
                                                             delayType = "debounce",
                                                             clip = TRUE,
                                                             nullOutside = TRUE
                                                           )
                                                         ),
                                                         div(style = "position:relative",
                                                             uiOutput("lineplot_info", style = "pointer-events: none")
                                                         ),
                                                         h5(style = detail_style_text, "Select a range by dragging your mouse, then double-click for a more detailed view and hover information.  Double-click without a selection to return to full-scale."),
                                                         h5(style = detail_style_text, "Data have not been cleaned up, thus names displayed on hovering may not be 'real' names.")
                                        ),
                                        conditionalPanel(condition="input.dataselecter == 'timeseries'",
                                                         plotOutput(
                                                           outputId = "timeseries", width = "90%", height = 500,
                                                         ),
                                                         h5(style = detail_style_text, HTML("Time series analysis also shows the increased rate of crime (the 'Trend' row) where some sort of change in data collection occurred (a bigger web scrape?). As of 10-15, even more cases are being included (Indiana and Florida, among other states, were using variations on terminology not captured prevoiusly).<br /><br /> (no zooming in on this plot)"))
                                        )
                                 )
                               )
                      ),
                      
                      ################################################
                      tabPanel("Groups of People",
                               fluidRow(
                                 column(4, 
                                        wellPanel(
                                          checkboxGroupInput("include", "Include in the plot:", 
                                                             choiceNames = list("'Not Listed'", "Trans people","Drag Queens"),
                                                             choiceValues =list("include_notlisted", "include_trans", "include_dragqueen"),
                                          )
                                        ),
                                        wellPanel(style = wellpanel_style, 
                                                  h4("This is the standard way to show off the groups data.")
                                        ),
                                        h5(style = detail_style_text, "People who are trans are highlighted in the categories where they are included, not counted separately from their group.")
                                 ),
                                 column(8,
                                        plotOutput(
                                          outputId = "plot1", width = "90%", height = 500,
                                          # tooltips
                                          hover = hoverOpts(
                                            id = "plot_hover_bar",
                                            delay = 200,
                                            delayType = "debounce",
                                            clip = TRUE,
                                            nullOutside = TRUE
                                          )
                                        ),
                                        div(style = "position:relative",
                                            uiOutput("hover_info_bar", style = "pointer-events: none")
                                        ),
                                        h5(style = detail_style_text, "Hover over the groups for more information.  Group percentages are based on the inclusion or exclusion of the 'Not Listed' group."),
                                 )
                               )
                      ),
                      
                      ################################################
                      tabPanel("Names and Genders",
                               fluidRow(
                                 column(4,
                                        wellPanel(checkboxGroupInput("gender", "First names of those who have committed crimes against children:", 
                                                                     choiceNames = list("Typically Female names", "Typically Male names", "Uncommon names"),
                                                                     choiceValues = list("gender_female", "gender_male", "gender_atypical"),
                                                                     selected = "gender_male"
                                        ),
                                        radioButtons("genderselecter", "Pick your output:",
                                                     c("Word cloud" = "word_cloud",
                                                       "Pie chart" = "piegender",
                                                       "Data table" = "data_table"), 
                                                     selected = "word_cloud", inline = TRUE)
                                        ),
                                        conditionalPanel(condition="input.genderselecter == 'word_cloud'",
                                                         wellPanel(
                                                           sliderInput("decimal", "Wordcloud size:",
                                                                       min = 0.1, max = 2,
                                                                       value = 0.5, step = 0.1),
                                                           p("Adjust the slider bar to be able to better visualize the names.")
                                                         ),
                                                         wellPanel(style = wellpanel_style,
                                                                   h4(HTML("Word clouds have an appeal that is hard to deny, although word clouds are not great for analytical accuracy.<br /><br />Data from 'https://genderize.io' are included."))
                                                         ),
                                                         h5(style = detail_style_text, "Unfortunately Wordcloud2 will also not print names that 'don't fit' and so as you play with the slider, some of the most common names may dissappear, or the colors assigned to names will become un-synchronized with their gender.")
                                        ),
                                        conditionalPanel(condition="input.genderselecter == 'piegender'",
                                                         wellPanel(style = wellpanel_style,
                                                                   h4(HTML("Pie charts aren't much better than word clouds, but they can indicate some obvious trends.<br /><br />Data from 'https://genderize.io' are included."))
                                                         )
                                        ),
                                        
                                        conditionalPanel(condition="input.genderselecter == 'data_table'", 
                                                         wellPanel(style = wellpanel_style,
                                                                   h4(HTML("Sort by 'First Name' to see how common a name is in the WMN crime set (versus sort by how common the name is in the US - they are not the same! - data from 'https://genderize.io'.)<br /><br />... or Search for your own name in the list.")
                                                                   )
                                                         ),
                                                         h5(style = detail_style_text, HTML("<b>'Crimes'</b> indicates how many events were recorded under that name in the 'Who's Making News' data set.<br /><b>'Percent from WMN'</b> indicates percent of crimes under that name from the total for that gender.<br /><b>'US Proportion'</b> indicates the frequency that the name occurs in the Genderize database (assumed to be the current frequency in the US).")
                                                         )
                                        )
                                 ),
                                 column(8, 
                                        conditionalPanel(condition="input.genderselecter == 'word_cloud'",
                                                         wordcloud2Output(
                                                           outputId = "wordcloud"
                                                         ),
                                                         div(style = "position:relative",
                                                             uiOutput("hover_wordinfo", style = "pointer-events: none")),
                                                         p(" "),
                                                         h5(style = detail_style_text, "Notes: I extraced the first names of the sex offenders from the WMN database, assuming one space between first and last name and that everyone has a first name (not true!).  These names are sent in batches to the 'https://genderize.io/' database server, where they are compared to a database of 'known' genders in the US and the probabilities of genders with each name is returned.  Obviously, there will be errors and names are not always exactly correlated with sex, gender, or expression of those concepts."),
                                                         p(" "),
                                                         h5(style = detail_style_text, "Entries such as might occur for first names in the WMN database like 'FL Sting Pasco', '18 men', or 'unnamed parent' will return first names that will not return a reasonable result in the genderize.io database (and I remove those for this analysis). Actual names like 'La Luz' may also not result in usable data (but I do not specifically remove these types of names).  Thus, these data are a subset of the bigger Who's Making News data set and should be considered a random (??) sample."),
                                        ),
                                        conditionalPanel(condition="input.genderselecter == 'piegender'",
                                                         plotOutput(
                                                           outputId = "pie1",
                                                           # tooltips
                                                           hover = hoverOpts(
                                                             id = "pie_hover",
                                                             delay = 200,
                                                             delayType = "debounce",
                                                             clip = TRUE,
                                                             nullOutside = TRUE
                                                           )
                                                         ),
                                                         h5("Using all data (not from the selection on the left). An apparent, obvious trend.", style=detail_style_text),
                                                         div(style = "position:relative",
                                                             uiOutput("hover_pieinfo", style = "pointer-events: none")
                                                         )
                                        ),
                                        conditionalPanel(condition="input.genderselecter == 'data_table'",
                                                         conditionalPanel(condition = "input.gender.indexOf('gender_male') > -1",
                                                                          DT::dataTableOutput("malenamesTable"),
                                                                          h5(HTML("&nbsp;")),
                                                                          p(style = HR_style),
                                                                          h5(HTML("&nbsp;"))
                                                         ),
                                                         conditionalPanel(condition = "input.gender.indexOf('gender_female') > -1",
                                                                          DT::dataTableOutput("femalenamesTable"),
                                                                          h5(HTML("&nbsp;")),
                                                                          p(style = HR_style),
                                                                          h5(HTML("&nbsp;"))
                                                         ),
                                                         conditionalPanel(condition = "input.gender.indexOf('gender_atypical') > -1",
                                                                          DT::dataTableOutput("atypicalnamesTable"),
                                                                          h5(HTML("&nbsp;")),
                                                                          p(style = HR_style),
                                                                          h5(HTML("&nbsp;"))
                                                         )
                                        )
                                 )
                               )
                      ),
                      
                      ################################################
                      tabPanel("States of People",
                               fluidRow(
                                 column(4,
                                        wellPanel(
                                          selectInput(
                                            inputId = "states",
                                            label = HTML("An analysis using some state statistics.<br />Sort states by:"),
                                            choices = list(
                                              "Demographics" = c("Crimes Against Children"="by_crimes", "Alphabetical"="alpha", "Percent of Males"="pct_males", "Percent Adults with No Children"="pct_no_childs", "Percent Who Voted"="pct_voted"),
                                              "Income and employment" = c("Median Annual Income"="median_income","Percent Below the Federal Poverty Level"="pct_below_FPL","Percent Part-Time Workers"="pct_part-time", "Percent Unemployed"="pct-unemployed"),
                                              "Politics and money" = c("Political Party"="political", "Gross State Product"="gsp", "Per Capita Spending"="per_capita_spending", "Spending on 1° & 2° Education"="pcs_edu", "Spending on Assistance"="pcs_assistance","Spending on Corrections"="pcs_corrections")
                                            )
                                          )
                                        ),
                                        conditionalPanel(condition = "input.states == 'by_crimes'",
                                                         wellPanel(style = wellpanel_style,
                                                                   h4(HTML("Crime data is expressed as a percentage of the difference between observed and expected crimes, relative to the expected based on the population.<br /><br />Data from 'www.kff.org' included."))
                                                         ),
                                                         h5(style = detail_style_text, HTML("Positive percentages indicate higher than expected crime rate percentage (e.g., +100% is twice the number of expected crimes), negative are lower. Expected crime numbers are calculated as an average US per capita rate, adjusted for the state's population.<br /><br />Data for the lollipop plot are sorted such that highest child sex crime rates are always nearest the vertical axis.<br /><br />The data used here is a subset of the larger data set by KFF and many of these variables are not independent from each other and/or are strongly correlated!")
                                                         )
                                        ),
                                        conditionalPanel(condition = "input.states != 'by_crimes'",
                                                         wellPanel(style = wellpanel_style,
                                                                   h4(HTML("Examine the figures below to determine if assumptions have been violated. (for example, choose 'Political Party')<br /><br />Data from 'www.kff.org' included."))
                                                         ),
                                                         plotOutput(height = "250px",
                                                                    outputId = "scatterbyState"
                                                         ),
                                                         plotOutput(height = "250px",
                                                                    outputId = "q_q"
                                                         )
                                        )
                                 ),
                                 column(8, 
                                        plotOutput(
                                          outputId = "byState",
                                          # tooltips
                                          hover = hoverOpts(
                                            id = "state_hover",
                                            delay = 200,
                                            delayType = "debounce",
                                            clip = TRUE,
                                            nullOutside = TRUE
                                          )
                                        ),
                                        div(style = "position:relative",
                                            uiOutput("hover_stateinfo", style = "pointer-events: none")
                                        ),
                                        h4(htmlOutput("statetext1"), style=detail_style_text),
                                        h5(htmlOutput("statetext2"), style=detail_style_text)
                                 )
                               )
                      ),
                      ################################################
                      tabPanel("Crime and Punishment",
                               fluidRow(
                                 column(4,
                                        wellPanel(h4("Select the variables to include in the plot."),
                                                  checkboxGroupInput("crime", "State statistics on per 100,000 people crime rates:", 
                                                                     choiceNames = list("Burglary","Larceny","Motor Vehicles", "Assault", "Murder", "Rape", "Robbery"),
                                                                     choiceValues = list("Burglary_rate", "Larceny_rate", "Motor_rate", "Assault_rate", "Murder_rate", "Rape_rate", "Robbery_rate"),
                                                                     selected = c("Burglary_rate", "Larceny_rate"), inline = TRUE
                                                  ),
                                                  checkboxGroupInput("punishment", "State statistics on per capita punishment rates:", 
                                                                     choiceNames = list("State Prisons","Federal Prisons","Local Jails", "Youth Facilities", "Involuntary", "Probation", "Parole"),
                                                                     choiceValues = list("State_Prisons_rate", "Federal_Prisons_rate", "Local_Jails_rate", "Youth_Facilities_rate", "Involuntary_Commitment_rate", "Probation_rate", "Parole_rate"),
                                                                     selected = "State_Prisons_rate", inline = TRUE
                                                  ),
                                                  radioButtons("cpselecter", "Pick your output:",
                                                               c("Principal Component" = "pca", "K-Means" = "kmeans"), 
                                                               selected = "pca", inline = TRUE
                                                  ),
                                                  checkboxInput("include_exp", "Include the Percent Difference from Expected Crime", value = TRUE
                                                  ),
                                                  conditionalPanel(condition="input.cpselecter == 'kmeans'",
                                                                   numericInput(
                                                                     "kmeansnum",
                                                                     "Number of Groups for K-Means",
                                                                     3,
                                                                     min = 1,
                                                                     max = 8,
                                                                     width = '75%'
                                                                   )
                                                  )
                                        ),
                                        conditionalPanel(condition="input.cpselecter == 'pca'",
                                                         wellPanel(style = wellpanel_style,
                                                                   h4(HTML("Principal Component Analysis is an 'unsupervised machine learning' method that shows what variables are responsible for the most variation in a multi-variable data set.<br /><br />Data from 'ucr.fbi.gov' and 'www.prisonpolicy.org' are included."))
                                                         ),
                                                         h5(style = detail_style_text, HTML("Only the first two components are shown. When comparing the data to 'the Percent Difference from Expected Crime', the arrows (loadings) indicate how the selected data are related to 'Crime' arrow.<br /><br />An arrow in the same direction as the 'Crime' arrow indicates an increase in that variable is associated with an increase in crime.  An arrow perpendicular (or not well-represented in the component) indicates not much correlation."))
                                        ),
                                        conditionalPanel(condition="input.cpselecter == 'kmeans'",
                                                         wellPanel(style = wellpanel_style,
                                                                   h4(HTML("K-Means clustering is also an 'unsupervised machine learning' method (except you select how many clusters, so 'semi-supervised'?). It divides data into clusters that share similarities and that are dissimilar to the data in other clusters.<br /><br />Data from 'ucr.fbi.gov' and 'www.prisonpolicy.org' are included."))
                                                         ),
                                                         h5(style = detail_style_text, "To calculate the clusters, usually a PCA is performed first.  Then for every data point, the distance is measured from the selected number of 'centroids' (means) chosen.  Centroids are then moved and the process continues until the distances are optimized. K-means is sensitive to initial values and outliers, so examine the clusters carefully.")
                                        )
                                 ),
                                 column(8, 
                                        conditionalPanel(condition="input.cpselecter == 'pca'",
                                                         h4(htmlOutput("pcatext")),
                                                         plotOutput(
                                                           outputId = "pcaplot"
                                                         ),
                                                         h4(htmlOutput("pcatext1"), style=detail_style_text),
                                                         column(6,
                                                                h5(htmlOutput("pcatext2"), style=detail_style_text)
                                                         ),
                                                         column(6, 
                                                                plotOutput(
                                                                  outputId = "pcacontributionspine"
                                                                )
                                                         )
                                        ),
                                        conditionalPanel(condition="input.cpselecter == 'kmeans'",
                                                         h4(htmlOutput("kmeanstext")),
                                                         plotOutput(
                                                           outputId = "kmeansplot"
                                                         ),
                                                         h4(htmlOutput("kmeanstext1"), style=detail_style_text),
                                                         DT::dataTableOutput("kmeansTable"),
                                                         column(6,
                                                                h5(htmlOutput("kmeanstext2"), style=detail_style_text)
                                                         ),
                                                         column(6, 
                                                                plotOutput(
                                                                  outputId = "kmeansSilhouette"
                                                                )
                                                         )
                                        )
                                 )
                               )
                      )
                      
             )
  )


server <- shinyServer(function(input,output, session)({
  
  ####### Some Functions #######
  
  # Function to extract overall p-value of model
  sorted_p <- function(state.lm) {
    f <- summary(state.lm)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  }
  
  ####### set up session and get data files
  session$onSessionEnded(stopApp) # kill the shiny app when the window is closed
  
  wmn_data_previous <- readRDS("wmn_data_previous.RDS")
  wmn_names_genderized <- readRDS("wmn_names_genderized.RDS")
  
  # static files
  KFF_data <- readRDS("KFF_data.RDS")
  state_crime <- readRDS("state_crime.RDS")
  state_punishment <- readRDS("state_punishment.RDS")
  
  # From the Unified Crime Reporting Statistics and under the collaboration of the U.S. Department of Justice 
  #   and the Federal Bureau of Investigation information crime statistics are available for public review. 
  #   The following data set has information on the crime rates and totals for states across the United States 
  #   for a wide range of years. The crime reports are divided into two main categories: property and violent crime. 
  #   Property crime refers to burglary, larceny, and motor related crime while violent crime refers to assault, murder, rape, and robbery. 
  #   These reports go from 1960 to 2019.
  
  # https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/downloads/download-printable-files
  #state_crime <- read_csv("state_crime.csv")
  
  # The non-profit, non-partisan Prison Policy Initiative produces cutting edge research to expose the broader harm of mass criminalization,
  #   and then sparks advocacy campaigns to create a more just society.
  # https://www.prisonpolicy.org/reports/correctionalcontrol2023_data_appendix.html
  # punishment rates per 100,000
  #state_punishment <- read_csv("state_punishment.csv")
  
  lastdate <- file.info("wmn_data_previous.RDS")$mtime  
  lastdate <- format(as.Date(lastdate), "%b %d, %Y")
  output$lastdate <- renderText({
    paste("Complete as of: ", lastdate, sep="")
  })
  
  ##################### data processing ####################################
  
  numcrimes <- max(row(wmn_data_previous))
  
  conditionalpanel_style_controls = paste("background: #EEEEEE", "padding: 15px", sep=";")
  conditionalpanel_style_text = paste("background: #FFFFEE", "padding: 15px", sep=";")
  wellpanel_style = "background: #FFFFEE"
  detail_style_text = paste("background: #EEEEEE", "padding: 15px", sep=";")
  
  # count numbers in categories
  category_counts <- wmn_names_genderized |>
    group_by(Category) |>
    summarize(Category_n = n()) |>
    arrange(desc(Category_n))
  
  # Recombine data set to include the broad categories with counts
  names_category_counts <- inner_join(wmn_names_genderized, category_counts, by="Category")
  
  names_category_counts <- names_category_counts |>
    mutate(
      big_category = case_when(
        Category %in% c("Teachers/Aides","Coaches", "Day Care/Babysitters") ~ "Teachers",
        Category %in% c("Pastors", "Priests/Brothers", "Church Employees", "Missionaries", "Mormon Leaders") ~ "Religious Employment",
        Category %in% c("Family Members", "Family Friends/Neighbors") ~ "Family/Known",
        Category == "Other" ~ "Other",
        Category == "Politicians" ~ "Politicians",
        Category == "Doctors" ~ "Doctors",
        Category == "Police" ~ "Police",
        Category == "Not Listed" ~ "Not Listed"
      )
    ) |>
    filter(!is.na(big_category))
  
  # Get big category counts for stats in tooltips
  big_category_counts <- names_category_counts |>
    group_by(big_category) |>
    summarize(big_category_n = n()) |>
    arrange(desc(big_category_n))
  
  total_count <- big_category_counts |>
    summarize(total = sum(big_category_n))
  
  named_total_count <- big_category_counts |>
    filter(big_category != "Not Listed") |>
    summarize(total = sum(big_category_n))
  
  # Get numbers for stacked graph, each category with big categories redundant, long
  stacked_barplot_numbers <- inner_join(names_category_counts, big_category_counts, by = "big_category") |>
    distinct(Category, .keep_all = TRUE) |>
    dplyr::select(Category, Category_n, big_category, big_category_n) |>
    arrange(desc(big_category_n), desc(Category_n))
  
  # make cumulative sums of the Category subsets to make tooltips easier to calculate 
  stacked_barplot_numbers <- stacked_barplot_numbers |> 
    group_by(big_category) |> 
    mutate(csum = cumsum(Category_n))
  
  # Give names_category_counts the big category counts
  names_category_counts <-
    left_join(names_category_counts, big_category_counts, by = "big_category")
  
  #### Process Trans and Drag Queens to put them on the chart as add-on lines
  #   in the categories of jobs where they belong as people
  
  # Get how many trans and what categories
  trans <- names_category_counts |>
    filter(Trans) |>
    left_join(stacked_barplot_numbers, by = "Category") |>
    dplyr::select(Category, big_category.x) |>
    rename('big_category' = big_category.x) |>
    mutate(barrank = which(big_category_counts == big_category))
  
  # Get y-axis value (the max) from the stacked bar plot
  trans <- inner_join(trans, stacked_barplot_numbers, by="Category") |>
    dplyr::select(Category, big_category.x, barrank, Category_n, csum) |>
    rename('big_category' = big_category.x)
  
  # Get locations within the small category (Category) to place the lines, per person
  # find number of trans people per category
  trans_groups <- trans |>
    group_by(Category, big_category, barrank, Category_n, csum) |>
    summarize(.groups = "keep", trans_n = n())
  
  # Get how many drag queens and what categories
  dqueens <- names_category_counts |>
    filter(DQueen) |>
    left_join(stacked_barplot_numbers, by = "Category") |>
    dplyr::select(Category, big_category.x) |>
    rename('big_category' = big_category.x)
  
  dqueens <- dqueens |>
    group_by(Category, big_category) |>
    summarize(.groups = "keep", Category_n = n())
  
  ######################## Last 7 days bubble plot ###############################
  
  # get the last 7 days of data (not necessarily from today)
  wmn_data_previous_week <- wmn_data_previous |> 
    filter(Date >= max(Date)-days(7)) |> 
    mutate(
      category_color = case_when(
        Category == "Teachers/Aides" ~ "#7A4419",
        Category =="Coaches" ~ "#755C1B",
        Category =="Day Care/Babysitters" ~ "#D7BE82",
        Category == "Pastors" ~ "#FF6340",
        Category =="Priests/Brothers" ~ "#F5365F",
        Category =="Church Employees" ~ "#FFBC40",
        Category =="Missionaries" ~ "#E88C46",
        Category =="Mormon Leaders" ~ "#F5D22F",
        Category == "Family Members" ~ "#697A29",
        Category =="Family Friends/Neighbors" ~ "#B8B42D",
        Category == "Other" ~ "#888888",
        Category == "Politicians" ~ "#990000",
        Category == "Doctors" ~ "#009900",
        Category == "Police" ~ "#1C77C3",
        Category == "Not Listed" ~ "#FFFFFF"
      )
    ) |> 
    mutate(
      text_color = case_when(
        Category == "Teachers/Aides" ~ "#FFFFFF",
        Category =="Coaches" ~ "#FFFFFF",
        Category =="Day Care/Babysitters" ~ "#000000",
        Category == "Pastors" ~ "#000000",
        Category =="Priests/Brothers" ~ "#000000",
        Category =="Church Employees" ~ "#000000",
        Category =="Missionaries" ~ "#000000",
        Category =="Mormon Leaders" ~ "#000000",
        Category == "Family Members" ~ "#FFFFFF",
        Category =="Family Friends/Neighbors" ~ "#000000",
        Category == "Other" ~ "#FFFFFF",
        Category == "Politicians" ~ "#FFFFFF",
        Category == "Doctors" ~ "#000000",
        Category == "Police" ~ "#FFFFFF",
        Category == "Not Listed" ~ "#000000"
      )
    )
  
  numcrimesweek <- max(row(wmn_data_previous_week))
  numcrimesweektext <- paste(numcrimesweek, " new crimes added for a total of ", numcrimes, sep="")
  
  # count numbers in small categories
  category_counts_bubble <- wmn_data_previous_week |> 
    group_by(Category) |>
    mutate(Category_n = n()) |>
    select(Category, Relation, Category_n, category_color, text_color)
  
  big_category_counts_bubble <- category_counts_bubble |>
    mutate(
      big_category = case_when(
        Category %in% c("Teachers/Aides","Coaches", "Day Care/Babysitters") ~ "Teachers",
        Category %in% c("Pastors", "Priests/Brothers", "Church Employees", "Missionaries", "Mormon Leaders") ~ "Religious Employment",
        Category %in% c("Family Members", "Family Friends/Neighbors") ~ "Family/Known",
        Category == "Other" ~ "Other",
        Category == "Politicians" ~ "Politicians",
        Category == "Doctors" ~ "Doctors",
        Category == "Police" ~ "Police",
        Category == "Not Listed" ~ "Not Listed"
      )
    ) |>
    mutate(Category = str_replace(Category, "Teachers/Aides", "Teachers\nAides")) |> 
    mutate(Category = str_replace(Category, "Day Care/Babysitters", "Day Care\nBabysitters")) |> 
    mutate(Category = str_replace(Category, "Priests/Brothers", "Priests\nBrothers")) |> 
    mutate(Category = str_replace(Category, "Family Friends/Neighbors", "Friends\nNeighbors")) |> 
    mutate(Category = str_replace(Category, "Family Members", "Family\nMembers")) |> 
    mutate(Category = str_replace(Category, "Church Employees", "Church\nEmployees")) |> 
    filter(!is.na(big_category)) |> 
    group_by(big_category) |> 
    mutate(big_category_n = n())

  # Make the plot
  
  output$weekbubble <- renderPlot({
    
    # if include the "not listed
    if(length(str_subset(input$includebubble, "include_notlistedbubble")) > 0) {
      big_category_counts_summary <- big_category_counts_bubble |> 
        distinct(Category, .keep_all = TRUE)
      # reduce size of text in plot
      range_b <- c(3,6)
    } else {
      big_category_counts_summary <- big_category_counts_bubble |> 
        distinct(Category, .keep_all = TRUE)
      big_category_counts_summary <- big_category_counts_summary |> 
        filter(Category != "Not Listed")
      range_b <- c(4,7)
    }
    
    # Create data, group by big categories, sort by size and alpha (in case of size ties)
    weekdata <- data.frame(group=paste(big_category_counts_summary$Category_n, big_category_counts_summary$Category,sep="\n"), 
                           value=big_category_counts_summary$Category_n, 
                           category_color=big_category_counts_summary$category_color, 
                           text_color=big_category_counts_summary$text_color,
                           hovertextinfo=big_category_counts_summary$Category) |> 
      arrange(desc(big_category_counts_summary$Category_n), big_category_counts_summary$Category)
    
    # Generate the layout. This function return a dataframe with one line per bubble. 
    # It gives its center (x and y) and its radius, proportional of the value
    packing <- circleProgressiveLayout(weekdata$value, sizetype='area')
    
    # We can add these packing information to the initial data frame
    weekdata_p <- cbind(weekdata, packing)
    
    # Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
    # plot(data$radius, data$value)
    
    # The next step is to go from one center + a radius to the coordinates of a circle that
    # is drawn by a multitude of straight lines.
    dat.gg <- circleLayoutVertices(packing, npoints=50) |> 
      mutate(category_color = weekdata_p$category_color[id]) |> 
      mutate(text_color = weekdata_p$text_color[id])
    
    
    
    # Make the plot
    bubbleplot <- ggplot() + 
      
      # Make the bubbles
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), 
                   colour = "black", 
                   fill = dat.gg$category_color,
                   alpha=0.7) +
      
      # Add text in the center of each bubble + control its size
      geom_text(data = weekdata_p, aes(x, y, size=value, label = group), color="black") + #, color=weekdata_p$text_color
      geom_text(data = weekdata_p, aes(x, y, size=value + 0.07, label = group), color="black") +
      scale_size_continuous(range = range_b) +
      coord_equal()

    # General theme:
    bubbleplot <- bubbleplot +
      theme_void() + 
      theme(legend.position="none") +
      theme(panel.border = element_blank()) 
    
    # add titles
    bubbleplot <- bubbleplot + 
      labs(
        title = "The last 7 days, Sex crimes against children",
        subtitle = numcrimesweektext,
      ) 
    
    # make theme prettier
    bubbleplot <- bubbleplot + 
      theme(
        plot.title = element_text( # font size "large"
          size = 20,
          hjust = 0, vjust = 1,
          margin = margin(b = 15/2)
        ),
        plot.subtitle = element_text( # font size "regular"
          size = 15,
          hjust = 0, vjust = 1,
          margin = margin(b = 15/2)
        )
      )
    
    # show the figure
    bubbleplot
    
  })
  
  ############ bubble plot hover text ##############
  
  output$bubbleplot_info <- renderUI({
    
    if(length(str_subset(input$includebubble, "include_notlisted")) > 0) {
      big_category_counts_summary <- big_category_counts_bubble |> 
        distinct(Category, .keep_all = TRUE)
    } else {
      big_category_counts_summary <- big_category_counts_bubble |> 
        distinct(Category, .keep_all = TRUE)
      big_category_counts_summary <- big_category_counts_summary |> 
        filter(Category != "Not Listed")
    }
    
    # Create data, group by big categories, sort by size and alpha (in case of size ties)
    weekdata <- data.frame(group=paste(big_category_counts_summary$Category_n, big_category_counts_summary$Category,sep="\n"), 
                           value=big_category_counts_summary$Category_n, 
                           category_color=big_category_counts_summary$category_color, 
                           text_color=big_category_counts_summary$text_color,
                           hovertextinfo=big_category_counts_summary$Category) |> 
      arrange(desc(big_category_counts_summary$Category_n), big_category_counts_summary$Category)
    
    # Generate the layout. This function return a dataframe with one line per bubble. 
    # It gives its center (x and y) and its radius, proportional of the value
    packing <- circleProgressiveLayout(weekdata$value, sizetype='area')
    
    # We can add these packing information to the initial data frame
    weekdata_p <- cbind(weekdata, packing)
    
    
    # capture location of mouse hovering after it pauses
    hover_bubble <- input$bubbleplot_hover
    
    if(!is.null(hover_bubble)){ # mouse is in bounds
      left_px_bubble <- hover_bubble$x  # for chart coordinates
      top_px_bubble <- hover_bubble$y  
      
      left_css_px_bubble <- hover_bubble$coords_css$x  # for position of tooltip
      top_css_px_bubble <- hover_bubble$coords_css$y
      
      # if mouse is on left of graph, put tool tips to right.  if on right put on left
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_css_px_bubble, "px; top:", top_css_px_bubble-700, "px;")
      
      groupval <- NULL
      for(i in 1:length(weekdata_p$radius)){
        if(sqrt((left_px_bubble - weekdata_p$x[i])^2 + (top_px_bubble - weekdata_p$y[i])^2) < weekdata_p$radius[i]){
          groupval <- weekdata_p$hovertextinfo[i]
        }
      }
      
      # construct names hover info
      if(!is.null(groupval)){
        hovertext_bubble <- big_category_counts_bubble |> 
          filter(Category == groupval) |> 
          ungroup() |> 
          select(Relation) |> 
          distinct()

        hovertext_str <- NULL
        if(dim(hovertext_bubble)[1] == 1){
          hovertextformatted <- paste("<b>People listed here only include:</b><br />* ", hovertext_bubble$Relation[1], sep="")
        } else if(dim(hovertext_bubble)[1] < 6) {
          for(i in seq(1, dim(hovertext_bubble)[1])){
            hovertext_sub <- paste("* ",hovertext_bubble$Relation[i],"<br />", sep="")
            hovertext_str <- paste(hovertext_str, hovertext_sub, sep="")
          }
          hovertextformatted <- paste("<b>People listed here included:</b>", hovertext_str, sep="<br />")
        } else if(dim(hovertext_bubble)[1] >= 6) {
          for(i in seq(1, dim(hovertext_bubble)[1], 2)){
            if(i + 1 > dim(hovertext_bubble)[1]){
              hovertext_sub <- paste("* ",hovertext_bubble$Relation[i], sep="")
              hovertext_str <- paste(hovertext_str, hovertext_sub, sep="")
            } else {
              hoversub1 <- hovertext_bubble$Relation[i]
              hoversub1 <- paste("* ",hoversub1, ", ", sep="")
              hoversub2 <- hovertext_bubble$Relation[i+1]
              hoversub2 <- paste("* ",hoversub2,",<br />", sep="")
              hovertext_str <- paste(hovertext_str, hoversub1 , hoversub2, sep="")
            }
          }
          #hovertext_str <- hovertext_sub
          #hovertext_str <- str_replace_all(hovertext_str, ";","<br />* ")
          hovertextformatted <- paste("<b>People listed here included:</b>", hovertext_str, sep="<br />")
        }
        
        crimes_on_date_text <- hovertextformatted
        
        wellPanel(
          style = style,
          p(HTML(crimes_on_date_text))
        )
      }
    }
    
  })
  
  ######################## About - cumulative sum of data ###############################
  
  ##############
  # Linear model - fill in zeros for days with no crimes, create a df ready for time series
  ##############
  eventsperday <- wmn_data_previous |>
    group_by(Date) |> 
    summarize(perday = n()) |> 
    complete(Date = seq.Date(min(Date), max(Date), by = "days"), 
             fill = list(perday = 0)) |> 
    mutate(cum_sum = cumsum(perday)) |> 
    arrange(Date)
  
  # get the last month's data, count, and make a linear regression
  sumduration = 30
  numEventsperday_reg <- eventsperday |> 
    filter(Date > max(Date) - duration(sumduration, 'days'))
  
  #textlabel <- "Linear model"
  cumsum30.lm = lm(cum_sum ~ Date, data=numEventsperday_reg)
  # p-value
  cumsum_p <- sorted_p(cumsum30.lm)
  cumsum_ar <- summary(cumsum30.lm)$adj.r.squared
  # slope, intercept
  cumsum_m <- cumsum30.lm$coefficients[2]
  cumsum_b <- cumsum30.lm$coefficients[1]
  cumsumlineartext <- "Predicted crimes based on linear regression use the last 30 days of data and the current date.<br /><br />The dark red dotted line represents the regression, with"
  cumsumlineartext <- paste(cumsumlineartext, " slope = ", round(cumsum_m,2), " crimes per day, and r<sup>2</sup> = ", round(cumsum_ar,4), sep='')
  cumsumlineartext <- paste(cumsumlineartext, "<br /><br />(no hover data on this plot)")
  
  nextcrimetext <- paste("Number of crimes predicted for ",strftime(now(), "%A, %B %d, %Y")," is: ",round(cumsum_m, 0), sep="")
  
  ############
  # ETS model - last month of data
  ###########
  eventsperday_m <- eventsperday |>
    filter(Date >= now()-days(30)) |> 
    filter(Date < date(now())) |>  # remove today because we're predicting today
    select(date=Date, value=perday) |> 
    as_tsibble(index=date)
  
  # ETS: Exponential smoothing state space model
  fit <- eventsperday_m |> 
    model(
      ets = ETS(value)
    )
  
  # use model to forecast 10 days into future, including today
  fc <- fit |> 
    forecast(h = 10)
  
  futuredata <- tibble(date=fc$date, crimes=fc$.mean)
  todaycrime <- futuredata[futuredata$date == strftime(now(), "%Y-%m-%d"),]$crimes
  todaydate <- futuredata[futuredata$date == strftime(now(), "%Y-%m-%d"),]$date
  # no more data past 10 days into future
  if(length(todaycrime) == 0){
    todaycrime <- futuredata[futuredata$date == max(futuredata$date),]$crimes
    todaydate <- futuredata[futuredata$date == max(futuredata$date),]$date
  }
  
  nextcrimetext_m <- paste("\nNumber of crimes predicted for ", strftime(todaydate, "%A, %B %d, %Y"), " is: ", round(todaycrime, 0), sep="")
  
  ##############
  # decomp model
  ##############
  eventsperday_d <- eventsperday |>
    filter(Date >= now()-(days(30) + (8 - wday(now())))) # last 30 days + some for next filter
  
  # start the time series on Sunday so we don't have to calculate what day the model spits out
  # day of the week, starting on Sunday, day 7
  startindex <- 1
  for(i in 1:8){
    if(wday(eventsperday_d$Date[i], week_start=1) == 7){
      startindex <- i
    }
  }
  
  tsdata = ts(eventsperday_d$perday[startindex:length(eventsperday_d$perday)], freq=7) ## “seasonal” window of 7 days
  #stl_data <- stl(tsdata, s.window=30)
  decomp_data <- decompose(tsdata, "multiplicative")
  
  # get today's crime by multiplying today's seasonal position by the current trend
  todaycrime_d <- decomp_data$seasonal[wday(now())] *
    mean(decomp_data$trend[!is.na(decomp_data$trend)])
    
  # # get the seasonal * mean trend for plotting, removing NAs from trend data
  # date_d <- eventsperday_d$Date[startindex:length(eventsperday_d$perday)]
  # date_d <- date_d[!is.na(decomp_data$trend)] |> 
  #   wday(label=TRUE)
  # seasonal_trend_d <- decomp_data$seasonal[!is.na(decomp_data$trend)] * mean(decomp_data$trend[!is.na(decomp_data$trend)])
  # decomp_df <- cbind.data.frame(date=seq(1:length(date_d)), value=seasonal_trend_d)
  
  # get the seasonal * mean trend for plotting, removing NAs from trend data
  date_d <- wday(seq(1:7),label=TRUE)
  seasonal_trend_d <- decomp_data$seasonal * mean(decomp_data$trend[!is.na(decomp_data$trend)])
  decomp_df <- cbind.data.frame(date=seq(1:7), value=seasonal_trend_d[1:7])
  
  # find today for plot
  decomp_df_today <- decomp_df |> 
    filter(date==wday(now()))
  
  nextcrimetext_d <- paste("\nNumber of crimes predicted for ", strftime(todaydate, "%A, %B %d, %Y"), " is: ", round(todaycrime_d, 0), sep="")
 
  ##################
  # Linear model plot
    
  output$plotcumsum <- renderPlot({
    
    if(input$modelselecter == "linearmodel"){
      output$cumsumlineartext <- renderText({
        cumsumlineartext
      })
        
    }
    
    cumsumplot <- ggplot(eventsperday, aes(x=Date, y=cum_sum)) + 
      geom_line() +
      #stat_bin(aes(, geom="step") +
      #geom_text(aes(x=min(Date), y=ypos, label = nextcrimetext, group = NULL), size=7, vjust=1, hjust=0) +
      theme_light() +
      theme(
        legend.position = "none",
        panel.border = element_blank(),
      ) 
    
    # construct the linear regression
    if(input$modelselecter == "linearmodel"){
      cumsumplot <- cumsumplot +
        geom_smooth(data = numEventsperday_reg, 
                    method='lm', formula = y ~ x, color='darkred', 
                    fullrange = TRUE, linetype="dotted",
                    se = FALSE, na.rm=TRUE) +
        ylim(0, NA)
    } else if(input$modelselecter == "daymodel"){
      # no regression
    }
    
    # add titles
    cumsumplot <- cumsumplot + 
      labs(
        title = nextcrimetext,
        subtitle = "Linear model",
        x = "Date", 
        y = "Cumulative sum of sex crimes against children"
      ) 
    
    # make theme prettier
    cumsumplot <- cumsumplot + theme(
      legend.position="none",  # remove legend because tooltips will suffice
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(1)),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.title.y = element_text(size=17),
      axis.title.x = element_text(size=17),
      plot.title = element_text( # font size "large"
        size = 20,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      ),
      plot.subtitle = element_text( # font size "regular"
        size = 15,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      )
    )
    
    # show the figure
    cumsumplot
    
  })
  
  ##################
  # ETS model plot
  
  output$ets <- renderPlot({
    
    forecastplot <- ggplot(eventsperday_m, aes(x=date, y=value)) + 
      geom_line() +
      geom_point() +
      geom_line(data = futuredata, aes(x=date, y=crimes), col="red") +
      geom_point(data = futuredata, aes(x=todaydate, y=todaycrime), col="red", shape = "diamond", size=5) +
      theme_light() +
      theme(
        legend.position = "none",
        panel.border = element_blank(),
      ) 
    
    # add titles
    forecastplot <- forecastplot + 
      labs(
        title = nextcrimetext_m,
        subtitle = "ETS model",
        x = "Date", 
        y = "Sex crimes against children"
      ) 
    
    # make theme prettier
    forecastplot <- forecastplot + theme(
      legend.position="none",  # remove legend because tooltips will suffice
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(1)),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.title.y = element_text(size=17),
      axis.title.x = element_text(size=17),
      plot.title = element_text( # font size "large"
        size = 20,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      ),
      plot.subtitle = element_text( # font size "regular"
        size = 15,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      )
    )
    
    # show the figure
    forecastplot
    
  })
  
  ##################
  # Decomp model plot
  
  output$decomp <- renderPlot({
    
    decompplot <- ggplot(decomp_df, aes(x=date, y=value)) + 
      geom_line() +
      geom_point(data = decomp_df_today, aes(x=date, y=value), col="red", shape = "diamond", size=5) +
      theme_light() +
      theme(
        legend.position = "none",
        panel.border = element_blank(),
      ) 
    
    # add titles
    decompplot <- decompplot + 
      labs(
        title = nextcrimetext_d,
        subtitle = "Decomposition model",
        x = "Date", 
        y = "Time series of sex crimes against children"
      ) +
      scale_x_continuous(breaks=seq(1:length(date_d)), labels=as.character(date_d))
    
    # make theme prettier
    decompplot <- decompplot + theme(
      legend.position="none",  # remove legend because tooltips will suffice
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(1)),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.title.y = element_text(size=17),
      axis.title.x = element_text(size=17),
      plot.title = element_text( # font size "large"
        size = 20,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      ),
      plot.subtitle = element_text( # font size "regular"
        size = 15,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      )
    )
    
    # show the figure
    decompplot
    
  })
  
  
  #################################### About That Line ###############################
  
  # get the brush info when needed
  ranges <- reactiveValues(x = NULL, y = NULL)
  #print(paste("reactiveValues: ", ranges, sep=""))
  
  # draw the line plot of raw (daily sum, "perday") values
  output$linedataraw <- renderPlot({
    
    #print(paste("plot: ", ranges$x, sep=""))
    
    lineplot <- ggplot(eventsperday, aes(x=Date, y=perday)) + 
      geom_line() +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      # xlim = ranges$x +
      # ylim = ranges$y +
      theme_light() +
      theme(
        legend.position = "none",
        panel.border = element_blank(),
      ) 
    
    # add titles
    lineplot <- lineplot + 
      labs(
        title = "Sex crimes against children",
        subtitle = "Daily data",
        x = "Date", 
        y = "Individual sex crimes against children"
      ) 
    
    # make theme prettier
    lineplot <- lineplot + theme(
      legend.position="none",  # remove legend because tooltips will suffice
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(1)),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.title.y = element_text(size=17),
      axis.title.x = element_text(size=17),
      plot.title = element_text( # font size "large"
        size = 20,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      ),
      plot.subtitle = element_text( # font size "regular"
        size = 15,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      )
    )
    
    # show the figure
    lineplot
    
  })
  
    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$plot_dblclick, {
      brush <- input$zoom_brush
      if (!is.null(brush)) {
        ranges$x <- c(as.Date(round(brush$xmin), origin = "1970-01-01"), as.Date(round(brush$xmax), origin = "1970-01-01"))
        ranges$y <- c(round(brush$ymin), round(brush$ymax))
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    
    #### tooltips for line plot, just give names ####
    
    middate <- (max(wmn_data_previous$Date) - min(wmn_data_previous$Date))/2+min(wmn_data_previous$Date)
    output$lineplot_info <- renderUI({
      
      # capture location of mouse hovering after it pauses
      hover <- input$lineplot_hover
      
      if(!is.null(hover)){ # mouse is in bounds
        left_px <- hover$x  # for chart coordinates
        wmn_data_previous_x <- as.Date(round(left_px), origin = "1970-01-01")  # turn into date
        top_px <- hover$y  
        
        left_css_px <- hover$coords_css$x  # for position of tooltip
        top_css_px <- hover$coords_css$y
        
        #wmn_data_previous_x <- as.Date(round(left_px), origin = "1970-01-01")  # turn into date
        crimes_on_date <-  wmn_data_previous |> 
          filter(Date == wmn_data_previous_x) |> 
          select(Date, Name, State)
        
        # if mouse is on left of graph, put tool tips to right.  if on right put on left
        if(wmn_data_previous_x < middate){
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:", left_css_px, "px; top:", top_css_px-500, "px;")
        } else if(wmn_data_previous_x >= middate && dim(crimes_on_date)[1] < 20) {
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:", left_css_px-250, "px; top:", top_css_px-500, "px;")
        } else if(wmn_data_previous_x >= middate && dim(crimes_on_date)[1] >= 20) {
          style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                          "left:", left_css_px-350, "px; top:", top_css_px-500, "px;")
        }
      
        # construct names hover info, two columns
        crimes_on_date_text <- paste("<b>",wmn_data_previous_x,"</b><br /><b>Names of people charged:</b><br />",sep="")
        counter <- 0
        for(i in 1:dim(crimes_on_date)[1]){
          if(i == dim(crimes_on_date)[1]){  # last entry, no comma afterwards
            crimes_on_date_text <- paste(crimes_on_date_text, crimes_on_date$Name[i],sep="")
          }
          else if(counter == 0 && dim(crimes_on_date)[1] > 20){
            crimes_on_date_text <- paste(crimes_on_date_text, crimes_on_date$Name[i], ", ", sep="")
            counter <- 1
          } else if(counter == 1 && dim(crimes_on_date)[1] > 20){
            crimes_on_date_text <- paste(crimes_on_date_text, crimes_on_date$Name[i], ", ", sep="")
            counter <- 2
          } else if(counter == 0 && dim(crimes_on_date)[1] <= 20){
            crimes_on_date_text <- paste(crimes_on_date_text, crimes_on_date$Name[i], ", ", sep="")
            counter <- 2
          } else if(counter == 2) {
            crimes_on_date_text <- paste(crimes_on_date_text, crimes_on_date$Name[i], ",<br />", sep="")
            counter <- 0
          }
        }
        #crimes_on_date_text
        
        wellPanel(
          style = style,
          p(HTML(crimes_on_date_text))
        )
      }
      
    })
    
    
    ###### time series analysis #####
    
    output$timeseries <- renderPlot({
      
    dat = cbind(eventsperday$Date[startindex:length(eventsperday$Date)], 
                  with(decomp_data, data.frame(Observed=x, Trend=trend, Seasonal=seasonal, Random=random)))
    
    dat <- dat |> 
      mutate(date = eventsperday$Date[startindex:length(eventsperday$Date)]) |> 
      select(c(date, Observed, Trend, Seasonal))
    
    timeseriesplot <- ggplot(gather(dat, component, value, -date), aes(date, value), scale = continuous) +
      facet_grid(component ~ ., scales="free_y") +
      geom_line() +
      scale_x_continuous() +
      scale_y_continuous()
    
    # add titles
    timeseriesplot <- timeseriesplot + 
      labs(
        title = "Decomposed WMN Sex Crimes against Children",
        x = "Date", 
        y = "Crimes per day"
      ) 
    
    # make theme prettier
    timeseriesplot <- timeseriesplot + theme(
      legend.position="none",  # remove legend because tooltips will suffice
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(1)),
      axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16),
      axis.title.y = element_text(size=17),
      axis.title.x = element_text(size=17),
      plot.title = element_text( # font size "large"
        size = 20,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      ),
      plot.subtitle = element_text( # font size "regular"
        size = 15,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      )
    )
    
    # show the plot
    timeseriesplot
    
  })
  
  
  ######################## Groups of People Bar Plot ###############################
  
  # make the stacked bar chart
  output$plot1 <- renderPlot({
    
    # if we don't include "Not Listed", filter it out so it doesn't get plotted; move annotations.
    boolean_include_notlisted = TRUE
    if(length(str_subset(input$include, "include_notlisted")) == 0){
      names_category_counts <- names_category_counts |>
        filter(Category != "Not Listed")
      trans_groups$barrank = trans_groups$barrank -1
      boolean_include_notlisted = FALSE
    }
    
    # set up annotations for trans and drag queens (they may be in more than one category)
    #    Function to add rectangle for number of trans, height = number in the correct category
    add_trans_rectangle <- function(trans_group){
      geom_rect(data = trans_group, aes(xmin = barrank-0.44, xmax = barrank+0.44, ymin = (csum - (Category_n/2)), ymax = (csum - (Category_n/2) + trans_n)), fill = "yellow", color = "yellow")
    }
    
    #    Function to add rectangle for number of drag queens, height = number in the correct category
    ##### Placeholder to be filled in once any drag queen is charged ####
    
    # Function to add text with numbers and percentage of trans, depending on if "not listed" is included
    add_trans_annotations <- function(trans_group, count, not_listed = FALSE){
      if(not_listed == FALSE){
        percenttext = "% of named total"
      } else {
        percenttext = "% of total"
      }
      ggplot2::annotate("text", x = trans_group$barrank + 0.75, y = trans_group$csum+20,
                        label = paste(trans_group$trans_n, " people who are trans\n", round(trans_group$trans_n / count * 100, digits=1), percenttext, sep=""),
                        size = 6, hjust = 0, vjust = 0)
    }
    
    # create base plot
    wmn_barplot <- ggplot(data = names_category_counts, aes(x = fct_infreq(big_category), fill= fct_reorder(Category,Category_n), text=Category)) +
      geom_bar(color = "black", stat = "count")
    
    # add annotations and bars if indicated trans
    if(length(str_subset(input$include, "include_trans")) > 0) {
      for(i in 1:dim(trans_groups)[1]){
        wmn_barplot <- wmn_barplot + add_trans_rectangle(trans_groups[i,])
      }
      trans_group_flat <- trans_groups |> 
        filter(barrank == max(trans_groups$barrank))
      trans_group_flat$trans_n = sum(trans_groups$trans_n)
      
      if(!boolean_include_notlisted){
        # pick farthest group to add text annotations to, but for sum of named groups
        wmn_barplot <- wmn_barplot + add_trans_annotations(trans_group_flat, named_total_count, not_listed = boolean_include_notlisted)
      } else {
        # pick farthest group to add text annotations to, but for sum of all groups
        wmn_barplot <- wmn_barplot + add_trans_annotations(trans_group_flat, total_count, not_listed = boolean_include_notlisted)
      }
    }
    
    if(length(str_subset(input$include, "include_dragqueen")) > 0){
      wmn_barplot <- wmn_barplot + ggplot2::annotate("text", x=Inf, y = Inf, label = "Not a single Drag Queen.", size = 7, vjust=1, hjust=1)
    }
    
    # manually put in labels and group matching colors
    wmn_barplot <- wmn_barplot +
      scale_fill_manual(drop=FALSE, na.translate = FALSE,
                        breaks = c(
                          "Not Listed",
                          "Pastors",
                          "Church Employees",
                          "Priests/Brothers",
                          "Mormon Leaders",
                          "Missionaries",
                          "Family Members",
                          "Family Friends/Neighbors",
                          "Teachers/Aides",
                          "Day Care/Babysitters",
                          "Coaches",
                          "Police",
                          "Other",
                          "Politicians",
                          "Doctors"
                        ),
                        labels = c(
                          "Not Listed",
                          "Pastors",
                          "Church Employees",
                          "Priests/Brothers",
                          "Mormon Leaders",
                          "Missionaries",
                          "Family Members",
                          "Family Friends/Neighbors",
                          "Teachers/Aides",
                          "Day Care/Babysitters",
                          "Coaches",
                          "Police",
                          "Other",
                          "Politicians",
                          "Doctors"
                        ),
                        values = c(
                          "Not Listed" = "#FFFFFF",
                          "Pastors" = "#FF6340",
                          "Church Employees" = "#FFBC40",
                          "Priests/Brothers" = "#F5365F",
                          "Mormon Leaders" = "#F5D22F",
                          "Missionaries" = "#E88C46",
                          "Family Members" = "#697A29",
                          "Family Friends/Neighbors" = "#B8B42D",
                          "Teachers/Aides" = "#7A4419",
                          "Day Care/Babysitters" = "#D7BE82",
                          "Coaches" = "#755C1B",
                          "Police" = "#1C77C3",
                          "Other" = "#888888",
                          "Politicians" = "#990000",
                          "Doctors" = "#009900"
                        )
      )
    
    # add titles
    wmn_barplot <- wmn_barplot + 
      labs(
        title = "Crimes against children",
        subtitle = "Data from 'Who's Making News for Sex Crimes Involving Children?'",
        x = "", 
        y = "Number of individuals"
      )
    
    # make bar chart prettier
    wmn_barplot <- wmn_barplot + theme(
      legend.position="none",  # remove legend because tooltips will suffice
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(0.5)),
      axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1, size=15),
      axis.text.y = element_text(size=15),
      axis.title.y = element_text(size=18),
      plot.title = element_text( # font size "large"
        size = 20,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      ),
      plot.subtitle = element_text( # font size "regular"
        size = 15,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      )
    )
    
    # display bar chart
    wmn_barplot
    
  })
  
  # tooltips for bar chart
  output$hover_info_bar <- renderUI({
    
    # if we don't include "Not Listed", filter it out of the data for labels - default is FALSE
    if(length(str_subset(input$include, "include_notlisted")) == 0){
      big_category_counts <- big_category_counts |>
        filter(big_category != "Not Listed")
      stacked_barplot_numbers <- stacked_barplot_numbers |>
        filter(Category != "Not Listed")
    }
    
    # capture location of mouse hovering after it pauses
    hover_bar <- input$plot_hover_bar
    
    left_px_bar <- hover_bar$x  # for chart coordinates
    top_px_bar <- hover_bar$y
    
    left_css_px_bar <- hover_bar$coords_css$x  # for position of tooltip
    top_css_px_bar <- hover_bar$coords_css$y
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_css_px_bar+5, "px; top:", top_css_px_bar-500, "px;")
    
    # actual tooltip created as a "wellPanel"
    # If the cursor is out of bounds of the figure, then hover is null and no tooltip shown
    if(is.null(hover_bar)){
      
    } else { # mouse is in bounds, find the column, 8 max (if including Not Listed), 0 is between bars
      mouse_pos <- case_when(
        left_px_bar < 0.55 ~ 0,
        left_px_bar < 1.4 ~ 1,
        left_px_bar < 1.55 ~ 0,
        left_px_bar < 2.4 ~ 2,
        left_px_bar < 2.55 ~ 0,
        left_px_bar < 3.4 ~ 3,
        left_px_bar < 3.55 ~ 0,
        left_px_bar < 4.4 ~ 4,
        left_px_bar < 4.55 ~ 0,
        left_px_bar < 5.4 ~ 5,
        left_px_bar < 5.55 ~ 0,
        left_px_bar < 6.4 ~ 6,
        left_px_bar < 6.55 ~ 0,
        left_px_bar < 7.4 ~ 7,
        left_px_bar < 7.55 ~ 0,
        left_px_bar <= 8.4 ~ 8,
        left_px_bar > 8.4 ~ 0
      )
      
      if(mouse_pos != 0){
        # set name of column
        tooltip_big_category <- big_category_counts[mouse_pos,1]
        
        # get name of small category ("Category") from position of top_px cursor
        tooltip_small_category_counts <- stacked_barplot_numbers |>
          filter(big_category == tooltip_big_category) |>
          filter(top_px_bar <= csum)
        tooltip_small_category <- tooltip_small_category_counts[1,1]
        tooltip_small_category_percent <- round(
          tooltip_small_category_counts[1,]$Category_n / tooltip_small_category_counts[1,]$big_category_n * 100, 
          digits=1
        )
        
        # if we don't include "Not Listed", percent is of named groups.  If we do, then percent is of total
        if(length(str_subset(input$include, "include_notlisted")) == 0){
          tooltip_small_category_percent_oftotal <- 
            round(tooltip_small_category_counts[1,]$Category_n / named_total_count * 100, 
                  digits=1)
        } else {
          tooltip_small_category_percent_oftotal <- 
            round(tooltip_small_category_counts[1,]$Category_n / total_count * 100, 
                  digits=1)
        }}
      
      if(mouse_pos == 0){ # If somehow there are missed items, do nothing, return nothing, no tooltip shown
        
        # if not out of y-value range and we are filtering out "Not Listed"
      } else if(!is.na(tooltip_small_category) && length(str_subset(input$include, "include_notlisted")) == 0) {  
        wellPanel(
          style = style,
          p(HTML(paste0("<b>", tooltip_small_category,
                        "</b><br />",
                        tooltip_small_category_percent,
                        "% of ",
                        tooltip_big_category, "<br />",
                        tooltip_small_category_percent_oftotal,
                        "% of named total"
          )))
        )
      } else if(!is.na(tooltip_small_category)) {  # if not out of y-value range only, includes not listed
        wellPanel(
          style = style,
          p(HTML(paste0("<b>", tooltip_small_category,
                        "</b><br />",
                        tooltip_small_category_percent,
                        "% of ",
                        tooltip_big_category, "<br />",
                        tooltip_small_category_percent_oftotal,
                        "% of total"
          )))
        )
      } else {
        # do nothing, return nothing
      }
    }
    
    
  })
  
  ######################## Gender ###############################
  
  female_count <- wmn_names_genderized |>
    filter(Gender == "female") |>
    add_count(First_name) |>
    distinct(First_name, .keep_all = TRUE) |>
    mutate(Proportion = n/sum(n)) |> 
    mutate(Global_prop = Gender_count/sum(Gender_count)) |> 
    mutate(gender = "female") |> 
    select(c('First_name','n','Proportion','gender','Global_prop'))
  
  male_count <- wmn_names_genderized |>
    filter(Gender == "male") |>
    add_count(First_name) |>
    distinct(First_name, .keep_all = TRUE) |>
    mutate(Proportion = n/sum(n)) |> 
    mutate(Global_prop = Gender_count/sum(Gender_count)) |> 
    mutate(gender = "male") |> 
    select(c('First_name','n','Proportion','gender','Global_prop'))
  
  atypical_count <- wmn_names_genderized |>
    filter(Gender == "atypical") |>
    add_count(First_name) |>
    distinct(First_name, .keep_all = TRUE) |>
    mutate(Proportion = n/sum(n)) |> 
    mutate(Global_prop = Gender_count/sum(Gender_count)) |> 
    mutate(gender = "atypical") |> 
    select(c('First_name','n','Proportion','gender','Global_prop'))
  
  # wordcloud size scale bar
  sliderValue <- reactive({
    data.frame(
      Value = as.numeric(input$decimal))
  })
  
  # Get all possible combinations of gendered names from checkboxes
  # total_count <- NA
  # numtables <- 1
  
  # checkboxvalues <- NULL
  observeEvent(input$gender, {
    checkboxvalues <- input$gender
    
    # Assign size of tables to the genderized names based on checkboxes
    if(length(str_subset(input$gender, "gender_male")) > 0){
      numtables <- 1
      if(length(str_subset(input$gender, "gender_female")) > 0){
        numtables <- 2
        if(length(str_subset(input$gender, "gender_atypical")) > 0) {
          numtables <- 3
        }
      } else if(length(str_subset(input$gender, "gender_atypical")) > 0) {
        numtables <- 2
      }
    } else if(length(str_subset(input$gender, "gender_female")) > 0) {
      numtables <- 1
      if(length(str_subset(input$gender, "gender_atypical")) > 0) {
        numtables <- 2
      }
    } else if(length(str_subset(input$gender, "gender_atypical")) > 0){
      numtables <- 1
    } else {
      # nothing selected
    }
    
    output$malenamesTable <- DT::renderDataTable({
      numtables <- as.integer(round(15 / numtables, 0)) # for size of tables to display
      datatable(male_count[,-4], options = list(pageLength = numtables, 
                                                autoWidth = TRUE,
                                                order = list(1, 'desc')
      ), 
      rownames = FALSE,
      colnames = c('First Name', 'Crimes', 'Percent from WMN', 'US Proportion'),
      caption = 'Table 1: Male Names.'
      ) |> 
        formatPercentage(c('Proportion', 'Global_prop'), 2)
    })
    
    output$femalenamesTable <- DT::renderDataTable({
      numtables <- round(15 / numtables, 0) # for size of tables to display
      datatable(female_count[,-4], options = list(pageLength = numtables, 
                                                  autoWidth = TRUE,
                                                  order = list(1, 'desc')
      ), 
      rownames = FALSE,
      colnames = c('First Name', 'Crimes', 'Percent from WMN', 'US Proportion'),
      caption = 'Table 2: Female Names.'
      ) |> 
        formatPercentage(c('Proportion', 'Global_prop'), 2)
    })
    
    output$atypicalnamesTable <- DT::renderDataTable({
      numtables <- round(15 / numtables, 0) # for size of tables to display
      datatable(atypical_count[,-4], options = list(pageLength = numtables, 
                                                    autoWidth = TRUE,
                                                    order = list(1, 'desc')
      ), 
      rownames = FALSE,
      colnames = c('First Name', 'Crimes', 'Percent from WMN', 'US Proportion'),
      caption = 'Table 1: Uncommon Names.'
      ) |> 
        formatPercentage(c('Proportion', 'Global_prop'), 2)
    })
    
  })
  
  output$wordcloud <- renderWordcloud2({
    
    wordslidervalue <- sliderValue()
    
    # Assign colors to the genderized names based on checkboxes
    if(length(str_subset(input$gender, "gender_male")) > 0){
      total_count <- male_count
      basecolors = c("black")
      if(length(str_subset(input$gender, "gender_female")) > 0){
        total_count <- rbind(total_count, female_count)
        basecolors = c(basecolors, "purple")
        if(length(str_subset(input$gender, "gender_atypical")) > 0) {
          total_count <- rbind(total_count, atypical_count)
          basecolors = c(basecolors, "green")
        }
      } else if(length(str_subset(input$gender, "gender_atypical")) > 0) {
        total_count <- rbind(total_count, atypical_count)
        basecolors = c(basecolors, "green")
      }
      
    } else if(length(str_subset(input$gender, "gender_female")) > 0) {
      total_count <- female_count
      basecolors = c("purple")
      if(length(str_subset(input$gender, "gender_atypical")) > 0) {
        total_count <- rbind(total_count, atypical_count)
        basecolors = c(basecolors, "green")
      }
    } else if(length(str_subset(input$gender, "gender_atypical")) > 0){
      total_count <- atypical_count
      basecolors = c("green")
    } else {
      # nothing selected
    }
    
    # sort by descending because wordcloud2 will skip names that dont fit at the end of the list
    if(!is.null(dim(total_count))){
      total_count <- total_count |>
        arrange(desc(n)) |> 
        select(c('First_name','n','Proportion','gender'))
      
      colorlist = basecolors[match(total_count$gender,unique(total_count$gender)) ]
      
      # create dataframe to pass to wordcloud2
      wordcloud_size = wordslidervalue$Value
      total_count2 <- data.frame(word = total_count$First_name, freq = total_count$n, color=colorlist) |>
        arrange(desc(freq))
      set.seed(1) # make semi-repeatable
      wordcloud2(data = total_count2, color=total_count2$color, size=wordcloud_size)
      
    }
  })
  
  output$pie1 <- renderPlot({
    
    # Create Data
    piedata <- data.frame(
      Names = as.factor(c("Male", "Female", "Uncommon")),
      value = c(sum(male_count$n), sum(female_count$n), sum(atypical_count$n))
    )
      
    # turn into percentages for human-readable debugging
    piedata <- piedata |>
      mutate(value = value/sum(value)*100)
    
    # Compute the position of labels
    piedata <- piedata |>
      arrange(Names) |>
      mutate(csum = rev(cumsum(rev(value))), 
             pos = value/2 + lead(csum, 1),
             pos = if_else(is.na(pos), value/2, pos))
    
    # pie chart with labels outside
    ggplot(piedata, aes(x = "", y = value, fill = fct_inorder(Names))) +
      geom_bar(linewidth = 1, size = 1, color = "white", stat = "identity") + # lines between
      coord_polar(theta = "y") +
      guides(fill = guide_legend(title = "Names")) +
      scale_y_continuous(breaks = piedata$pos, labels = paste(round(piedata$value,digits=1),"%",sep="")) +
      theme(axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text = element_text(size = 17), 
            legend.text = element_text(size = 17),
            legend.title = element_text(size = 17),
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white")) +
      scale_fill_manual(values=c("purple","#555555","green"))
    
  })
  
  # tooltips for bar chart
  
  malenum <- sum(male_count$n)
  femalenum <- sum(female_count$n)
  atypicalnum <- sum(atypical_count$n)
  
  output$hover_pieinfo <- renderUI({
    
    # capture location of mouse hovering after it pauses
    hover <- input$pie_hover 
    
    left_px <- hover$x  # for chart coordinates
    top_px <- hover$y
    
    left_css_px <- hover$coords_css$x  # for position of tooltip
    top_css_px <- hover$coords_css$y
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_css_px+5, "px; top:", top_css_px-500+30, "px;")
    
    # actual tooltip created as a "wellPanel"
    # If the cursor is out of bounds of the figure, then hover is null and no tooltip shown
    if(is.null(hover)){
      
    } else {
      wellPanel(
        style = style,
        p(HTML(paste0("<b>", malenum," males</b><br />",
                      "<b>", femalenum, " females</b><br />",
                      "<b>", atypicalnum, " uncommon</b>")))
      )
    }
    
    
  })
  
  ######################### by State ################################
  
  # sort THEN make State a factor
  sorting <- function(KFF_data, column_name, reverse=FALSE) {
    #print(paste("entered sorting function: ", column_name))
    if(reverse){
      #print("reverse")
      KFF_data_out <- KFF_data |> 
        arrange(get(column_name), Location)# |>  # this changes by sort
      #print(KFF_data_out)
    } else {
      KFF_data_out <- KFF_data |> 
        arrange(desc(get(column_name)), desc(Location)) #|> 
      #print(KFF_data_out)
    }
    KFF_data_out <- KFF_data_out |> 
      mutate(State=factor(State, State)) # state must be factor to display sorted
    return(KFF_data_out)
  }
  
  # function to sort data to plot
  
  get_labels <- function(inputStates) {
    sortlabel <- ""
    sortcol <- ""
    sortcaption1 <- ""
    sortcaption2 <- ""
    if(inputStates == "median_income"){  
      sortlabel <- "Median Annual Income"
      sortcol <- "Median_Annual_Household_Income"
      sortcaption1 <- "Median annual household income"
      sortcaption2 <- "Data for 2021, number is in dollars."
    } else if(inputStates == "pct_below_FPL"){
      sortlabel <- "Percent Below the Federal Poverty Level"
      sortcol <- "FPL_lessthan_100"
      sortcaption1 <- "The percentage of people living below the Federal Poverty Level"
      sortcaption2 <- "Data for 2021, number is decimal (multiply by 100 for a percentage)."
    } else if(inputStates == "pct_part-time"){
      sortcol <- "Part_Time_Workers"
      sortlabel <- "Percent Part-Time Workers"
      sortcaption1 <- "The distribution of nonelderly population by household employment status: the percentage of people working part-time"
      sortcaption2 <- "Data for 2021, number is decimal (multiply by 100 for a percentage)."
    } else if(inputStates == "pct-unemployed"){
      sortcol <- "Unemployed"
      sortlabel <- "Percent Unemployed"
      sortcaption1 <- "Seasonally adjusted unemployment rate"
      sortcaption2 <- "Data for March 2022, number is decimal (multiply by 100 for a percentage)."
    } else if(inputStates == "political"){
      sortcol <- "Party"
      sortlabel <- "Political Party"
      sortcaption1 <- "The main political party of the state"
      sortcaption2 <- "Calculated as a linear combination of State Governor, Senate Majority, House Majority, and the Attorney General political affiliation (+1 for GOP, -1 for Dem, divided by n), except where such entities do not exist, as noted in the data. A value of -1 indicates 100% Democrat, a value of +1 indicates 100% Republican, and a value of 0 indicates an equal mix."
    } else if(inputStates == "gsp"){
      sortcol <- "Gross_State_Product_millions"
      sortlabel <- "Gross State Product"
      sortcaption1 <- "Total gross state product"
      sortcaption2 <- "Data are for 2022 in millions of current dollars"
    } else if(inputStates == "per_capita_spending"){
      sortcol <- "Per_Capita_State_Spending"
      sortlabel <- "Per Capita State Spending"
      sortcaption1 <- "Total state per capita expenditures"
      sortcaption2 <- "Data are for SFY 2021 in current dollars"
    } else if(inputStates == "pcs_edu"){
      sortcol <- "PCSpending_Elementary_Secondary_Education"
      sortlabel <- "State Spending on 1° & 2° Education"
      sortcaption1 <- "Total state expenditures on 1° & 2° Education"
      sortcaption2 <- "Data are for SFY 2021 in millions of current dollars"
    } else if(inputStates == "pcs_assistance"){
      sortcol <- "PCSpending_Assistance"
      sortlabel <- "Spending on Assistance"
      sortcaption1 <- "Total state expenditures on assistance"
      sortcaption2 <- "Data are for SFY 2021 in millions of current dollars"
    } else if(inputStates == "pcs_corrections"){
      sortcol <- "PCSpending_Corrections"
      sortlabel <- "Spending on Corrections"
      sortcaption1 <- "Total state expenditures on corrections"
      sortcaption2 <- "Data are for SFY 2021 in millions of current dollars"
    } else if(inputStates == "by_crimes"){
      sortcol <- "crime_percent_from_expected"
      sortlabel <- "Crimes Against Children"
      sortcaption1 <- "A regression line through the state data vs. crimes will indicate a sigificant effect of the state statistics on the occurrance of those crimes (or not).  The p-value will be reported on the figure above."
      sortcaption2 <- "The test statistic is (coefficient of slope / standard error of slope) with n-2 degrees of freedom and assumes the error in the linear regression is independent of x, data are normally distributed, etc."
    } else if(inputStates == "alpha"){
      sortcol <- "state_index"
      sortlabel <- "Alphabetical"
      sortcaption1 <- "State names sorted alphabetically"
      sortcaption2 <- "Because state names should not have any relation to sex crimes against children, these data should not be significant"
    } else if(inputStates == "pct_males"){
      sortcol <- "Male"
      sortlabel <- "Percent of Males"
      sortcaption1 <- "Population distribution by sex, percentage of males"
      sortcaption2 <- "Data for 2021, number is decimal (multiply by 100 for a percentage), only male or female included, numbers are rounded to the nearest 100."
    } else if(inputStates == "pct_no_childs"){
      sortcol <- "Adults_with_No_Children"
      sortlabel <- "Percent Adults with No Children"
      sortcaption1 <- "Population distribution by family structure, percent of adults without children"
      sortcaption2 <- "Data for 2021, number is decimal (multiply by 100 for a percentage)"
    } else if(inputStates == "pct_voted"){
      sortcol <- "Individuals_who_Voted"
      sortlabel <- "Percent Who Voted"
      sortcaption1 <- "Number of voters as a share of the voter population"
      sortcaption2 <- "Data for November 2022, number is decimal (multiply by 100 for a percentage)"
    } 
    
    return(list(label=sortlabel, col=sortcol, caption1=sortcaption1, caption2=sortcaption2))
  }
  
  output$byState <- renderPlot({
    
    # get the labels for sorting
    sortinfo <- get_labels(input$states)
    
    # sort the data for display
    KFF_data_sort <- sorting(KFF_data, sortinfo$col)
    
    # regression model
    if(sortinfo$col != "crime_percent_from_expected"){ # to avoid the warning for lm when perfect line
      state.lm = lm(crime_percent_from_expected ~ get(sortinfo$col), data=KFF_data_sort)
      # p-value
      state_p <- sorted_p(state.lm)
      state_ar <- summary(state.lm)$adj.r.squared
      # slope, intercept
      state_m <- state.lm$coefficients[2]
      state_b <- state.lm$coefficients[1]
    } else {
      state_p <- NULL
      state_ar <- NULL
      state_m <- 0
      state_b <- NULL
    }
    
    if(sortinfo$col == "state_index" || state_m < 0){  # states get reverse sorting
      KFF_data_sort <- sorting(KFF_data, sortinfo$col, reverse=TRUE)
    } 
    
    # plot
    lollipop <- ggplot(KFF_data_sort, aes(x=State, y=crime_percent_from_expected)) +
      geom_point(aes(color=mycolor), size=2) + 
      geom_segment(aes(x=State, xend=State, y=0, yend=crime_percent_from_expected, color=mycolor), linewidth=2, alpha=0.9) +
      theme_light() +
      theme(
        legend.position = "none",
        panel.border = element_blank(),
      ) 
    
    # add titles
    lollipop <- lollipop + 
      labs(
        title = "Crimes against children by State",
        subtitle = paste("Sorted by: ", sortinfo$label, sep=""),
        x = "", 
        y = "Percent difference from expected"
      )
    
    lollipop <- lollipop + theme(
      legend.position="none",  # remove legend because tooltips will suffice
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(0.5)),
      axis.text.x = element_text(angle = 90, vjust = 1.0, hjust=1, size=12),
      axis.text.y = element_text(size=14),
      axis.title.y = element_text(size=15),
      axis.title.x = element_text(size=15),
      plot.title = element_text( # font size "large"
        size = 20,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      ),
      plot.subtitle = element_text( # font size "regular"
        size = 15,
        hjust = 0, vjust = 1,
        margin = margin(b = 15/2)
      )
    )
    
    if(sortinfo$col != "crime_percent_from_expected"){
      if(state_p <= 0.05){
        if(state_m > 0){
          addtext = "has a <b><u>significant</u> positive correlation</b> to sex crimes against children (the higher the value, the higher number of sex crimes)."
        } else if(state_m < 0){
          addtext = "has a <b><u>significant</u> negative correlation</b> to sex crimes against children (the higher the value, the lower number of sex crimes)."
        } 
      } else {
        addtext = "does <b><u>not</u></b> have a correlation to sex crimes against children."
      }
    } else {
      addtext = ""
    }
    
    # put text below the lollipop chart to help describe
    output$statetext1 <- renderText({
      paste(sortinfo$caption1, addtext, sep=" ")
    })
    output$statetext2 <- renderText({
      sortinfo$caption2
    })
    
    if(sortinfo$col != "crime_percent_from_expected"){
      textlabel <- paste("p-value = ", signif(state_p, digits=3),"\n", "adjusted r2 = ", round(state_ar, digits=3), sep="")
      if(state_p < 0.05){
        lollipop <- lollipop + ggplot2::annotate("text", x=Inf, y = Inf, label = textlabel, size = 8, vjust=1, hjust=1, color="darkred")
      } else {
        lollipop <- lollipop + ggplot2::annotate("text", x=Inf, y = Inf, label = textlabel, size = 8, vjust=1, hjust=1, color="black")
      }
    } 
    
    lollipop
    
  })
  
  output$scatterbyState <- renderPlot({
    
    sortinfo <- get_labels(input$states)
    KFF_data_sort <- sorting(KFF_data, sortinfo$col, reverse=TRUE)
    
    # plot
    statescatter <- ggplot(KFF_data_sort, aes(x=get(sortinfo$col), y=crime_percent_from_expected)) +
      geom_point(aes(color="black"), size=1) + 
      theme_light() +
      theme(
        legend.position = "none",
        panel.border = element_blank(),
      ) 
    
    statescatter <- statescatter +                                     
      stat_smooth(method = "lm",
                  formula = y ~ x,
                  geom = "smooth")
    
    # add titles
    statescatter <- statescatter + 
      labs(
        x = sortinfo$label, 
        y = "Percent difference from expected"
      )
    
    statescatter <- statescatter + theme(
      legend.position="none",  # remove legend because tooltips will suffice
      panel.background = element_rect(fill = "white", colour = "white"),
      panel.grid = element_line(colour = "grey92"),
      panel.grid.minor = element_line(linewidth = rel(1)),
      axis.text.x = element_text(size=8),
      axis.text.y = element_text(size=8),
      axis.title.y = element_text(size=8),
      axis.title.x = element_text(size=8),
      panel.spacing.y = unit(1, "pt"),
      #aspect.ratio=2/3,
      plot.margin = unit(c(0, 0, 0, 0),"cm")
    )
    
    # don't plot the 1:1 figure
    if(sortinfo$col != "crime_percent_from_expected"){
      statescatter
    }
    
  })
  
  output$q_q <- renderPlot({
    
    sortinfo <- get_labels(input$states)
    x <- KFF_data |> 
      pull(sortinfo$col)
    
    qqnorm(x, pch = 1, frame = FALSE)
    qqline(x, col = "steelblue", lwd = 2)
    
  })
  
  # tooltips for state lollipop chart
  
  output$hover_stateinfo <- renderUI({
    
    sortinfo <- get_labels(input$states)
    
    # prepare tool tips.
    if(sortinfo$col == "crime_percent_from_expected" || 
       sortinfo$col == "Male" ||
       sortinfo$col == "Party" ||
       sortinfo$col == "FPL_lessthan_100" || 
       sortinfo$col == "Per_Capita_State_Spending") 
    {  # sorted forwards
      KFF_data_sort <- sorting(KFF_data, sortinfo$col)
    } else {
      KFF_data_sort <- sorting(KFF_data, sortinfo$col, reverse=TRUE)
    }
    
    tooltipStateList <- KFF_data_sort$Location
    tooltipExpected <- KFF_data_sort$expected_crime
    tooltipObserved <- KFF_data_sort$State_n
    
    toolinfo <- KFF_data_sort[sortinfo$col]
    
    # capture location of mouse hovering after it pauses
    hover <- input$state_hover 
    
    left_px <- hover$x  # for chart coordinates
    top_px <- hover$y
    
    left_css_px <- hover$coords_css$x  # for position of tooltip
    top_css_px <- hover$coords_css$y
    
    # if mouse is on left of graph, put tool tips to right.  if on right put on left
    if(!is.null(hover)){
      if(left_px < 25){
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_css_px+5, "px; top:", top_css_px-500, "px;")
      } else {
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_css_px-175, "px; top:", top_css_px-500, "px;")
      }
    }
    # actual tooltip created as a "wellPanel"
    # If the cursor is out of bounds of the figure, then hover is null and no tooltip shown
    
    mouseposcursor <- round(as.numeric(left_px), digits = 2)
    mousepos <- round(as.numeric(left_px), digits = 0)
    
    multiplier <- 1
    trailingsymbol <- ""
    precedingsymbol <- ""
    
    # those categories that need to be multiplied by 100 for a percent
    multstring <- c("Male","Adults_with_No_Children","Individuals_who_Voted","FPL_lessthan_100","Part_Time_Workers","Unemployed")
    if(any(str_detect(multstring, sortinfo$col))){
      multiplier <- 100
      trailingsymbol <- "%"
    } 
    
    millionstring <- c("Gross_State_Product_millions","PCSpending_Elementary_Secondary_Education","PCSpending_Assistance","PCSpending_Corrections")
    if(any(str_detect(millionstring, sortinfo$col))){
      trailingsymbol <- " M"
      precedingsymbol <- "$"
    } else if(sortinfo$col == "Median_Annual_Household_Income"){
      precedingsymbol <- "$"
    }
    
    # special sort depending on category
    if(sortinfo$col == "Male" || 
       #sortinfo$col == "Party" || 
       sortinfo$col == "state_index" || 
       sortinfo$col == "FPL_lessthan_100" || 
       sortinfo$col == "Per_Capita_State_Spending") 
    {  # sorted forwards
      KFF_data_sort <- sorting(KFF_data, sortinfo$col)
    } else {
      KFF_data_sort <- sorting(KFF_data, sortinfo$col, reverse=TRUE)
    }
    
    tooltiptextLine1 <- paste("For <b>", tooltipStateList[mousepos], "</b><br />", sep="")
    tooltiptextLine2 <- paste("with ",sortinfo$label, " at ",precedingsymbol, format(round(toolinfo[mousepos,]*multiplier,digits=2),big.mark=","),trailingsymbol,"<br />", sep="")
    tooltiptextLine3 <- paste("Expected crimes: ", round(tooltipExpected[mousepos],digits=1),"<br />", sep="")
    tooltiptextLine4 <- paste("Observed crimes: ", tooltipObserved[mousepos],"<br />",sep="")
    
    # special removals or modifications for tooltips depending on category
    if(sortinfo$col == "crime_percent_from_expected" || 
       sortinfo$col == "state_index") 
    {  
      tooltiptextLine2 = ""
    } else if(sortinfo$col == "Party" && !is.null(hover)){
      if(toolinfo[mousepos,] == 0){
        tooltiptextLine2 <- paste("with ",sortinfo$label, " at 50-50 Democrat-Republican<br />", sep="")
      } else if(toolinfo[mousepos,] > 0){
        tooltiptextLine2 <- paste("with ",sortinfo$label, " at ", abs(round(toolinfo[mousepos,] * 100, digits = 0)) / 2 + 50 ,"% Republican<br />", sep="")
      } else if(toolinfo[mousepos,] < 0){
        tooltiptextLine2 <- paste("with ",sortinfo$label, " at ", abs(round(toolinfo[mousepos,] * 100, digits = 0)) / 2 + 50 ,"% Democrat<br />", sep="")
      }
    }
    
    tooltiptext <- HTML(paste(tooltiptextLine1, tooltiptextLine2, tooltiptextLine3, tooltiptextLine4, sep=""))
    
    if(is.null(hover)){
      # do nothing
    } else if(mouseposcursor %% 1 < 0.3 || mouseposcursor %% 1 > 0.7) { # if close to a point
      wellPanel(
        style = style,
        p(tooltiptext)
      )
    }
    
    
  })
  
  ######################### Crime and Punishment ################################
  
  # # Turn state names into two-letter codes to compare with WMN data
  statecodes <- state_crime$State 
  statecodes <- state.abb[match(statecodes,state.name)]
  state_crime <- state_crime |>
    mutate(ST = statecodes)
  state_punishment <- state_punishment |>
    mutate(ST = statecodes)
  
  # combine wmn data with state crime data
  wmn_data_crime <- inner_join(KFF_data, state_crime, by = c("State" = "ST")) |> 
    rename(State_long = State.y) |> 
    relocate(State_long, .after = State)
  
  # combine wmn + crime data with punishment data
  wmn_data_crime_punishment <- inner_join(wmn_data_crime, state_punishment, by = c("State" = "ST")) |> 
    select(!State.y)
  
  ############################## subset from inputs for PCA #####################
  
  betterNames <- function(subsetdata_noexp) {
    namesofcrimes <- names(subsetdata_noexp) |> 
      str_replace_all(c("Burglary_rate"="Burglary",
                        "Larceny_rate"="Larceny",
                        "Motor_rate"="Motor Vehicles",
                        "Assault_rate"="Assault",
                        "Murder_rate"="Murder",
                        "Rape_rate"="Rape",
                        "Robbery_rate"="Robbery",
                        "State_Prisons_rate"="State Prisons",
                        "Federal_Prisons_rate"="Federal Prisons",
                        "Local_Jails_rate"="Local Jails",
                        "Youth_Facilities_rate"="Youth Facilities",
                        "Involuntary_Commitment_rate"="Involuntary",
                        "Probation_rate"="Probation",
                        "Parole_rate"="Parole",
                        "crime_percent_from_expected"="Crime Percent\nfrom Expected"
      ))
    return(namesofcrimes)
  }
  
  output$pcaplot <- renderPlot({
    
    # get the checkbox info from both groups
    # subsets <- c("Burglary_rate", "Larceny_rate", "Motor_rate")
    subsets <- paste(input$crime, input$punishment, sep=", ", collapse = ", ")
    subsets <- unlist(str_split(subsets, ", "))
    
    # subset the data for analysis, including crime expected and state (but won't be used)
    subsetdata <- wmn_data_crime_punishment |> 
      select(any_of(subsets), crime_percent_from_expected, State)
    
    # do the PCA on the subset, not including the state (or the crime frequency if not included)
    if(input$include_exp){
      subsetdata_noexp <- subsetdata |> 
        select(-State)
    } else {
      subsetdata_noexp <- subsetdata |> 
        select(-crime_percent_from_expected, -State)
    }
    
    # adjust loading labels
    crimechoiceNames <- betterNames(subsetdata_noexp)
    
    # NO output for PCA with dimensions less than 2
    if(dim(subsetdata_noexp)[2] <= 1){
      output$pcatext <- renderText({
        HTML("PCA requires at least two variables to compare.<br />Please check the input variables to the left.")
      })
      output$pcatext1 <- renderText({
        NULL
      })
    } else {
      output$pcatext <- renderText({
        NULL
      })
      
      wmn_data_crime_punishment.pca <- prcomp(subsetdata_noexp,
                                              center = TRUE,
                                              scale. = TRUE)
      
      pcainfo <- summary(wmn_data_crime_punishment.pca)
      
      # put text below the PCA to help describe
      output$pcatext1 <- renderText({
        paste("The two principal components (this graph) explain ", round(pcainfo$importance[3,2]*100, 1),"% of the variation in the data.", sep="")
      })
      
      wmn_data_crime_punishment.pca.plot <- autoplot(wmn_data_crime_punishment.pca,
                                                     data = subsetdata,
                                                     label = TRUE, 
                                                     label.label = 'State',
                                                     label.size = 4.5,
                                                     shape = FALSE, # no points
                                                     loadings.label = TRUE, 
                                                     loadings.label.repel = T,
                                                     loadings.label.color="black",
                                                     loadings.label.size = 6,
                                                     loadings.label.label = crimechoiceNames,
                                                     color = 'crime_percent_from_expected',
                                                     loadings.colour = 'black') +
        scale_color_gradient(low="blue", high="red", name="Percent Crime\nfrom Expected") +
        theme_light() +
        #xlim(-1, 1) + ylim(-1, 1) +
        theme(
          legend.title = element_text(size=15),
          legend.text = element_text(size=15),
          panel.border = element_blank()
        ) 
      
      # # center the plot
      # xscales <- max(abs(ggplot_build(wmn_data_crime_punishment.pca.plot)$layout$panel_params[[1]]$x.range))
      # yscales <- max(abs(ggplot_build(wmn_data_crime_punishment.pca.plot)$layout$panel_params[[1]]$y.range))
      
      # add titles
      wmn_data_crime_punishment.pca.plot <- wmn_data_crime_punishment.pca.plot +
        labs(
          title = "Principal Components Plot",
          subtitle = "How State Crime and Punishment Relates to Sex Crimes Against Children",
        ) #+
        #xlim(-xscales, xscales) + ylim(-yscales, yscales) 
      
      # make theme prettier
      wmn_data_crime_punishment.pca.plot <- wmn_data_crime_punishment.pca.plot +
        theme(
          panel.background = element_rect(fill = "white", colour = "white"),
          panel.grid = element_line(colour = "grey92"),
          panel.grid.minor = element_line(linewidth = rel(1)),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16),
          axis.title.y = element_text(size=17),
          axis.title.x = element_text(size=17),
          plot.title = element_text( # font size "large"
            size = 20,
            hjust = 0, vjust = 1,
            margin = margin(b = 15/2)
          ),
          plot.subtitle = element_text( # font size "regular"
            size = 15,
            hjust = 0, vjust = 1,
            margin = margin(b = 15/2)
          )
        )
      
      # show the plot
      wmn_data_crime_punishment.pca.plot

    }

  })
  
  output$pcacontributionspine <- renderPlot({
    
    # get the checkbox info from both groups
    # subsets <- c("Burglary_rate", "Larceny_rate", "Motor_rate")
    subsets <- paste(input$crime, input$punishment, sep=", ", collapse = ", ")
    subsets <- unlist(str_split(subsets, ", "))
    
    # subset the data for analysis, including crime expected and state (but won't be used)
    subsetdata <- wmn_data_crime_punishment |> 
      select(any_of(subsets), crime_percent_from_expected, State)
    
    # do the PCA on the subset, not including the state (or the crime frequency if not included)
    if(input$include_exp){
      subsetdata_noexp <- subsetdata |> 
        select(-State)
    } else {
      subsetdata_noexp <- subsetdata |> 
        select(-crime_percent_from_expected, -State)
    }
    
    # adjust loading labels
    crimechoiceNames <- betterNames(subsetdata_noexp)
    
    # NO output for mosaic or text with dimensions less than 2
    if(dim(subsetdata_noexp)[2] <= 1){
      output$pcatext2 <- renderText({
        NULL
      })
    } else {
      output$pcatext2 <- renderText({
        HTML("The first two Principal Components (on the above plot) always 'explain' the most <u>variation</u> in the data (which may or may not be interesting).<br /><br />The plot to the right shows the contributions of the data to the first two components (PC1 and PC2; The variable names are on the left but do not line up with the bars, except in their order).<br /><br />To interpret, if a variable is not well-represented in a component, then it is not well correlated with the other variables in that component.<br /><br />For example, looking at Burglary, Larceny, and State Prison population with the Percent Difference from Expected Crime, the Expected Crime values are nearly alone in PC2, indicating that there is not much correlation to Burglary, Larceny, and State Prison population.")
      })
      
      wmn_data_crime_punishment.pca <- prcomp(subsetdata_noexp,
                                              center = TRUE,
                                              scale. = TRUE)
      # get numbers for chart
      pcacontributions <- get_pca_var(wmn_data_crime_punishment.pca)
      # get colors to then add red only if Crime is included
      pal <- gray.colors
      if(input$include_exp){
        colorlist <- pal(dim(t(pcacontributions$contrib[,1:2]))[2]-1)
        colorlist <- c("#8B0000", colorlist)
      } else {
        colorlist <- pal(dim(t(pcacontributions$contrib[,1:2]))[2])
      }
      
      par(las = 2, mar = c(5, 7, 5, 5))
      spineplot(t(pcacontributions$contrib[,1:2]),
                border = c("#FFFFFF"),
                col=colorlist,
                main = "Contributions to the first two Principal Components", xlab = "", ylab = "",
                xaxlabels = c("PC 1", "PC 2"), 
                yaxlabels = crimechoiceNames)
      
    }
  })
  
  ############################## subset from inputs for KMeans #####################  
  
  output$kmeansplot <- renderPlot({
    
    # get the checkbox info from both groups
    subsets <- paste(input$crime, input$punishment, sep=", ", collapse = ", ")
    subsets <- unlist(str_split(subsets, ", "))
    
    # subset the data for analysis, including crime expected and state (but won't be used)
    subsetdata <- wmn_data_crime_punishment |> 
      select(any_of(subsets), crime_percent_from_expected, State)
    
    # do the PCA/kmeans on the subset, not including the state (or the crime frequency if not included)
    if(input$include_exp){
      subsetdata_noexp <- subsetdata |> 
        select(-State)
    } else {
      subsetdata_noexp <- subsetdata |> 
        select(-crime_percent_from_expected, -State)
    }
    
    # # adjust loading labels
    # crimechoiceNames <- betterNames(subsetdata_noexp)
    
    kmeansnumber <- as.integer(input$kmeansnum)
    
    # NO output for PCA with dimensions less than 2
    if(dim(subsetdata_noexp)[2] <= 1){
      output$kmeanstext <- renderText({
        HTML("K-Means requires at least two variables to compare.<br />Please check the input variables to the left.")
      })
      output$kmeanstext1 <- renderText({
        NULL
      })
    } else {
      output$kmeanstext <- renderText({
        NULL
      })
      
      output$kmeanstext1 <- renderText({
        paste("Someting here about the groups")
      })
    
    set.seed(42)
    k2 <- kmeans(subsetdata_noexp, centers = kmeansnumber, nstart=25, iter.max=500)
      
    kmeansplot <- autoplot(k2, 
                           data = subsetdata, 
                           frame = TRUE,
                           label = TRUE, 
                           label.label = 'State',
                           label.size = 4.5,
                           shape = FALSE) +
      #scale_color_manual(values = c("red", "green", "blue"), name="Clusters") +
      theme_light() +
      theme(
        legend.title = element_text(size=18),
        legend.text = element_text(size=18),
        panel.border = element_blank()
      ) 
      
    
    # add titles
    kmeansplot <- kmeansplot +
      labs(
        title = "K-Means Plot",
        subtitle = "How State Crime and Punishment Are Grouped with Sex Crimes Against Children",
      )
    
    # make theme prettier
    kmeansplot <- kmeansplot +
      theme(
        panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid = element_line(colour = "grey92"),
        panel.grid.minor = element_line(linewidth = rel(1)),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=17),
        axis.title.x = element_text(size=17),
        plot.title = element_text( # font size "large"
          size = 20,
          hjust = 0, vjust = 1,
          margin = margin(b = 15/2)
        ),
        plot.subtitle = element_text( # font size "regular"
          size = 15,
          hjust = 0, vjust = 1,
          margin = margin(b = 15/2)
        )
      )
    
    # show the plot
    kmeansplot
    }
    
  })  
  
  
  output$kmeansSilhouette <- renderPlot({
    
    # get the checkbox info from both groups
    # subsets <- c("Burglary_rate", "Larceny_rate", "Motor_rate")
    subsets <- paste(input$crime, input$punishment, sep=", ", collapse = ", ")
    subsets <- unlist(str_split(subsets, ", "))
    
    # subset the data for analysis, including crime expected and state (but won't be used)
    subsetdata <- wmn_data_crime_punishment |> 
      select(any_of(subsets), crime_percent_from_expected, State)
    
    # do the PCA on the subset, not including the state (or the crime frequency if not included)
    if(input$include_exp){
      subsetdata_noexp <- subsetdata |> 
        select(-State)
    } else {
      subsetdata_noexp <- subsetdata |> 
        select(-crime_percent_from_expected, -State)
    }
    
    #kmeansnumber <- 3
    kmeansnumber <- as.integer(input$kmeansnum)
    
    # NO output for mosaic or text with dimensions less than 2
    if(dim(subsetdata_noexp)[2] <= 1){
      output$kmeanstext2 <- renderText({
        NULL
      })
      # send null to figure
      NULL
      
    } else {
      output$kmeanstext2 <- renderText({
        HTML("The graph to the right in an Average Silhouette that measures the quality of a clustering. The optimal number of clusters is the one that <b>maximizes</b> the average silhouette (max of 1.0) over a range of possible cluster numbers.<br /><br />You should try that maximum value in the 'Number of Groups for K-Means'.")
      })
      
      # function to calculate how many clusters are optimal
      silhouette_score <- function(k){
        set.seed(42)
        km <- kmeans(subsetdata_noexp, centers = k, nstart=25, iter.max=500)
        ss <- silhouette(km$cluster, dist(subsetdata_noexp))
        mean(ss[, 3])
      }
      
      # iterate
      k <- 2:7
      avg_sil <- sapply(k, silhouette_score)
      
      # plot the silhouette 
      plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
      
    }
  })
  
  output$kmeansTable <- DT::renderDataTable({
    
    # get the checkbox info from both groups
    # subsets <- c("Burglary_rate", "Larceny_rate", "Motor_rate")
    subsets <- paste(input$crime, input$punishment, sep=", ", collapse = ", ")
    subsets <- unlist(str_split(subsets, ", "))
    
    # subset the data for analysis, including crime expected and state (but won't be used)
    subsetdata <- wmn_data_crime_punishment |> 
      select(any_of(subsets), crime_percent_from_expected, State)
    
    # do the PCA on the subset, not including the state (or the crime frequency if not included)
    if(input$include_exp){
      subsetdata_noexp <- subsetdata |> 
        select(-State)
    } else {
      subsetdata_noexp <- subsetdata |> 
        select(-crime_percent_from_expected, -State)
    }
    
    #kmeansnumber <- 3
    kmeansnumber <- as.integer(input$kmeansnum)
    
    # NO output for mosaic or text with dimensions less than 2
    if(dim(subsetdata_noexp)[2] <= 1){
      output$kmeanstext1 <- renderText({
        NULL
      })
      
      # send data table null
      NULL
    } else {
      output$kmeanstext1 <- renderText({
        HTML("Examine the table below. It shows the mean values for each variable.  Look across the rows (clusters) to get a feeling for what each cluster 'represents' and how each cluser is different or the same as the others.")
      })
      
      k2 <- kmeans(subsetdata_noexp, centers = kmeansnumber)
      
      # Extract the clusters and add to our initial data to do some descriptive statistics at the cluster level:
      meangroups <- subsetdata_noexp |> 
        mutate(Cluster = k2$cluster) |> 
        group_by(Cluster) |> 
        summarise_all("mean") |> 
        mutate(across(is.numeric, round, digits = 1))
      
      # adjust loading labels
      crimechoiceNames <- betterNames(subsetdata_noexp)
      names(meangroups)[2:length(names(meangroups))] <- crimechoiceNames
      
      # display the table
      datatable(meangroups, options = list(autoWidth = TRUE,
                                           order = list(0, 'asc'),
                                           ordering = FALSE,
                                           dom = 't'
      ), 
      rownames = FALSE,
      colnames = names(meangroups)
      ) 
      
    }
  })
      
  
}))


shinyApp(ui, server)

