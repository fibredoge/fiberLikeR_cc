library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyalert) # for popup messages

library(conflicted)

library(tidyverse)
conflict_prefer("filter", "dplyr")
conflict_prefer("dashboardHeader", "shinydashboardPlus")
conflict_prefer("dashboardSidebar", "shinydashboardPlus")
conflict_prefer("box", "shinydashboardPlus")
conflicts_prefer(shinydashboardPlus::notificationItem)

library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(bslib) # for popup thing and colours
library(mathjaxr)
library(fontawesome)
library(purrr)
library(tippy) # tooltip hover
library(shinyBS)
library(shinybusy) # test server busy spinner

# install.packages("tinytex")  # install LaTeX to generate PDF report
# tinytex::install_tinytex()
library(tinytex)

options(scipen=999)

# HELP & INTRO DATA ---------------------------------------------------------------
steps <- read_csv2("help.csv",
                   show_col_types = FALSE)

scenario <- read_csv2("scenario.csv",
                      show_col_types = FALSE)

help_me <- controlbarMenu(
  id = "helpmenu",
  
  controlbarItem(
    title = NULL,
    icon = icon("subscript"),
    div(HTML("
         <h4> Glossary of Mathematical Terms </h4>
         <table>
         <tr>
         <th>Term</th>
         <th>  Definition</th>
         </tr>
         <tr><td><b><i>b0</td></b></i><td>Background probability of 0 fibres</td></tr>
          <tr><td><b><i>bn</td></b></i><td>Background probability of n fibres</td></tr>
          <tr><td><b><i>gamma</td></b></i><td>Match probability</td></tr>
          <tr><td><b><i>t0</td></b></i><td>Transfer probability of 0 fibres</td></tr>
          <tr><td><b><i>tn</td></b></i><td>Transfer probability of n fibres under Hp</td></tr>
          <tr><td><b><i>tdn</td></b></i><td>Transfer probability of n fibres under Hd</td></tr>
         </table>
         ")
    )
  ),
  
  controlbarItem(
    title = NULL,
    icon = icon("comment-dots"),
    div(HTML("
         <h4> Verbal Scale for Expression of LR </h4>
         <table>
         <tr>
         <th>LR Value</th>
         <th>  Verbal Scale of Support</th>
         </tr>
         <tr><td><1 - 10</td><td>Weak</td></tr>
          <tr><td>10 - 100</td><td>Moderate</td></tr>
          <tr><td>100 - 1000</td><td>Moderately strong</td></tr>
          <tr><td>1000 - 10000</td><td>Strong</td></tr>
          <tr><td>10000 - 1000000</td><td>Very strong</td></tr>
          <tr><td>>1000000</td><<td>Extremely strong</td></tr>
         </table>
         <br>
         <br>
         <p><b>Association of Forensic Science Providers (2009)</b>. Standards for the formulation of evaluative forensic science expert opinion. <i>Science & Justice</i>, 49(3), 161-164. </p>
         ")
        )
    ),
  
  controlbarItem(
    title = NULL,
    icon = icon("envelope"),
    "Feedback and questions: ",
    tags$a("victoria.lau@uts.edu.au",
           href="mailto:victoria.lau@uts.edu.au"
           )
    )
  )



# DATASETS ---------------------------------------------------------------
# load in data - HC.boot.summstats saved as csv and xlsx in wd
tfdata <- "Data/HC.boot.summstats.xlsx"
dataset <- read_excel(tfdata)
HC.boot.summstats <- read_excel("Data/HC.boot.summstats.xlsx")


# list of incident types
incidents <-  list(
  "forceful physical contact /assault" = "assault",
  "sitting" = "sitting",
  "smother" = "smother"
)

# list of scene versions
scene_versions <- list(
  "Max denies any knowledge of or involvement with Joe." = "v.A",
  "Max claims he was at the pub across the road and saw someone else tackle Joe." = "v.B",
  "Max recalls walking into someone with a yellow T-shirt whilst trying to find a friend at the bar." = "v.C"
)

# defining Hd
source.X <- list("Source is X" = "yes_X",
                 "Source is NOT X" = "no_X")


# Create variables for each formula with mathematical notation
scene_formulas <- list(
  A = "$$LR_{scene_A} = \\frac{b_0 \\cdot t_n}{b_n \\cdot \\gamma}$$",
  B = "$$LR_{scene_B} = \\frac{t_n}{\\gamma \\cdot td_n}$$",
  Bfull = "$$LR_{scene_{B.full}} = \\frac{b_0 \\cdot t_n + b_n \\cdot \\gamma \\cdot t_0}{b_0 \\cdot \\gamma \\cdot td_n + b_n \\cdot \\gamma \\cdot td_0}$$",
  C = "$$LR_{scene_C} = \\frac{b_0 \\cdot t_n + b_n \\cdot \\gamma \\cdot t_0}{b_0 \\cdot td_n + b_n \\cdot \\gamma \\cdot td_0}$$",
  C1 = "$$LR_{scene_{C1}} = \\frac{b_0 \\cdot t_n}{b_0 \\cdot td_n + b_n \\cdot \\gamma \\cdot td_0}$$",
  C2 = "$$LR_{scene_{C2}} = \\frac{t_n}{td_n}$$"
)


# customise valueBox function - see https://jkunst.com/blog/posts/2020-06-26-valuebox-and-sparklines/
valueBox2 <-
  function(value,
           title,
           subtitle,
    icon = NULL,
           color = "aqua",
           width = 4,
           href = NULL) {
    shinydashboard:::validateColor(color)
    
    if (!is.null(icon))
      shinydashboard:::tagAssert(icon, type = "i")
    
    boxContent <- div(
      class = paste0("small-box bg-", color),
      div(class = "inner",
          tags$small(title),
          h3(value),
          p(subtitle)),
      if (!is.null(icon))
        div(class = "icon-large", icon)
    )
    
    if (!is.null(href))
      boxContent <- a(href = href, boxContent)
    
    div(class = if (!is.null(width))
      paste0("col-sm-", width),
      boxContent)
  }

