ui <- function(req) {
  shinydashboardPlus::dashboardPage(
    # HEADER ----
    header = dashboardHeader(
      title = span(img(src = "rounded_aqua5.png", height = 35), "fiberLikeR: activity level LR app"),
      titleWidth = 400,
      controlbarIcon = icon("question"), # change icon for right sidebar / controlbar
      
      # help drop down
      dropdownMenu(type = "notifications",
                   headerText = strong("NAVIGATION HELP"),
                   icon = icon("route"),
                   badgeStatus = NULL,
                   notificationItem(text = steps$text[1],
                                    icon = icon("home")
                                    ),
                   notificationItem(text = steps$text[2],
                                    icon = icon("calculator")
                                    ),
                   notificationItem(text = steps$text[3],
                                    icon = icon("calculator")
                                    ),
                   notificationItem(text = steps$text[4],
                                    icon = icon("download")
                                    )
                   ),

      dropdownMenu(type = "notifications",
                   headerText = strong("CASE SCENARIO"),
                   icon = icon("masks-theater"),
                   badgeStatus = NULL,
                   notificationItem(text = scenario$text[1],
                                    icon = icon("masks-theater")
                                    ),
                   notificationItem(text = scenario$text[2],
                                    icon = icon("magnifying-glass")
                                    )
                   )
                   
      ),
    
    # SIDEBAR  ----
    sidebar = dashboardSidebar(minified = FALSE,
                               collapsed = FALSE,
                               sidebarMenu(menuItem("Home", tabName = "home", icon = icon("home")),
                                           menuItem("LR Calculator",
                                                    tabName = "LRcalc",
                                                    icon = icon("calculator"),
                                                    menuItem("1. Case & Examination Details",
                                                             tabName = "LRcalc_1"),
                                                    menuItem("2. LR Calculation",
                                                             tabName = "LRcalc_2")
                                                    ),
                                           menuItem("Appendices",
                                                    tabName = "Appx",
                                                    icon = icon("magnifying-glass-chart", lib = "font-awesome"),
                                                    menuItem("System Values",
                                                             tabName = "Appx_1",
                                                             icon = icon("laptop-code", lib = "font-awesome")),
                                                    menuItem("References",
                                                             tabName = "Appx_2",
                                                             icon = icon("book", lib = "font-awesome")
                                                             )
                                                    )
                                           )
                               ),
    # BODY ----
    body = dashboardBody(
      shinyjs::useShinyjs(),
      tags$head(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML"),
        tags$style('#Cscenes_Q2 .box-header{ display: none}'),
        tags$link(
          rel = "stylesheet",
          type = "text/css",
          href = "fiberliker_style.css"
        )
      ),
      
      tags$style(HTML(".mathjax-valuebox { font-size: px; }")),
      
      tabItems(
        # 1. HOME TAB ----
        ## row 1: box for image / uni logo:----
        tabItem(tabName = "home",
                fluidRow(
                  column(width = 12,
                         box(id = 'bannerpic',
                             width = NULL,
                             uiOutput('some_banner'),
                             tags$figcaption(tags$i("Photo by L. Summer from Pexels")),
                             tags$head(tags$style('#bannerpic .box-header{ display: none}'))
                             )
                         )
                  ),
                
                ## row 2: instructions----
                fluidRow(
                  column(width = 6,
                         box(width = NULL,
                             collapsible = TRUE,
                             title = span( icon("circle-info"), "Application Overview"),
                             status = "teal",
                             uiOutput('intro_1'))
                         ),
                  
                  column(width = 6,
                         box(width = NULL,
                             title = span( icon("route"), "How to navigate"),
                             status = "teal",
                             collapsible = TRUE,
                             uiOutput('intro_2')
                             )
                         )
                  ),
                
                ## row 3: scenario----
                fluidRow(
                  column(width = 6,
                         box(width = NULL,
                             collapsible = TRUE,
                             title = span( icon("masks-theater"), "The scenario"),
                             status = "purple",
                             uiOutput('intro_3')
                             )
                         ),
                  column(width = 6,
                         box(title = span( icon("copy"), "Versions of events"),
                             status = "purple",
                             width = NULL,
                             # dummy tick boxes to mark version to evaluate
                             prettyRadioButtons(inputId = "check.version",
                                                label = "Read the following version of events and mark one to proceed to evaluate:",
                                                choices = scene_versions,
                                                selected = FALSE,
                                                icon = icon("check"),
                                                bigger = TRUE,
                                                status = "info",
                                                animation = "jelly")
                             )
                         )
                  )
                ),
        # 2, SECOND TAB - LR calculator ----
        # subtab 1: page one ----
        tabItem(tabName = "LRcalc_1",
                tippy::tippy_this(elementId = "trace.type",
                                  tooltip = "Generic class of trace fibre",
                                  placement = "right"),
                
                ## row 1: case details ----
                add_busy_spinner(spin = "fading-circle",
                                 margins = c(10,10)), # automated server busy spinner
                
                fluidRow(
                  column(width = 4,
                         box(width = NULL,
                             title = "1. Case Details",
                             status = "info",
                             "Enter available information about the case circumstances.",
                             br(),
                             br(),
                             textAreaInput(inputId = "casenotes",
                                           label = "Case notes:",
                                           placeholder = "Summarise case information and other notes ...",
                                           rows = 3),
                             actionButton(inputId = "savenotesButton", label = "Save case notes"),
                             br(),
                             br(),
                             uiOutput("tt_incident"),
                             radioButtons(inputId = "incident.type",
                                          label = NULL,
                                          choices = incidents,
                                          selected = FALSE),
                             uiOutput("tt_control"),
                             pickerInput(inputId = "control_type",
                                         label = NULL,
                                         selected = NULL,
                                         multiple = FALSE,
                                         choices = c("", list(Textile = c("garment", "seating", "soft_furnishing", "another_category"),
                                                              Other = c("head hair", "skin", "hard_surface", "other"))
                                                     )
                                         ),
                             conditionalPanel(condition = "input$control_type == 'garment'", # show when alternative transfer event is selected
                                              uiOutput("control_shape")),
                             conditionalPanel(condition = "input$control_type == 'garment'",
                                              uiOutput("control_shed"))
                             )
                         ),
                  ## row 1: trace details ----
                  column(width = 4,
                         box(width = NULL,
                             collapsible = TRUE,
                             title = "3. Trace Details",
                             status = "info",
                             "Enter any relevant notes regarding examination and/or analysis.",
                             br(),
                             br(),
                             textAreaInput(inputId = "examnotes",
                                           label = "Examination notes:",
                                           placeholder = "Enter pertinent examination and/or analysis notes ...",
                                           rows = 3),
                             actionButton(inputId = "saveexamButton", label = "Save examination notes"),
                             br(),
                             br(),
                             fluidRow(
                               column(width = 6,
                                      
                                      uiOutput("tt_type"),
                                      selectInput(inputId = "trace.type",
                                                  label = NULL,
                                                  choices = c("", list("cotton", "man-made", "vegetable", "other")),
                                                  selected = NULL,
                                                  multiple = FALSE)
                                      ),
                               column(width = 6,
                                      selectInput(inputId = "trace.colour",
                                                  label = "Colour:",
                                                  choices = c("", list("black","blue","red","green","purple","yellow","orange","brown")),
                                                  selected = NULL,
                                                  multiple = FALSE)
                                      )
                               ),
                             br(),
                             shinyWidgets::switchInput(inputId = "trace.dir",
                                                     label = "Direction of transfer",# labelWidth = "100px",
                                                     onLabel = "FROM offender",
                                                     offLabel = "TO offender",
                                                     size = "mini"),
                             
                             ### n recovered to calculate tn.sys ---------
                             sliderInput(inputId = 'n.recovered.slider',
                                         label = 'Number of recovered trace fibres:',
                                         min = 200,
                                         max = 3000,
                                         value = 1000),
                             textInput(inputId = 'textValue',
                                       value = 1000,
                                       label = NULL),
                             actionBttn(inputId = "Set",
                                        "Confirm",
                                        style = "jelly",
                                        color = "success",
                                        size = "xs"),
                             textOutput("value") # Paste the 'Set' value
                             )
                         ),
                  ## row 1: recipient  ----
                  column(width = 4,
                         box(width = NULL,
                             title = "4. Recipient Properties",
                             status = "primary",
                             "Enter available information about recipient surface.",
                             br(),
                             br(),
                             pickerInput(inputId = "select.r.type",
                                         label = "Type of recipient substrate",
                                         selected = NULL,
                                         multiple = FALSE,
                                         choices = c("", list(Textile = c("garment", "seating", "soft_furnishing", "another_category"),
                                                              Other = c("head hair", "skin", "hard_surface", "other"))
                                                     )),
                             conditionalPanel(condition = "input$select.r.type == 'garment'", # show when alternative transfer event is selected
                                            uiOutput("recipient.shape")),
                             conditionalPanel(condition = "input$select.r.type == 'garment'",
                                              uiOutput("recipient.retain"))
                             )
                         )
                  ),
                ##row 2:activity propositions ----
                fluidRow(
                  column(width = 6,
                         box(width = NULL,
                             collapsible = TRUE,
                             title = "2. Activity Level Propositions",
                            status = "teal",
                            useSweetAlert(),
                            "Formulate and express Hp / Hd to guide LR calculation",
                            textInput(inputId = "enter.Hp",
                                      label = div(HTML("<i>Hp: </i>")),
                                      placeholder = "Enter Hp here ..."),
                            actionButton(inputId = "saveHpButton", label = "Save Hp"),
                            hr(),
                            "Given available background case information, formulate and express Hd.",
                            br(),br(),
              
              prettyRadioButtons(
                inputId = "Hd.alt",
                label = "Under Hd there is: ",
                choices = list(
                  "no alternative transfer event (no counterclaim)" = 'no_alt',
                  "an alternative transfer event (activity or actor disputed)" = 'yes_alt'
                ),
                selected = NULL,
                icon = icon("check"),
                bigger = TRUE,
                status = "success",
                animation = "jelly"
              ),
              
              
              conditionalPanel(condition = "input$Hd.alt == 'yes_alt'", # show when alternative transfer event is selected
                               uiOutput("Hd_Xsource")),
              
              br(),
              textInput(
                inputId = "enter.Hd",
                label = div(HTML("<i>Hd: </i>")),
                placeholder = "Enter Hd here ..."
              ),
              
              actionButton(inputId = "saveHdButton", label = "Save Hd"),
              
              useShinyjs()
              
            )
          ),
          
          ## row 2a: Summary ----
          column(
            width = 6,
            box(
              width = NULL,
              title = span( icon("flag-checkered"), "Summary"),
              "Given the following user input, the corresponding LR / Bayes formula is displayed below.",
              br(),
              br(),
              
              textOutput("selected_Hd.alt"),
              # reactive output as input on previous tab
              br(),
              textOutput("user.Hp"),
              textOutput("user.Hd"),
              br(),
              textOutput("no.recovered"),
              br(),
              uiOutput("LR_scene"),
              br(),
              verbatimTextOutput("user.casenotes")
            )
          ),
          
          ## row 2b: BF ----
          column(
            width = 4,
            uiOutput("Cscenes_Q1"),
            uiOutput("Cscenes_Q2"),
            uiOutput("Cscenes_selected")
          )
          )
        ),
        
        #subtab 2: page two ----
        
        tabItem(
          tabName = "LRcalc_2",
          
          # First row
          ## box D ----
          fluidRow(
          
          column(width = 8,
            valueBoxOutput("LR_full",
                           width = 12)
          ),
          
          column(width = 4,
            valueBoxOutput("system_value",
                           width = 12),
            valueBoxOutput("user_calculation",
                           width = 12)
          )
          ),
          
          ##box E ----
          fluidRow(
              uiOutput("override_parameters"),
              
              column(width = 4,
              box(
                title = "User-entered values",
                width = 12,
                column(
                  width = 12,
                  style = 'padding:0px',
                  
                  textOutput("confirm_user_tn"),
                  textOutput("confirm_user_tdn"),
                  textOutput("confirm_user_td0"),
                  textOutput("confirm_user_t0"),
                  textOutput("confirm_user_bn"),
                  textOutput("confirm_user_b0"),
                  textOutput("confirm_user_gamma"),
                  
                  uiOutput("calc_parameters")
                  
                )
              ),
              div(
                uiOutput("tt_pdf"),
                style = 'padding: 1px 15px 1px 15px;'
                ),
              ### download summary HTML -----
              div(
                downloadButton("report", "Download summary document .HTML", class = 'downloadbutton'), 
                style = 'padding:5px 12px 5px 12px;'
              ),
              tags$head(tags$style(".downloadbutton{background-color:#eb34e5; 
                                                    color: #ffffff;
                                                    border-radius: 200px;} 
                                   ")),
              

              ### download summary PDF -----
              
              div(
                downloadButton("report_pdf", "Download summary document .PDF", class = 'downloadbutton'), 
                style = 'padding:12px'
              ),
              tags$head(tags$style(".downloadbutton{background-color:#eb34e5; 
                                                    color: #ffffff;
                                                    border-radius: 200px;} 
                                   "))
              )
          )
          ),
        
        
        # 3. THIRD TAB - Appendices ----
        tabItem(tabName = "Appx_1",
                ## box A ----
                fluidRow(
                  column(
                    width = 4,
                    box(
                      width = NULL,
                      title = "System values",
                      status = "info",
                      collapsible = TRUE,
                      "System-selected values:",
                      # all the defined parameters:
                      textOutput("tn_output"),
                      textOutput("bn_output"),
                      textOutput("b0_output"),
                      textOutput("gamma_output"),
                      textOutput("td0_output"),
                      textOutput("t0_output"),
                      textOutput("tdn_output")
                    )
                  ),
                  ## box B ----
                  column(
                    width = 4,
                    box(
                      width = NULL,
                      title = "LR formulae",
                      status = "info",
                      collapsible = TRUE,
                      "all formulae used for LR in scenarios",
                      withMathJax(
                        "$$LR_{full} = \\frac{{(b_0 \\cdot t_n) + (b_n \\cdot \\gamma \\cdot t_0)}}{{(b_0 \\cdot \\gamma \\cdot t_{d_n}) + (b_n \\cdot \\gamma \\cdot t_{d_0})}}$$"
                      ),
                      withMathJax(
                        "$$LR_{scene_A} = \\frac{b_0 \\cdot t_n}{{b_n \\cdot \\gamma}}$$"
                      ),
                      withMathJax(
                        "$$LR_{scene_B} = \\frac{b_0 \\times t_n}{{b_0 \\times \\gamma \\times t_{d_n}}}$$"
                      ),
                      withMathJax(
                        "$$LR_{scene_C1} = \\frac{b_0 \\cdot t_n}{{b_0 \\cdot t_{d_n} + b_n \\cdot \\gamma \\cdot t_{d_0}}}$$"
                      ),
                      withMathJax("$$LR_{scene_C2} = \\frac{t_n}{{t_{d_n}}}$$")
                                         )
                  ),
                  ## box C ----
                  column(
                    width = 4,
                    box(
                      width = NULL,
                      title = "",
                      status = "primary",
                      collapsible = TRUE,
                      "Results of LR calculation with no user override",
                      textOutput("LR_sceneB_full_result_sys"),
                      textOutput("LR_sceneA_result_sys"),
                      textOutput("LR_sceneB_result_sys"),
                      textOutput("LR_sceneC1_result_sys"),
                      textOutput("LR_sceneC2_result_sys"),
                     
                      hr(),
                      textOutput("LR_scene_formula_dyn"),
                      
                      hr()
                      
                    )
                  )
                ),
                
                fluidRow(
                  column(
                    width = 8,
                    box(width = NULL,
                        title = "Transfer Probability (tn)",
                        status = "info",
                        collapsible = TRUE,
                        "Transfer probability (tn) values for hoody cotton from experimental re-enactments",
                        br(),
                        br(),
                        plotlyOutput('tnplot')
                         )))),
        
        ## References ----
        tabItem(tabName = "Appx_2",
                fluidRow(
                  column(
                    width = 12,
                    box(title = "Literature References",
                        width = NULL,
                  "Reference values for LR calcuation sourced from:",
                  
                  div(HTML("<b>Schnegg, M., Turchany, M., Deviterne, M., Gueissaz, Hess, S., & Massonnet, G. (2017). </b>A preliminary investigation of textile fibers in smothering scenarios and alternative legitimate activities. <i>Forensic Science International, 279, </i>165-176. https://doi.org/10.1016/j.forsciint.2017.08.020
")),
                  br(),
                  div(HTML("<b>Watt, R., Roux, C., & Robertson, J. (2005). </b>The population of coloured textile fibres in domestic washing machines. <i>Science and Justice, 45, </i>75-83. 
")),
                  br(),
                  div(HTML("<b>Lau, V., Spindler, X., & Roux, C. (2023). </b>The transfer of fibres between garments in a choreographed assault scenario. 
                  <i>Forensic Science International, 349</i>(349C), 111746. https://doi.org/https://doi.org/10.1016/j.forsciint.2023.111746")
                      )
                  )
                  )
                  )
                )
        ),
      # RHS CONTROLBAR ----
      # help menu 
      dashboardControlbar(
        collapsed = TRUE,
        icon = icon("question"),
        width = 400,
        skin = "dark",
        help_me # controlbarMenu in global.r
      ),
      title = NULL
      )
   )
}
