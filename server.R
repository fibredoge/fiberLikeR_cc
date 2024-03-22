server <- function(input, output, session)
{
  
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_popup.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "getstarted", label = "START", icon = icon("circle-play"))
      )
    ))
  })
  
  observeEvent(input$getstarted,{
    removeModal()
  })
  
  
  output$some_banner = renderUI({
    tags$img(src = "banner1.jpg",
             align = "center",
             width = "100%")
    # tags$figcaption("Photo by L. Summer from Pexels")
   })

  # second row LH box

   output$intro_1 <- renderUI({
      HTML(
         '<script src="https://kit.fontawesome.com/5bfd0f34f2.js" crossorigin="anonymous"></script>

         <p> You will be presented with a case example involving the transfer of textile fibres. From this main scenario, there are three sub-scenarios or versions of events. </p>
         <p> The goals of this activity are to:
         <ul>
         <li> formulate a pair of competing activity-level propositions corresponding to a given sub-scenario </li>
         <li> understand and consider effects of relevant factors in the evaluation of fibre findings  </li>
         <li> select appropriate terms and values [from experience and research] in calculating a likelihood ratio (LR) </li>
         <li> develop and/or reinforce competence and confidence in evaluative reporting at the activity level </li>
         </ul>
         <p> </p>
         <br>

         '
      )
   })
   
   output$intro_2 <- renderUI({
     HTML('
     <script src="https://kit.fontawesome.com/5bfd0f34f2.js" crossorigin="anonymous"></script>
     <ol>
     <li> Read the main scenario and select a sub-scenario version (A / B / C) to evaluate.</li>
     <li>Under "LR Calculator" in the sidebar, navigate to tab 1 to enter case and results of laboratory examination. The system will provide a LR formula based on your formulation of activity level propositions.</li>
     <li>Navigate to tab 2 (LR Calculation) to review values for formula terms </li>
     <li>Download summary document as HTML or PDF.</li>
     <br></br>
     <p> Navigation guide can also be accessed by clicking on the <i class="fa-solid fa-route"></i> icon in the header bar. </p>
     </ol>
          ')
   })
   
   output$intro_3 <- renderUI({
      HTML(
         'On Saturday 21st March, Joe Citizen attended a major social function at a prime licensed venue. Shortly after arriving, he exited the venue to retrieve something
         from his car when he was tackled and assaulted by a man wearing a red hoody. Security and police quickly attended the scene. A few hours later, a man matching the description,
         Max Mustermann, was taken into custody and his garments including a red hoody and brown chinos were collected. Joe also submitted his yellow Minions T-shirt for examination.

         <br>

         As the forensic scientist tasked with performing analyses and interpreting findings relating to this case, you have recovered a number of foreign fibres
         from Joes T-shirt. '
      )
   })
   
   
   # Function to disable options "sitting" and "smother"
   observe({
      if (!is.null(input$incident.type)) {
         if (input$incident.type %in% c("sitting", "smother")) {
            # If "sitting" or "smother" is selected, disable them
            choices <- incidents
            choices[c("sitting", "smother")] <-
               lapply(choices[c("sitting", "smother")], function(choice) {
                  paste0(
                     '<input type="radio" name="incident.type" value="',
                     choice,
                     '" disabled>',
                     choice
                  )
               })
            updateRadioButtons(session,
                               "incident.type",
                               label = NULL,
                               choices = choices)
         } else {
            # If any other option is selected, enable all options
            updateRadioButtons(session,
                               "incident.type",
                               label = NULL,
                               choices = incidents)
         }
      }
   })
   
   # tooltip hovers ----

   output$tt_control <- renderUI({
     tags$span(
       HTML("<b>Type of Control item / substrate (X):</b>"),
       popify(icon("info-circle", verify_fa = FALSE),
              title = NULL,
              "Reference or known (X)")
     )
   })
   
   
   output$tt_type <- renderUI({
     tags$span(
       HTML("<b>Fibre type</b>"),
       popify(icon("info-circle", verify_fa = FALSE),
              title = NULL,
              "Generic fibre type")
     )
   })
   
   output$tt_pdf <- renderUI({
     tags$span(
       popify(icon("circle-exclamation", verify_fa = TRUE),
              title = NULL,
              "interactive plots currently retain functionality only in HTML")
     )
   })
   
   output$tt_incident <- renderUI({
     tags$span(
       HTML("<b>Nature of alleged activity:</b>"),
       popify(icon("info-circle", verify_fa = FALSE),
              title = NULL,
              "not all options are functional but provide opportunity for future development")
     )
   })


   
   
   # subtab 1: LR calc----
   # user enters free text for case notes
   casenotes_val <- reactiveVal()
   examnotes_val <- reactiveVal()
   
   
    ## Observe casenotes input ----
   # If input is empty, disable "Save case notes" button, and enable on valid input
   observe({
     if (is.null(input$casenotes) || input$casenotes == "") {
       shinyjs::disable("savenotesButton")
     } else {
       shinyjs::enable("savenotesButton")
     }
   })
   
   ## Observe 'Save case notes' button ----
   observeEvent(input$savenotesButton, {
     casenotes_val(input$casenotes)
     
     # if casenotes_val() is empty, show error alert
     if (casenotes_val() == "") {
       shinyWidgets::sendSweetAlert(
         session = session,
         title = "Error:",
         text = "Please enter case notes to save",
         type = "error"
       )
     }
     # else, show success message
     else {
       sendSweetAlert(
         session = session,
         title = "Success!",
         text = "Case notes saved!",
         type = "success"
       )
     }
   })
   
   output$user.casenotes <- renderText({
     paste0("Case notes: ", casenotes_val())
   })
   
   
   ## Observe examnotes input ----
   # If input is empty, disable "Save exam notes" button, and enable on valid input
   observe({
     if (is.null(input$examnotes) || input$examnotes == "") {
       shinyjs::disable("saveexamButton")
     } else {
       shinyjs::enable("saveexamButton")
     }
   })
   
   ## Observe 'Save exam notes' button ----
   observeEvent(input$saveexamButton, {
     examnotes_val(input$examnotes)
     
     # if examnotes_val() is empty, show error alert
     if (examnotes_val() == "") {
       shinyWidgets::sendSweetAlert(
         session = session,
         title = "Error:",
         text = "Please enter examination notes to save",
         type = "error"
       )
     }
     # else, show success message
     else {
       sendSweetAlert(
         session = session,
         title = "Success!",
         text = "Examination notes saved!",
         type = "success"
       )
     }
   })
   
   output$user.examnotes <- renderText({
     paste0("Examination notes: ", examnotes_val())
   })
   
   
   output$Hd_Xsource <- renderUI({
      if (input$Hd.alt == 'yes_alt') {
         prettyRadioButtons(
            inputId = "Hd.Xsource",
            label = NULL,
            choices = c("Source is X (activity disputed)" = "yes_X",
                        "Source is NOT X (actor disputed)" = "no_X"),
            selected = NULL,
            icon = icon("check"),
            bigger = TRUE,
            status = "success",
            animation = "jelly"
         )
      }
   })

   output$control_shape <- renderUI({
     req(input$control_type)
     if (input$control_type == 'garment') {
       pickerInput(
         inputId = "control_shape",
         label = NULL,
         choices = c("hoody", "T-shirt", "blouse", "jumper"),
         options = list(title = "Select shape / style")
       )
     }
   })
   
  output$control_shed <- renderUI({
    req(input$control_type)
    if (input$control_type == 'garment') {
      radioButtons(
        inputId = "control_shed",
        label = "Sheddability of Control Item",
        choices = c("low", "moderate", "high", "indeterminate"),
        selected = FALSE
      )
    }
  })
   
   output$recipient.shape <- renderUI({
      if (input$select.r.type == 'garment') {
         pickerInput(
            inputId = "r.shape",
            label = NULL,
            choices = c("hoody", "T-shirt", "blouse", "jumper"),
            options = list(title = "Select shape / style")
         )
      }
   })
   
   output$recipient.retain <- renderUI({
     if (input$select.r.type == 'garment') {
       radioButtons(
         inputId = "r.retain",
         label = "Retentive Capacity of Recipient surface",
         choices = c("low", "moderate", "high", "indeterminate"),
         selected = FALSE
       )
     }
   })
   
   
   
   # sync slider and text inputs
   observeEvent(input$textValue, {
      print(input$textValue)
      if ((as.numeric(input$textValue) != input$n.recovered.slider) &
          input$textValue != "" &  input$n.recovered.slider != "")
      {
         updateSliderInput(
            session = session,
            inputId = 'n.recovered.slider',
            value = input$textValue
         )
      } else {
         if (input$textValue == "") {
            updateSliderInput(session = session,
                              inputId = 'n.recovered.slider',
                              value = 0)
         }
      }
   })
   
   observeEvent(input$n.recovered.slider, {
      if ((as.numeric(input$textValue) != input$n.recovered.slider) &
          input$n.recovered.slider != "" & input$textValue != "")
      {
         updateTextInput(
            session = session,
            inputId = 'textValue',
            value = input$n.recovered.slider
         )
      }
   })
   
   
   
   ## user phrases Hp/Hd ------------
   hp_val <- reactiveVal()
   hd_val <- reactiveVal()
   
   
   ## Observe Hp input ----
   # If input is empty, disable "Save Hp" button, and enable on valid input
   observe({
      if (is.null(input$enter.Hp) || input$enter.Hp == "") {
         shinyjs::disable("saveHpButton")
      } else {
         shinyjs::enable("saveHpButton")
      }
      
      if (is.null(input$enter.Hd) || input$enter.Hd == "") {
         shinyjs::disable("saveHdButton")
      } else {
         shinyjs::enable("saveHdButton")
      }
   })
   
   ## Observe 'Save Hp' button ----
   # When the 'Save Hp' button is clicked, save the input value for Hp
   observeEvent(input$saveHpButton, {
      # save hp input value to reactive value
      hp_val(input$enter.Hp)
      
      # if hp_val() is empty, show error alert
      if (hp_val() == "") {
         shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error:",
            text = "Please enter Hp value",
            type = "error"
         )
      }
      # else, show success message
      else {
         sendSweetAlert(
            session = session,
            title = "Success!",
            text = "Prosecution proposition (Hp) saved!",
            type = "success"
         )
      }
   })
   
   observeEvent(input$Set, {
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "Number of recovered trace fibres (n) saved!",
         type = "success"
      )
   })
   
   
   # When the 'Save Hp' button is clicked, save the input value for Hd
   observeEvent(input$saveHdButton, {
      hd_val(input$enter.Hd)
   })
   # When the 'Save Hd' button is clicked, save the input value for Hd
   observeEvent(input$saveHdButton, {
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "Defence proposition (Hd) saved!",
         type = "success"
      )
   })
   
   
   output$selected_Hd.alt <- renderText({
      if (input$Hd.alt == "no_alt") {
         paste0("Under Hd, there is no alternative transfer event.")
      } else {
        req(input$Hd.Xsource)
         if (input$Hd.Xsource == "yes_X") {
            paste0("Under Hd, there is an alternative transfer event, and the source is X. (actor NOT disputed)")
         } else if (input$Hd.Xsource == "no_X") {
            paste0("Under Hd, there is an alternative transfer event, and the source is NOT X. (actor in dispute)")
         } else {
            "Please select an option for Hd_Xsource."
         }
      }
   })
   
   output$user.Hp <- renderText({
      paste0("Hp: ", hp_val())
   })
   
   output$user.Hd <- renderText({
      paste0("Hd: ", hd_val())
   })
   
   
   output$no.recovered <- renderText({
      paste0("Number of recovered fibres: ", input$n.recovered.slider)
   })
   
   
   ## tn reactivity ------------
   # Define reactive expression for tn - will work with mathjax
   
   tn_value <- reactive({
     dataset$mean.prob[dataset$no.fibres == input$n.recovered.slider]
   })
   
   ### other parameter values ----
   
   # tdn.sys <- 0.001
   # tdn.sys <- reactive({
   #   if (chosen_scene == "A"){
   #     0.001
   #   }  else {
   #     # req(input$Hd.Xsource)
   #     if (chosen_scene == "c") {
   #       0.0008
   #     } else if (chosen_scene == "C1") {
   #       0.0008
   #     } else if (chosen_scene == "C2") {
   #       0.0008
   #     } else if (chosen_scene == "B") {
   #       0.001
   #     }
   #     }
   # })

   # change tdn depending on scene chosen; 

   tdn.sys <- reactive({
     req(input$Hd.Xsource)
     if (input$Hd.Xsource == "yes_X") { # scene C
         0.0008
       } else if (input$Hd.Xsource == "no_X") {
         0.001
       }
     else {
       0.001
     }  
   })

   bn.sys <- 0.0001
   b0.sys <- 0.846
   gamma.sys <- 0.152

   # td0.sys <- 0.001
   
   td0.sys <- reactive({
     req(input$Hd.Xsource)
     if (input$Hd.Xsource == "yes_X") { # scene C
       0.0002
     } else if (input$Hd.Xsource == "no_X") {
       0.0001
     }
     else {
       0.0001
     }  
   })
   
   
   t0.sys <- 0.0001

   params.sys.all <- reactive({
     list(
     'tdn' = tdn.sys(),
     'bn' = bn.sys,
     'b0' = b0.sys,
     'gamma' = gamma.sys,
     'td0' = td0.sys(),
     't0' = t0.sys,
     'tn' = tn_value()
     )
   })
   
   
   # define formula for 'correct' scenario using reactive values
   
   LR.sceneB.sys <- reactive({
     (tn_value()) / (gamma.sys * tdn.sys())
   })
 
   LR.sceneB.full.sys <- reactive({
     ((b0.sys * tn_value()) + (bn.sys * gamma.sys * t0.sys)) / ((b0.sys * gamma.sys * tdn.sys) + (bn.sys * gamma.sys * td0.sys))
   })
   
   LR.sceneA.sys <- reactive({
     (b0.sys * tn_value()) / (bn.sys * gamma.sys)
   })
   
   LR.sceneC.sys <- reactive({
     (b0.sys * tn_value() + bn.sys * gamma.sys * t0.sys) / (b0.sys * tdn.sys() + bn.sys * gamma.sys * td0.sys())
   })

   LR.sceneC1.sys <- reactive({
     (b0.sys * tn_value()) / (b0.sys * tdn.sys() + bn.sys * gamma.sys * td0.sys())
   })
   
   LR.sceneC2.sys <- reactive({
     tn_value() / tdn.sys()
   })

     # values for LR parameters
   output$tn_output <- renderText({
      paste0("tn: ", format(tn_value(), scientific = F))
   })
   
   output$bn_output <- renderText({
      paste0("bn: ", format(bn.sys, scientific = F))
   })
   
   output$b0_output <- renderText({
      paste0("b0: ", format(b0.sys, scientific = F))
   })
   
   output$gamma_output <- renderText({
      paste0("match probability (gamma): ", format(gamma.sys, scientific = F))
   })
   
   output$td0_output <- renderText({
      paste0("td0: ", format(td0.sys(), scientific = F))
   })
   
   output$t0_output <- renderText({
      paste0("t0: ", format(t0.sys, scientific = F))
   })
   
   output$tdn_output <- renderText({
      paste0("tdn: ", format(tdn.sys(), scientific = F))

   })
   
   ## LR Formula ----
   
   chosen_scene <- reactive({
     
     if (input$Hd.alt == "no_alt") {
       "A"
     } else {
       req(input$Hd.Xsource)
       if (input$Hd.Xsource == "no_X") {
         "B"
       } else if (input$Hd.Xsource == "yes_X") {
         if (is.null(input$tn_assumption) || input$tn_assumption == "No") {
           "C"
         } else if (input$tn_assumption == "Yes") {
           req(input$tdn_assumption)
           if (input$tdn_assumption == "No") {
             "C1"
           } else if (input$tdn_assumption == "Yes") {
             "C2"
           }
         }
       }
     }
   })
   
   ## Extra questions for C scenes
   
   # added this in to fix loop in rendering
   Cscenes_active <- reactiveVal(FALSE)
   
   observeEvent(chosen_scene(), {
     updated_scene <- grepl("C", chosen_scene(), fixed = TRUE)
     req(updated_scene != Cscenes_active())
     Cscenes_active(updated_scene)
   })
   
   # added this to fix resetting to "No" when number of fibres is changed
   C1_active <- reactiveVal(value = "No")
   C2_active <- reactiveVal(value = "No")
   
   observeEvent(input$tn_assumption, {
     C1_active(input$tn_assumption)
   })
   
   observeEvent(input$tdn_assumption, {
     C2_active(input$tdn_assumption)
   })
   
   output$Cscenes_Q1 <- renderUI({
     req(Cscenes_active())
     if (Cscenes_active()) {
       box(title = "2a. Scene C",
           status = "danger",
           width = 12,
           paste0("Assume tn (transfer probability of ", input$n.recovered.slider, " fibres under Hp) 
           >> t0 (transfer probability of 0 fibres)?"),
           
           div(style = "height:10px"),
           
           prettyRadioButtons(
             inputId = "tn_assumption",
             label = NULL,
             choices = list(
               "Yes",
               "No"
             ),
             selected = C1_active(),
             icon = icon("check"),
             bigger = TRUE,
             status = "success",
             animation = "jelly"
           )
           )
     } else {}
   })
   
   output$Cscenes_Q2 <- renderUI({
     req(input$tn_assumption)
     if (input$tn_assumption == "Yes" & Cscenes_active()) {
       box(title = NULL,
           headerBorder = FALSE,
           width = 12,
           
           paste0("Assume tdn (transfer probability of ", input$n.recovered.slider, " fibres under Hd)
            >> td0 (transfer probability of 0 fibres)?"),
           
           div(style = "height:10px"),
           
           prettyRadioButtons(
             inputId = "tdn_assumption",
             label = NULL,
             choices = list(
               "Yes",
               "No"
             ),
             selected = C2_active(),
             icon = icon("check"),
             bigger = TRUE,
             status = "success",
             animation = "jelly"
           )
       )
     } else {
     }
   })
   
   output$Cscenes_selected <- renderUI({
     req(Cscenes_active())
     if (Cscenes_active()) {
       div(
         p(paste0("You have selected scene ", chosen_scene())),
         style = "padding-left:20px; color:red;"
       )
     } else {}
   })
   
   
   
   lr.formula <- reactive({
     print(chosen_scene())
     withMathJax(scene_formulas[[chosen_scene()]])
   })
   
   ## MAY NEED DOUBLE CHECKING
   params.sys.scene <- reactive({
     if (chosen_scene() == "A") {
       within(params.sys.all(), rm(tdn, td0, t0))
     } else if (chosen_scene() == "B") {
       within(params.sys.all(), rm(td0, t0, b0, bn))
     } else if (chosen_scene() == "C") {
       params.sys.all()
     } else if (chosen_scene() == "C1") {
       within(params.sys.all(), rm(t0))
     } else if (chosen_scene() == "C2") {
       within(params.sys.all(), rm(bn, b0, gamma, td0, t0))
     }
   })
   
   lr.calculation <- reactive({
     paste0("Calculated LR Scene ", chosen_scene(), ": ", 
           get(paste0("LR.scene", chosen_scene(), ".sys", sep = ""))(), 
           sep = "")
   })
   
   # base formula
   output$LR_full <- renderValueBox({
      valueBox(
         # show formula based on the reactive value
         lr.formula(),
         "LR formula",
         icon = icon("square-root-variable"),
         color = "aqua"
      )
   })
   
   output$system_value <- renderValueBox({
     valueBox(
       value = round(get(paste0("LR.scene", chosen_scene(), ".sys", sep = ""))(), 3),
       subtitle = "System-calculated LR",
       icon = icon("square-root-variable"),
       color = "aqua"
     )
   })
   
   
   # Show lr.formula's reactive value as the output for LR_scene
   output$LR_scene <- renderUI({
      lr.formula()
   })
   
   # Calculated LR for scene (no changes)
   output$LR_scene_formula_dyn <- renderText({
      lr.calculation()   
   })
   
   output$LR_sceneA_result_sys <- renderText({
      paste0("Calculated LR Scene A: ", LR.sceneA.sys())
   })
   
   output$LR_sceneB_result_sys <- renderText({
      paste0("Calculated LR Scene B: ", LR.sceneB.sys())
   })
   
   output$LR_sceneB_full_result_sys <- renderText({
      paste0("Calculated LR Scene B full eqn: ",
            LR.sceneB.full.sys())
   })
   
   output$LR_sceneC1_result_sys <- renderText({
      paste0("Calculated LR Scene C1: ", LR.sceneC1.sys())
   })
   
   output$LR_sceneC2_result_sys <- renderText({
      paste0("Calculated LR Scene C2: ", LR.sceneC2.sys())
   })
   
   # display only parameters required from scene formula
   
   output$scene_parameters <- renderText({
     paste0("Terms corresponding to scene ", chosen_scene(), " LR:<br>",
           paste(names(params.sys.scene()), format(params.sys.scene(), scientific = F), sep=": ", collapse="<br>")
     )
   })
   
   # subtab2: accept / override parameter values --------------------------------------
   
   output$override_parameters <- renderUI({
     
     box(title = paste0("Terms corresponding to scene ", chosen_scene()),
         width = 8,

         tags$span(
           HTML("<b>System terms:</b>"),
             popify(icon("info-circle", verify_fa = FALSE),
                    title = NULL,
                    "Click on ? in header for glossary of terms")
           ),
         
         
         div(style = "height:10px"),
         
         map(names(params.sys.scene()), ~ fluidRow(
           column(3, 
                  style = "display: flex; align-items: baseline;",
                  p(HTML(paste0("<b>", .x, ": </b>", format(params.sys.scene()[[.x]], scientific = F))))
           ),
           column(5, 
                  radioGroupButtons(
                    inputId = paste0("override_", .x),
                    label = NULL,
                    choices = c("accept value" = "accept", "enter own" = "own"),
                    status = "primary",
                    size = "xs",
                    selected = NULL,
                    checkIcon = list(
                      yes = icon("ok", lib = "glyphicon"),
                      no = icon("remove", lib = "glyphicon")
                    )
                  )
           ),
           column(2,
                  disabled(
                    numericInput(
                      inputId = paste0("enter.", .x),
                      label = NULL,
                      width = "100px",
                      value = NULL
                    )
                  )
           ),
           column(2, 
                  disabled(
                    actionBttn(
                      inputId = paste0("save", .x, "Button"),
                      label = paste0("Update ", .x),
                      style = "jelly",
                      color = "royal",
                      size = "xs"
                    )
                  )
           )
         )
         )
     )
   })
   

   # user enter own parameter values ---------------------------------------------------
   tn.user <- reactiveVal()
   # tdn.user <- reactiveVal(value = tdn.sys)
   tdn.user <- reactiveVal()
   
   td0.user <- reactiveVal()
   t0.user <- reactiveVal(value = t0.sys)

   b0.user <- reactiveVal(value = b0.sys)
   bn.user <- reactiveVal(value = bn.sys)

   gamma.user <- reactiveVal(value = gamma.sys)
   
   params.user.all <- reactive({
     list(
       'tdn' = tdn.user(),
       'bn' = bn.user(),
       'b0' = b0.user(),
       'gamma' = gamma.user(),
       'td0' = td0.user(),
       't0' = t0.user(),
       'tn' = tn.user()
     )
   })

   ##tn ---------------------------------------------------
   # if 'accept value'/'enter own' changes, enable/disable input and save
   observeEvent(input$override_tn, {
     if (input$override_tn == "accept") {
       disable("enter.tn")
       disable("savetnButton")
     } else {
       enable("enter.tn")
       enable("savetnButton")
     }
   })
   
   # When the 'Save tn' button is clicked, save the input value for tn in tn.user
   observeEvent(input$savetnButton, {
      tn.user(input$enter.tn)
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "Tn value saved",
         type = "success"
      )
   })
   
   ##tdn ----
   observeEvent(input$override_tdn, {
     if (input$override_tdn == "accept") {
       disable("enter.tdn")
       disable("savetdnButton")
     } else {
       enable("enter.tdn")
       enable("savetdnButton")
     }
   })
   
   observeEvent(input$savetdnButton, {
      tdn.user(input$enter.tdn)
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "Tdn value saved",
         type = "success"
      )
   })

   ##td0 ----
   observeEvent(input$override_td0, {
     if (input$override_td0 == "accept") {
       disable("enter.td0")
       disable("savetd0Button")
     } else {
       enable("enter.td0")
       enable("savetd0Button")
     }
   })
   observeEvent(input$savetd0Button, {
      td0.user(input$enter.td0)
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "Td0 value saved",
         type = "success"
      )
   })

   ##t0 ----
   observeEvent(input$override_t0, {
     if (input$override_t0 == "accept") {
       disable("enter.t0")
       disable("savet0Button")
     } else {
       enable("enter.t0")
       enable("savet0Button")
     }
   })
   observeEvent(input$savet0Button, {
      t0.user(input$enter.t0)
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "T0 value saved",
         type = "success"
      )
   })

   ##b0 ----
   observeEvent(input$override_b0, {
     if (input$override_b0 == "accept") {
       disable("enter.b0")
       disable("saveb0Button")
     } else {
       enable("enter.b0")
       enable("saveb0Button")
     }
   })
   observeEvent(input$saveb0Button, {
      b0.user(input$enter.b0)
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "b0 value saved",
         type = "success"
      )
   })

   ##bn ----
   observeEvent(input$override_bn, {
     if (input$override_bn == "accept") {
       disable("enter.bn")
       disable("savebnButton")
     } else {
       enable("enter.bn")
       enable("savebnButton")
     }
   })
   observeEvent(input$savebnButton, {
      bn.user(input$enter.bn)
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "bn value saved",
         type = "success"
      )
   })

   ##gamma ----
   observeEvent(input$override_gamma, {
     if (input$override_gamma == "accept") {
       disable("enter.gamma")
       disable("savegammaButton")
     } else {
       enable("enter.gamma")
       enable("savegammaButton")
     }
   })
   observeEvent(input$savegammaButton, {
      gamma.user(input$enter.gamma)
      sendSweetAlert(
         session = session,
         title = "Success!",
         text = "gamma value saved",
         type = "success"
      )
   })



   # user vs system values ---------------------------------------------------
   
   params.calculation <- reactive({
     req(params.sys.scene())
     for (param in names(params.sys.scene())) {
        req(input[[paste0("override_", param)]])
     }
     
     imap(params.sys.scene(), ~{
       if (input[[paste0("override_", .y)]] == "accept") {
         list("value" = .x, "source" = "System")
       } else {
         list("value" = params.user.all()[[.y]], "source" = "User")
       }
     }
     )
   })
   
   output$calc_parameters <- renderText({
     paste0("Terms corresponding to calculation:<br>",
            paste0(names(params.calculation()), ": ", 
                  lapply(params.calculation(), function(x) format(x$value, scientific = F)), "  (",
                  lapply(params.calculation(), function(x) x$source), ")",
                  collapse="<br>")
     )
   })
   
   value.calculation <- reactive({
     with(params.calculation(), 
       if (chosen_scene() == "A") {
         (b0$value * tn$value) / (bn$value * gamma$value)
       } else if (chosen_scene() == "B") {
         (tn$value) / (gamma$value * tdn$value)
       } else if (chosen_scene() == "C") {
         (b0$value * tn$value + bn$value * gamma$value * t0$value) / (b0$value * tdn$value + bn$value * gamma$value * td0$value)
       } else if (chosen_scene() == "C1") {
         (b0$value * tn$value) / (b0$value * tdn$value + bn$value * gamma$value * td0$value)
       } else if (chosen_scene() == "C2") {
         tn$value / tdn$value
     }
     )
   })
   
   output$user_calculation <- renderValueBox({
     valueBox(
       value = round(value.calculation(), 3),
       subtitle = "User-calculated LR",
       icon = icon("square-root-variable"),
       color = "green"
     )
   })
   
   
   
   # transfer probabillity plotly output
   output$tnplot <- renderPlotly(
     tn.HC.plotlyci <- plot_ly(data = HC.boot.summstats, x = ~no.fibres) %>% 
       add_trace(y = ~mean.prob, name = 'mean prob', mode = 'lines+markers', type = "scatter",
                 hovertemplate = paste('<i>No. fibres:</i>: %{x}',
                                       '<br>Mean prob: %{y:.2e}')) %>% 
       add_trace(y = ~ci.lower, name = 'lower limit', mode = 'lines', type= "scatter",
                 hovertemplate = paste('<i>No. fibres:</i>: %{x}',
                                       '<br>CI lower limit: %{y:.2e}')) %>% 

       add_trace(y = ~ci.upper, name = 'upper limit', mode = 'lines', type = "scatter",
                 hovertemplate = paste('<i>No. fibres:</i>: %{x}',
                                       '<br>CI lower limit: %{y:.2e}')) %>%
       plotly::layout(title = 'Transfer Probability and 95% CI of Hoody Cotton',
              hovermode = "x")
     )

   
   

# SUMMARY DOCUMENT --------------------------------------------------------

   summarydoc <- reactiveVal()

   
   ## HTML --------
   output$report <- downloadHandler(
     filename = "SummaryDocument.html",
     content = function(file) {
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path(tempdir(), "SummaryTemplate.Rmd")
       file.copy("SummaryTemplate.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(
         activity = input$incident.type,
         control_substrate = input$control_type,
         control_shape = input$control_shape,
         control_shed = input$control_shed,
         fibre_type = input$trace.type,
         fibre_colour = input$trace.colour,
         transfer_direction = input$trace.dir,
         n_fibres = input$n.recovered.slider,
         recipient_substrate = input$select.r.type,
         recipient_shape = input$r.shape,
         recipient_retain = input$r.retain,
         Hp = input$enter.Hp,
         Hd = input$enter.Hd,
         alternative_transfer = input$Hd.alt,
         source_is_x = input$Hd.Xsource,
         chosen_scene = chosen_scene(),
         LR_formula = scene_formulas[[chosen_scene()]],
         tn_assumption = input$tn_assumption,
         tdn_assumption = input$tdn_assumption,
         system_parameters_all = params.sys.all(),
         system_parameters = params.sys.scene(),
         system_calculation = get(paste0("LR.scene", chosen_scene(), ".sys", sep = ""))(),
         user_parameters = lapply(params.calculation(), function(x){x$value}),
         user_calculation = value.calculation(),
         dataset = dataset,
         case_notes = input$casenotes
       )
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
   
   
   
   ## PDF --------
   output$report_pdf <- downloadHandler(
     filename = "SummaryDocument.pdf",
     content = function(file) {
       tempReport_pdf <- file.path(tempdir(), "SummaryTemplate_pdf.Rmd")
       file.copy("SummaryTemplate_pdf.Rmd", tempReport_pdf, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(
         activity = input$incident.type,
         control_substrate = input$control_type,
         control_shape = input$control_shape,
         control_shed = input$control_shed,
         fibre_type = input$trace.type,
         fibre_colour = input$trace.colour,
         transfer_direction = input$trace.dir,
         n_fibres = input$n.recovered.slider,
         recipient_substrate = input$select.r.type,
         recipient_shape = input$r.shape,
         recipient_retain = input$r.retain,
         Hp = input$enter.Hp,
         Hd = input$enter.Hd,
         alternative_transfer = input$Hd.alt,
         source_is_x = input$Hd.Xsource,
         chosen_scene = chosen_scene(),
         LR_formula = scene_formulas[[chosen_scene()]],
         tn_assumption = input$tn_assumption,
         tdn_assumption = input$tdn_assumption,
         system_parameters_all = params.sys.all(),
         system_parameters = params.sys.scene(),
         system_calculation = get(paste0("LR.scene", chosen_scene(), ".sys", sep = ""))(),
         user_parameters = lapply(params.calculation(), function(x){x$value}),
         user_calculation = value.calculation(),
         dataset = dataset,
         case_notes = input$casenotes,
         exam_notes = input$examnotes
       )

       rmarkdown::render(tempReport_pdf, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
   
}
