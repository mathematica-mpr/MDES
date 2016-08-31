# /******************************************************************************
# * Copyright (C) Mathematica Policy Research, Inc. 
# * This code cannot be copied, distributed or used without the express written permission
# * of Mathematica Policy Research, Inc. 
# *******************************************************************************/

library(shiny)
library(MDES)
library(shinyBS)

shinyServer(function(input, output, session) {

  shinyURL::shinyURL.server()

  output$Design <- renderText({
    paste(input$Design, 'settings')
  })

  output$txt.outcome <- renderText({
    ifelse(
      test = input$outcome == "continuous", yes = txt.outcome <-
        "SD y:", no = txt.outcome <- "Mean y:"
    )
  })

  output$g <- renderUI({
    ifelse(
      test = input$Clusters == TRUE ,
      yes = g <-
        numericInput(
          "g", "Total number of groups:", min = 1, step = 1, value = 20
        ),
      no = g <- ""
    )
    return(g)

  })

  output$rho <- renderUI({
    ifelse(
      test = input$Clusters == TRUE ,
      yes = rho <-
        numericInput(
          "rho", "Intraclass correlation coefficient:", min = 0.0, max = 0.999, step =
            0.001, value = 0.02
        ),
      no = rho <- ""
    )
    return(rho)

  })

  output$dynUI <- renderUI({
    ifelse(
      test = (input$Clusters == FALSE &
                input$Design == "Random Assigment"),
      yes = dynUI <-
        flowLayout(
          sliderInput(
            "R_sq_xy", HTML("R<sup>2</sup><sub>x,y<//sub>"), min = 0.0, 
            max = 0.999, step = 0.01, value = 0.00
          )
        ),
      no = ifelse(
        test = (input$Clusters == TRUE & input$Design == "Random Assigment"),
        yes = dynUI <-
          flowLayout(
            sliderInput(
              "R_sq_WG", "Within group variance of y explained by x:", min = 0.0, max =
                0.999, step = 0.01, value = 0.00
            ),
            sliderInput(
              "R_sq_BG", "Group lvl variance of y explained by x:", min = 0.0, max = 0.999, step =
                0.01, value = 0.00
            )
          ),
        no = ifelse(
          test = (
            input$Clusters == FALSE &
              input$Design == "Matched Comparison Group"
          ),
          yes =  dynUI <-
            flowLayout(
              sliderInput(
                "R_sq_xyt", "R_sq_xyt:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
              ),
              sliderInput(
                "R_sq_xt", "R_sq_xt:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
              )
            ),
          no = ifelse(
            test = (
              input$Clusters == TRUE & input$Design == "Matched Comparison Group"
            ),
            yes = dynUI <-
              flowLayout(
                sliderInput(
                  "R_sq_WG", "R_sq_WG:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                ),
                sliderInput(
                  "R_sq_BG", "R_sq_BG:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                ),
                sliderInput(
                  "R_sq_xt", "R_sq_xt:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                )
              ),
            no = ifelse(
              test = (input$Clusters == FALSE & input$Design == "DID"),
              yes = dynUI <-
                flowLayout(
                  sliderInput(
                    "R_sq_WG", "R_sq_WG:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                  ),
                  sliderInput(
                    "R_sq_xt", "R_sq_xt:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                  ),
                  sliderInput(
                    "theta", "theta:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                  )
                ),
              no = dynUI <-
                flowLayout(
                  sliderInput(
                    "R_sq_WG", "R_sq_WG:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                  ),
                  sliderInput(
                    "R_sq_BG", "R_sq_BG:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                  ),
                  sliderInput(
                    "R_sq_xt", "R_sq_xt:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                  ),
                  sliderInput(
                    "theta", "theta:", min = 0.0, max = 0.999, step = 0.01, value = 0.00
                  )
                )
            )
          )
        )
      )
    )
    return(dynUI)
  })



  MDES <- reactive({
    ifelse(
      test = (input$Design == "Random Assigment" &
                input$Clusters == FALSE) ,
      yes = x <- MDES_RA(
        sig.lvl = as.numeric(input$sig.lvl),
        power = as.numeric(input$power),
        N = input$N,
        p = input$p,
        outcome = input$outcome,
        mean_y = input$num,
        sigma_y = input$num,
        R_sq_xy = input$R_sq_xy
      ) ,
      no = ifelse(
        test = (input$Design == "Random Assigment" &
                  input$Clusters == TRUE),
        yes = x <- MDES_CL(
          sig.lvl = as.numeric(input$sig.lvl),
          power = as.numeric(input$power),
          N = input$N,
          g = input$g,
          p = input$p,
          outcome = input$outcome,
          mean_y = input$num,
          sigma_y = input$num,
          rho = input$rho,
          R_sq_WG = input$R_sq_WG,
          R_sq_BG = input$R_sq_BG
        ) ,
        no = ifelse(
          test = (
            input$Design == "Matched Comparison Group" &
              input$Clusters == FALSE
          ),
          yes = x <-
            MDES_MCG(
              sig.lvl = as.numeric(input$sig.lvl),
              power = as.numeric(input$power),
              N = input$N,
              p = input$p,
              outcome = input$outcome,
              mean_y = input$num,
              sigma_y = input$num,
              R_sq_xyt = input$R_sq_xyt,
              R_sq_xt = input$R_sq_xt
            ),
          no = ifelse(
            test = (
              input$Design == "Matched Comparison Group" &
                input$Clusters == TRUE
            ),
            yes = x <-
              MDES_MCG_CL(
                sig.lvl = as.numeric(input$sig.lvl),
                power = as.numeric(input$power),
                N = input$N,
                g = input$g,
                p = input$p,
                outcome = input$outcome,
                mean_y = input$num,
                sigma_y = input$num,
                rho = input$rho,
                R_sq_WG = input$R_sq_WG,
                R_sq_BG = input$R_sq_BG,
                R_sq_xt = input$R_sq_xt
              ),
            no = ifelse(test = (
              input$Design == "DID" &
                input$Clusters == FALSE
            ),
            yes = x <- MDES_DID(sig.lvl = as.numeric(input$sig.lvl),
                           power = as.numeric(input$power),
                           N = input$N,
                           p = input$p,
                           outcome = input$outcome,
                           mean_y = input$num,
                           sigma_y = input$num,
                           R_sq_WG = input$R_sq_WG,
                           R_sq_xt = input$R_sq_xt,
                           theta = input$theta),
            no = x <- MDES_DID_CL(sig.lvl = as.numeric(input$sig.lvl),
                                  power = as.numeric(input$power),
                                  N = input$N,
                                  g = input$g,
                                  p = input$p,
                                  outcome = input$outcome,
                                  mean_y = input$num,
                                  sigma_y = input$num,
                                  rho = input$rho,
                                  R_sq_WG = input$R_sq_WG,
                                  R_sq_BG = input$R_sq_BG,
                                  R_sq_xt = input$R_sq_xt,
                                  theta = input$theta))
          )
        )
      )
    )
    return(round(x,3))
  })







  output$text1 <- renderText({
    paste0(
      "The minimum detectable effect size (MDES) is ", MDES()$MDES, ". The minimum detectable effect (MDE) is ", MDES()$MDE,"."
    )
  })

})
