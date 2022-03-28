# CRT_analysis
# Michael Dugdale
library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)

pretty_uncert <- function(x, dx){
  # A small function for displaying a value and uncertainty to appropriate number of sig. figs.  General rule, keep one uncertain digit
  # unless leading digit is 1 in which case, retain two uncertain digtis.

  pos <- -floor(log10(dx))
  u_digits <- dx * 10^pos

  if (round(u_digits < 2)){
    pos <- pos + 1
  }
  f_string <- paste0("%.", pos, "f")

  x_str <- sprintf(f_string, round(x*10^pos)/10^pos)
  u_str <- sprintf(f_string, round(dx*10^pos)/10^pos)

  return(paste0(x_str, " ± ", u_str))
}

pred_slope <- function(L, d_L, D, d_D, sep, d_sep){
  # Function that finds the predicted slope (and uncertainty thereof) of the deflection vs. voltage ratio line based on
  # measurements/estimates of physical parameters of the CRT.

  # Inputs:
    # L, d_L : value and measurement uncertainty of plate length L.
    # D, d_D : value and measurement uncertainty of plate-to-screen distance D.
    # sep, d_sep: value and measurement uncertainty of plate spacing d.

  # Returns a list containing:
    # String expressing the predicted slope ± propagated uncertainty
    # Minimum and maximum values of the predicted slope (slope - d_slope, slope + d_slope)
    # If a caculation results in non-finite or NA, returns Inf for each element as a flag

  # Compute the value of the slope
  v <- L^2/(4.0*sep)+L*D/(2.0*sep)

  # Propagate uncertainties to find uncertainty of predicted slope.
  u_L_sq <- ((L + D)/(2.0*sep) * d_L)^2
  u_D_sq <- (L/(2.0*sep)*d_D)^2
  u_sep_sq <- ((v/sep)*d_sep)^2

  u <- sqrt(u_L_sq + u_D_sq + u_sep_sq)

  # If finite, return the slope ± uncertainty, as well as min and max slopes.
  if (is.finite(v) && is.finite(u)){
    out <- list(pretty_uncert(v,u), v-u, v+u)
  }

  # Otherwise, return 'Inf' (to be caught later by is.finite functions.)

  else{
    out <- list(Inf,Inf, Inf)
  }
  return(out)
}


make_model <- function(data){
  # Finds an appropriate fitting function (quadratic/linear, with/without intercept) best describing the deflction vs. voltage ratio data
  # uploaded by a student.

  # Start with full quadratic model
  V_sq <- data$Vratio^2
  data <- cbind(data, V_sq)

  qmodel <- lm(y~Vratio+V_sq, data=data)

  # Is quadratic term significant?
  quad_p <- summary(qmodel)$coefficients[3,4]
  quad_p_pos <- -floor(log10(quad_p))+1
  quad_p_nice <- round(quad_p*10^quad_p_pos)/10^quad_p_pos

  if (quad_p < 0.05){
    # Is intercept significant?
    int_p <- summary(qmodel)$coefficients[1,4]
    if(int_p < 0.05){

      # In this case, both quadratic and intercept terms are significant and must be retained.  That is, we reject both the hypothesis that
      # there is no quadratic term and the hypothesis that there is no intercept (i.e., hypotheses encoded by theoretical prediction.)

      quad_val <- summary(qmodel)$coefficients[3,1]
      quad_u <- summary(qmodel)$coefficients[3,2]

      int_val <- summary(qmodel)$coefficients[1,1]
      int_u <- summary(qmodel)$coefficients[1,2]

      int_p_pos <- -floor(log10(int_p)) + 1
      int_p_nice <- round(int_p*10^int_p_pos)/10^int_p_pos

      slope_val <- summary(qmodel)$coefficients[2,1]
      slope_u <- summary(qmodel)$coefficients[2,2]

      r_sq <- round(summary(qmodel)$adj.r.squared,4)

      string1 <- "The model is quadratic with a significant (non-zero) intercept."
      string2 <- paste0("Quadratic term: p = ", quad_p_nice, " < 0.05 (significant).  Value = ", pretty_uncert(quad_val, quad_u), " cm.")
      string3 <- paste0("Linear term: value = ", pretty_uncert(slope_val,slope_u), " cm.")
      string4 <- paste0("Intercept: p = ", int_p_nice, " < 0.05 (significant).  Value = ", pretty_uncert(int_val, int_u), "cm.")
      string5 <- paste0("Adjusted R^2: ", r_sq)

      strings <- paste(string1, string2, string3, string4, string5, sep="\n")
      return(list("quad1", strings))
    }
    else{
      # Quadratic, but no intercept.  Redo regression with intercept = 0.  *** This may change significance of quadratic term
      # Only return if quadratic term is still significant.

      qmodel <- lm(y~0+Vratio+V_sq, data=data)

      quad_p <- summary(qmodel)$coefficients[2,4]
      quad_p_pos <- -floor(log10(quad_p))+1
      quad_p_nice <- round(quad_p*10^quad_p_pos)/10^quad_p_pos

      int_p_pos <- -floor(log10(int_p)) + 1
      int_p_nice <- round(int_p*10^int_p_pos)/10^int_p_pos

      if (quad_p < 0.05){
        quad_val <- summary(qmodel)$coefficients[2,1]
        quad_u <- summary(qmodel)$coefficients[2,2]

        slope_val <- summary(qmodel)$coefficients[1,1]
        slope_u <- summary(qmodel)$coefficients[1,2]

        r_sq <- round(summary(qmodel)$adj.r.squared,4)
        quad_p_pos <- -floor(log10(quad_p))+1
        quad_p_nice <- round(quad_p*10^quad_p_pos)/10^quad_p_pos

        string1 <- "The model is quadratic with no significant intercept (i.e., intercept = 0)."
        string2 <- paste0("Quadratic term: p = ", quad_p_nice, " < 0.05 (significant).  Value = ", pretty_uncert(quad_val, quad_u), " cm.")
        string3 <- paste0("Linear term: value = ", pretty_uncert(slope_val, slope_u), " cm.")
        string4 <- paste0("Intercept: p = ", int_p_nice, " > 0.05 (not significant).")
        string5 <- paste0("Adjusted R^2: ", r_sq)

        strings <- paste(string1, string2, string3, string4, string5, sep="\n")

        return(list('quad0', strings))
      }
    }
  }

  # If we've reached here, it should be a linear fit.
  lmodel <- lm(y~Vratio, data=data)

  # Check if intercept is significant
  p_int <- summary(lmodel)$coefficients[1,4]
  if (p_int < 0.05){

    constant_raw <- summary(lmodel)$coefficients[1,1]
    constant_u <- summary(lmodel)$coefficients[1,2]

    p_val <- summary(lmodel)$coefficients[1,4]
    pos <- -floor(log10(p_val)) + 1
    p_val <- round(p_val*10^pos)/10^pos

    ratio_raw <- summary(lmodel)$coefficients[2,1]
    ratio_u <- summary(lmodel)$coefficients[2,2]

    r_sq <- round(summary(lmodel)$adj.r.squared,4)

    string1 <- "The model is linear with a significant (i.e., non-zero) intercept."
    string2 <- paste0("Quadratic term: p = ", quad_p_nice," > 0.05 (not significant)")
    string3 <- paste0("Slope: ", pretty_uncert(ratio_raw,ratio_u), " cm.")
    string4 <- paste0("Intercept: p = ",p_val," < 0.05 (significant), value=",pretty_uncert(constant_raw, constant_u), " cm.")
    string5 <- paste0("Adjusted R^2: ", r_sq)

    strings <- paste (string1, string2, string3, string4, string5, sep="\n")

    return(list('lin1', strings))
  }

  else{
    p_val <- summary(lmodel)$coefficients[1,4]
    pos <- -floor(log10(p_val)) + 1
    p_val <- round(p_val*10^pos)/10^pos

    # Now redo the regression w/o intercept
    lmodel <- lm(y~0+Vratio, data=data)

    ratio_raw <- summary(lmodel)$coefficients[1,1]
    ratio_u <- summary(lmodel)$coefficients[1,2]

    r_sq <- round(summary(lmodel)$adj.r.squared,4)

    string1 <- "The model is linear with no significant intercept (i.e., intercept = 0)."
    string2 <- paste0("Quadratic term: p = ", quad_p_nice," > 0.05 (not significant)")
    string3 <- paste0("Slope: ", pretty_uncert(ratio_raw,ratio_u), " cm.")
    string4 <- paste0("Intercept: p = ",p_val," ≥ 0.05 (not significant).")
    string5 <- paste0("Adjusted R^2: ", r_sq)
    strings <- paste(string1, string2, string3, string4, string5, sep="\n")
    return(list('lin0', strings))
  }
}

make_plot <- function(data){

  # Makes a minimalist ggplot2 scatterplot, colour of points based on Vacc.

  model <- make_model(data)[1]

  x_label <-bquote(V[def]/V[acc]~(dimensionless))
  y_label <- "Electron Deflection y (cm)"

  my_plot <- ggplot(data, aes(x=Vratio, y=y)) +
    geom_point(aes(color=factor(Va))) +
    theme_classic() +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    xlab(x_label)+
    ylab(y_label) + labs(color=bquote(V[acc]~(V)))

  if (model=='lin0'){
    my_plot <- my_plot + geom_smooth(
      data=data,
      method="lm",
      formula=y~0+x,
      color='blue',
      fill='blue',
      alpha=0.2)
  }
  else if (model=='lin1'){
    my_plot <- my_plot+geom_smooth(
      data=data,
      method="lm",
      formula=y~1+x,
      color='blue',
      fill='blue',
      alpha=0.2)
  }
  else if (model=='quad0'){
    my_plot <- my_plot+geom_smooth(
      data=data,
      method="lm",
      formula=y~0+x+I(x^2),
      color='blue',
      fill='blue',
      alpha=0.2)
  }

  else if (model=='quad1'){
    my_plot <- my_plot+geom_smooth(
      data=data,
      method="lm",
      formula=y~1+x+I(x^2),
      color='blue',
      fill='blue',
      alpha=0.2)
  }
  return(my_plot)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Cathode Ray Tube analysis"),

  sidebarLayout(
    sidebarPanel(
      numericInput(
        'plateLength',
        label="Plate Length L (cm)",
        value=0,
        min = 1E-2,
        max = 4,
        step = NA,
        width = 200
      ),

      numericInput(
        'delta_plateLength',
        label="Plate Length Uncertainty (cm)",
        value=0,
        min = 1E-2,
        max = 2,
        step = NA,
        width = 200
      ),

      numericInput(
        'screenDist',
        label="Distance D to screen (cm)",
        value=0,
        min = 1E-2,
        max = 30,
        step = NA,
        width = 200
      ),

      numericInput(
        'delta_screenDist',
        label="Distance D Uncertainty (cm)",
        value=0,
        min = 1E-2,
        max = 10,
        step = NA,
        width = 200
      ),

      numericInput(
        'plateSep',
        label="Separation d between the deflection plates (cm)",
        value=0,
        min = 1E-3,
        max = 2,
        step = NA,
        width = 200
      ),

      numericInput(
        'delta_plateSep',
        label="Separation d Uncertainty (cm)",
        value=0,
        min = 1E-3,
        max = 1,
        step = NA,
        width = 200
      ),

      textOutput("predSlope")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      fileInput(
        'data_file',
        label="Upload your completed Excel file",
        multiple = FALSE,
        accept = '.xlsx',
        width = NULL,
        buttonLabel = "Browse...",
        placeholder = "No file selected"
      ),
      verbatimTextOutput("LM"),
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  model_type <- 'None'

  output$predSlope <- renderText({
    values <- pred_slope(input$plateLength,
                         input$delta_plateLength,
                         input$screenDist,
                         input$delta_screenDist,
                         input$plateSep,
                         input$delta_plateSep)


    if (!is.finite(as.numeric(values[3]))){
      string <- "Enter data above to compute predicted slope."
    }
    else if (is.finite(as.numeric(values[3]))){
      string <- paste0("Predicted slope: ", values[1]," cm")
    }
    else {
      string <- "Enter data above to compute predicted slope."
    }
    string
  })

  output$LM <- renderText({
    file <- input$data_file
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "xlsx", "Please upload an Excel .xlsx file"))

    data <- read_xlsx(file$datapath,
                      sheet = NULL,
                      range = 'A1:D200',
                      col_names = TRUE,
                      #col_types = numeric,
                      na = "",
                      trim_ws = TRUE,
                      skip = 0,
                      #n_max = Inf,
                      #guess_max = min(1000, n_max),
                      progress = FALSE,
                      .name_repair = "unique")
    names(data)<-c("Va", "Vd", "Vratio", "y")
    results <- make_model(data)
    model_type <- results[[1]]
    results[[2]]
  })

  output$plot <- renderPlot({
    file <- input$data_file
    ext <- tools::file_ext(file$datapath)
    validate(need(ext == "xlsx", "Please upload an Excel .xlsx file"))
    data <- read_xlsx(file$datapath,
                      sheet = NULL,
                      range = 'A1:D25',
                      col_names = TRUE,
                      #col_types = numeric,
                      na = "",
                      trim_ws = TRUE,
                      skip = 0,
                      #n_max = Inf,
                      #guess_max = min(1000, n_max),
                      progress = FALSE,
                      .name_repair = "unique")
    names(data)<-c("Va", "Vd", "Vratio", "y")

    values <- pred_slope(input$plateLength,
                         input$delta_plateLength,
                         input$screenDist,
                         input$delta_screenDist,
                         input$plateSep,
                         input$delta_plateSep)

    slope_min <- as.numeric(values[2])
    slope_max <- as.numeric(values[3])

    y_min <- data$Vratio*slope_min
    y_max <- data$Vratio*slope_max

    data2 <- data.frame(cbind(data,y_min,y_max))

    plot <- make_plot(data2)

    if (is.finite(slope_min) && is.finite(slope_max)){
      plot <- plot +
        geom_ribbon(data=data2, aes(ymin=y_min, ymax=y_max), alpha=0.05, colour="green", fill="green", show.legend=FALSE)
    }

    plot
  })
}

# Run the application
shinyApp(ui = ui, server = server)
