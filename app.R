# Load necessary libraries
library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(agricolae)
library(knitr)
library(kableExtra)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("ANOVA Analysis For BIOL 1208"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File", accept = c(".xlsx")),
      uiOutput("selectUI"),
      actionButton("analyze", "Analyze")
    ),
    mainPanel(
      uiOutput("tableOutputUI"),
      plotOutput("barPlot", width = "50%"),
      plotOutput("scatterPlot", width="50%")
    )
  )
)

# Define server logic for the app
server <- function(input, output, session) {
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file1)
    read_excel(input$file1$datapath)
  })

  # Dynamic UI for selecting the treatment column
  output$selectUI <- renderUI({
    req(data())
    # Filter the choices to only the specified treatments
    valid_treatments <- intersect(names(data()), c("Temperature", "pH", "Catechol Volume"))
    selectInput("treatment", "Select Treatment Column", choices = valid_treatments)
  })

  # Perform the analysis and display the final table and plot
  observeEvent(input$analyze, {
    req(data())
    req(input$treatment)

    # Get the data and selected treatment column
    df <- data()
    treatment <- input$treatment

    # Perform ANOVA analysis
    if (treatment == "Catechol Volume") {
      formula <- as.formula(paste("`Benzoquinone Concentration (mM)` ~ `Catechol Volume`"))
    } else {
      formula <- as.formula(paste("`Benzoquinone Concentration (mM)` ~", treatment))
    }
    anova_result <- aov(formula, data = df)
    anova_summary <- summary(anova_result)

    # Calculate descriptive statistics
    desc_stats <- df %>%
      group_by(!!sym(treatment)) %>%
      summarise(
        Mean = round(mean(`Benzoquinone Concentration (mM)`), 3), # round the results to 3 decimal points
        Standard_Deviation = round(sd(`Benzoquinone Concentration (mM)`), 3)
      ) %>%
      mutate(Treatment = as.character(!!sym(treatment)))  # Convert to character to display in table

    # Extract ANOVA p-value
    anova_p_value <- round(anova_summary[[1]][1, "Pr(>F)"], 3) # From anova summary exract probability column to get the p-value

    # Perform post-hoc test using Tukey's HSD
    tukey_result <- HSD.test(anova_result, treatment)
    # The tukey_results contains following parameters
    # #parameters, $means, $comparison and $groups. The $group has t test letter stored in it.
    # Correctly extract Tukey HSD groups
    tukey_groups <- data.frame(Treatment = rownames(tukey_result$groups), `t-Test Letters` = tukey_result$groups[, "groups"])
    tukey_groups$Treatment <- as.character(tukey_groups$Treatment)  # Ensure Treatment is character

    # Create a data frame for the final table
    final_table <- desc_stats %>%
      left_join(tukey_groups, by = "Treatment") %>%
      mutate(`ANOVA p-Value` = ifelse(row_number() == 1, as.character(anova_p_value), ""))

    # Render the final table
    output$tableOutputUI <- renderUI({
      tableHTML <- final_table %>%
        select(Treatment, Mean, Standard_Deviation, `ANOVA p-Value`, `t.Test.Letters`) %>%
        kbl() %>%
        kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"), position = "center") %>%
        collapse_rows(columns = 4, valign = "top") %>%
        row_spec(0, bold = TRUE, background = "white", align="c")#"#D3D3D3", align="c")

      HTML(tableHTML)
    })

    # Render the bar plot
    output$barPlot <- renderPlot({
      ggplot(desc_stats, aes(x = as.factor(Treatment), y = Mean)) +
        geom_bar(stat = "identity", position = position_dodge(), fill = "skyblue") +
        geom_errorbar(aes(ymin = Mean - Standard_Deviation, ymax = Mean + Standard_Deviation), width = 0.2) +
        labs(title = paste("Mean Benzoquinone Concentration by", treatment),
             x = treatment,
             y = "Mean Benzoquinone Concentration (mM)") +
        theme_classic()
    })
    # Render the scatter plot
    output$scatterPlot <- renderPlot({
      data_group <- df %>%
        group_by(!!sym(treatment), `Time (s)`) %>%
        summarise(
          mean = mean(`Benzoquinone Concentration (mM)`),
          sd = sd(`Benzoquinone Concentration (mM)`)
        )

      ggplot(data_group, aes(x = `Time (s)`, y = mean, color = as.factor(!!sym(treatment)))) +
        scale_color_discrete(name = treatment) +
        geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd)) +
        geom_smooth(se = FALSE) +
        labs(y = "Mean Benzoquinone Concentration (mM)") +
        theme_classic()
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
