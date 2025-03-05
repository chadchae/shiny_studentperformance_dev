# Package
library(shiny)
library(shinymanager)
library(DT)

# Load Script
source("script/script.R")

# Server
server <- function(input, output, session) {
    # Authenticate user
    res_auth <- secure_server(check_credentials = check_credentials(credentials))
    user <- reactive(res_auth$user)

    # Reactive to check if the user is an admin
    is_admin <- reactive({
        user <- reactiveValuesToList(res_auth)$user
        credentials %>%
            filter(user == !!user) %>%
            pull(admin) %>%
            as.logical()
    })
    
    # Ensure `data` is a data frame
    processed_data <- reactive({
        # Assuming `data` is defined globally in `script.R`
        as.data.frame(data)
    })

    # Reactive expression for user-specific data
    user_data <- reactive({
        user <- reactiveValuesToList(res_auth)$user
        processed_data() %>%
            filter(E.mail.address == !!user) %>%
            as.data.frame()
    })
    
    # Reactive for individual detail based on search
    individual_data <- reactive({
        req(input$student_id)  # Ensure the input is provided
        processed_data() %>%
            filter(ID == input$student_id) %>%
            as.data.frame()
    })
    
    # Values
    indvalue <- reactive(filter(data, E.mail.address == user())$ScorewithoutP)
    seniroityvalue <- reactive(filter(data, E.mail.address == user())$Class)
    sectionvalue <- reactive(filter(data, E.mail.address == user())$Section)

    
    
    # Render UI for students or admin dynamically
    output$dynamic_ui <- renderUI({
        if (is_admin()) {
            # Admin UI
            fluidPage(
                titlePanel("Admin Dashboard"),
                sidebarLayout(
                    sidebarPanel(
                        tags$h3("Admin Tools"),
                        tags$p("Here you can manage student data and view overall analytics.")
                    ),
                    mainPanel(
                        tabsetPanel(
                            tabPanel(
                                "Students Table",
                                DTOutput("admin_student_table")
                            ),
                            tabPanel(
                                "Individual Detail",
                                fluidPage(
                                    textInput("student_id", "Enter Student ID:", ""),
                                    actionButton("search_btn", "Search"),
                                    h3("Student Details"),
                                    tableOutput("individual_detail"),
                                    h3("Attendance"),
                                    tableOutput("attendance_table"),
                                    h3("Quizzes"),
                                    tableOutput("quizzes_table"),
                                    h3("Assignments"),
                                    tableOutput("assignments_table"),
                                    h3("Final Assignment"),
                                    tableOutput("finalassignment_table"),
                                    h3("Exams"),
                                    tableOutput("exams_table"),
                                    h3("Participation"),
                                    tableOutput("participation_table"),
                                    h3("Comparision"),
                                    plotOutput("admin_comparison_all"),
                                    plotOutput("admin_comparison_gender"),
                                    plotOutput("admin_comparison_section"),
                                    plotOutput("admin_comparison_seniority")
                                )
                            ),
                            tabPanel("Summary Tables",
                                     h3("Summary"),
                                     tableOutput("adminsummary0"),
                                     tableOutput("adminsummary1"),
                                     tableOutput("adminsummary2"),
                                     tableOutput("adminsummary3"),
                                     tableOutput("adminsummary4"),
                                     plotOutput("admin_summary_boxplot"),
                                     plotOutput("admin_summary_boxplot_gender"),
                                     plotOutput("admin_summary_boxplot_section"),
                                     plotOutput("admin_summary_boxplot_class"),
                                     h3("What IF"),
                                     tableOutput("whatiftable"),
                                     plotOutput("whatif1"),
                                     plotOutput("whatif2"),
                                     plotOutput("whatif3")
                                     ),
                            tabPanel("Plots", 
                                     plotOutput("admin_plot"),
                                     plotOutput("comparison_all"),
                                     plotOutput("comparison_gender"),
                                     plotOutput("comparison_section"),
                                     plotOutput("comparison_seniority")
                                     )
                        )
                    )
                )
            )
        } else {
            # Student UI
            fluidPage(
                titlePanel("Score and Grade"),
                sidebarLayout(
                    sidebarPanel(
                        titlePanel("Student Performance Overview"),
                        DTOutput("student_table"),
                        tags$h3("Grade Table"),
                        tags$p("Scores are calculated as follows:"),
                        tags$p("Attendence 10%"),
                        tags$p("Assignment 20%"),
                        tags$p("Quiz 10%"),
                        tags$p("Exams (Midterm + Final) 50%"),
                        tags$p("Final Assignment 10%"),
                        tags$p("Participation extra 10%"),
                        tags$p("Final grades may include participation adjustments."),
                        tags$p(em("Note: This is not the final grade."))
                    ),
                    mainPanel(
                        tags$p(""),
                        DTOutput("myinfo"),
                        tags$p(""),
                        tabsetPanel(
                            tabPanel("Overview", 
                                     tableOutput("attendence"),
                                     tags$p(""),
                                     tableOutput("quizs"),
                                     tags$p(""),
                                     tableOutput("assignments"),
                                     tags$p(""),
                                     tableOutput("finalassignment"),
                                     tags$p(""),
                                     tableOutput("exams"),
                                     tags$p(""),
                                     tableOutput("participation")
                                     ),
                            tabPanel("Score and Grade",
                                     tags$p(strong("Score and Grade")),
                                     tags$p("Your Current Score (without participation)"),
                                     tags$p(""),
                                     tableOutput("currentscorewithoutp"),
                                     tags$p("Score you need for A (without participation)"),
                                     tableOutput("scorewhatyouneedwithoutp")
                            ),
                            tabPanel(
                                "Comparisons",
                                tags$p(""),
                                plotOutput("comparison_all"),
                                tags$p(""),
                                plotOutput("comparison_gender"),
                                tags$p(""),
                                plotOutput("comparison_section"),
                                tags$p(""),
                                plotOutput("comparison_seniority")
                            ),
                            tabPanel("What-If", 
                                     tags$p("What if you got all 30 score of the last of the attendence, quize, assignment, exam and final assignment."),
                                     tableOutput("whatif30"),
                                     tags$p("What if you got all 50 score of the last of the attendence, quize, assignment, exam and final assignment."),
                                     tableOutput("whatif50"),
                                     tags$p("What if you got all 80 score of the last of the attendence, quize, assignment, exam and final assignment."),
                                     tableOutput("whatif80"),

                        )
                    )
                )
            )
        )
        }
    })

#===============Data for Admin===================#    
    # Render admin-specific plot
    output$admin_plot <- renderPlot({
        if (is_admin()) {
            ggplot(processed_data(), aes(x = ScorewithoutP)) +
                geom_histogram(binwidth = 5, fill = "blue", color = "white") +
                labs(title = "Score Distribution", x = "Scores", y = "Frequency") +
                theme_minimal()
        }
    })
    
    # Render admin student table
    output$admin_student_table <- renderDT({
        processed_data() %>%
            mutate(
                Picture = paste0('<img src="face/', ID, '.png" height="50"/>')
            ) %>%
            select(Picture, E.mail.address, ID, Student, 
                   Section, Class, Gender, ScorewithoutP, Grade1, ScorewithPandC, Grade2) %>%
            datatable(#filter="top", 
                selection="multiple",
                escape=FALSE,
                options = list(bFilter=0)) 
    })

    # Render individual details table
    output$individual_detail <- renderTable({
        req(input$search_btn)  # Trigger when search button is clicked
        isolate({
            df <- individual_data()
            if (nrow(df) == 0) {
                return(data.frame(Message = "No data available for the given Student ID"))
            }
            df %>%
                select(ID, Student, Section, Class, Gender, 
                       ScorewithoutP, Grade1, 
                       ScorewithPandC, Grade2)
        })
    })
    
    # Render attendance table
    output$attendance_table <- renderTable({
        req(input$search_btn)
        isolate({
            df <- individual_data()
            if (nrow(df) == 0) {
                return(data.frame(Message = "No data available for the given Student ID"))
            }
            df %>%
                select(Attendence1, Attendence2, Attendence3, Attendence4, 
                       Attendence5, Attendence6, Attendence7, Attendence8, 
                       Attendence9, Attendence10)
        })
    })
    
    # Render quizzes table
    output$quizzes_table <- renderTable({
        req(input$search_btn)
        isolate({
            df <- individual_data()
            if (nrow(df) == 0) {
                return(data.frame(Message = "No data available for the given Student ID"))
            }
            df %>%
                select(quiz1, quiz2, quiz3, quiz4, quiz5)
        })
    })
    
    # Render assignments table
    output$assignments_table <- renderTable({
        req(input$search_btn)
        isolate({
            df <- individual_data()
            if (nrow(df) == 0) {
                return(data.frame(Message = "No data available for the given Student ID"))
            }
            df %>%
                select(assignment1, assignment2, assignment3, assignment4, assignment5)
        })
    })
    
    # Render final assignment table
    output$finalassignment_table <- renderTable({
        req(input$search_btn)
        isolate({
            df <- individual_data()
            if (nrow(df) == 0) {
                return(data.frame(Message = "No data available for the given Student ID"))
            }
            df %>%
                select(finalassignment)
        })
    })
    
    # Render exams table
    output$exams_table <- renderTable({
        req(input$search_btn)
        isolate({
            df <- individual_data()
            if (nrow(df) == 0) {
                return(data.frame(Message = "No data available for the given Student ID"))
            }
            df %>%
                select(midterm, final)
        })
    })
    
    # Render participation table
    output$participation_table <- renderTable({
        req(input$search_btn)
        isolate({
            df <- individual_data()
            if (nrow(df) == 0) {
                return(data.frame(Message = "No data available for the given Student ID"))
            }
            df %>%
                select(Participation)
        })
    })
    
    # adminsummary
    ## Total Students
    output$adminsummary0 <- renderTable({
        data %>%
            summarise(
                total_students = n(),
            )
    })
    
    output$adminsummary1 <- renderTable({
        data %>%
            group_by(Section) %>%
            summarise(
                total_students = n(),
            )
    })
    
    output$adminsummary2 <- renderTable({
        data %>%
            group_by(Gender) %>%
            summarise(
                total_students = n(),
            )
    })
    
    output$adminsummary3 <- renderTable({
        data %>%
            group_by(Class) %>%
            summarise(
                total_students = n(),
            )
    })
    
    output$adminsummary4 <- renderTable({
        data %>%
            group_by(Section) %>%
            summarise(
                mean_score = mean(ScorewithoutP, na.rm = TRUE),
                median_score = median(ScorewithoutP, na.rm = TRUE),
                min_score = min(ScorewithoutP, na.rm = TRUE),
                max_score = max(ScorewithoutP, na.rm = TRUE)
            )
    })

    output$admin_summary_boxplot <- renderPlot({
        ggplot(data, aes(y = ScorewithoutP)) +  # Using x for categorical group
            geom_boxplot(alpha = .2, fill = "#FF6666") +  # Boxplot specific to groups
            theme_minimal() +  # Minimal visual theme
            labs(
                title = "Score Distribution",
                y = "Score witout Participant Extra Score"
            ) +  # Meaningful axis labels
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
            )
    })

    output$admin_summary_boxplot_gender <- renderPlot({
        ggplot(data, aes(x = Gender, y = ScorewithoutP, fill = Gender)) +  # Using x for categorical group
            geom_boxplot(alpha = .2) +  # Boxplot specific to groups
            theme_minimal() +  # Minimal visual theme
            labs(
                title = "Score Distribution by Gender",
                x = "Gender",
                y = "Score witout Participant Extra Score"
            ) +  # Meaningful axis labels
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
            )
    })
    
    output$admin_summary_boxplot_section <- renderPlot({
        ggplot(data, aes(x = Section, y = ScorewithoutP, fill = Section)) +  # Using x for categorical group
            geom_boxplot(alpha = .2) +  # Boxplot specific to groups
            theme_minimal() +  # Minimal visual theme
            labs(
                title = "Score Distribution by Section",
                x = "Section",
                y = "Score witout Participant Extra Score"
            ) +  # Meaningful axis labels
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
            )
    })
    
    output$admin_summary_boxplot_class <- renderPlot({
        ggplot(data, aes(x = Class, y = ScorewithoutP, fill = Class)) +  # Using x for categorical group
            geom_boxplot(alpha = .2) +  # Boxplot specific to groups
            theme_minimal() +  # Minimal visual theme
            labs(
                title = "Score Distribution by Class",
                x = "Gender",
                y = "Score witout Participant Extra Score"
            ) +  # Meaningful axis labels
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold"),
                plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
            )
    })
    
        
    # whatiftable
    output$whatiftable <- renderTable({
        combined_table
    })
    
    # Plots for Admin
    # Graph
    output$whatif1 <- renderPlot({
        g1  
    })
    output$whatif2 <- renderPlot({
        g2
    })
    output$whatif3 <- renderPlot({
        g3
    })
    
    output$admin_comparison_all <- renderPlot({
        req(input$search_btn)  # Ensure the search button was clicked
        isolate({
            df <- individual_data()  # Fetch the data
            # Check if data has rows
            if (nrow(df) == 0) {
                validate(need(FALSE, "No data available for the given Student ID"))
            }
            # Extract the score for the selected ID
            selected_id_score <- df %>% filter(ID == input$student_id) %>% pull(ScorewithoutP)
            
            # Generate the plot
            ggplot(data, aes(x = ScorewithoutP)) +
                geom_density(alpha = .2, fill = "#FF6666") +  # Density plot
                geom_vline(
                    xintercept = mean(df$ScorewithoutP, na.rm = TRUE),  # Blue dashed line for mean
                    color = "blue",
                    size = 2,
                    linetype = "dashed"
                ) +
                xlab("Score witout Participant Extra Score") +  # Add x-axis label
                ylab("Density") +  # Add y-axis label
                theme_minimal() +  # Apply a minimal theme
                theme(
                    axis.text = element_text(size = 16),
                    axis.title = element_text(size = 16, face = "bold")
                )
        })
    })
    output$admin_comparison_gender <- renderPlot({
        req(input$search_btn)  # Ensure the search button was clicked
        isolate({
            df <- individual_data()  # Fetch the data
            # Check if data has rows
            if (nrow(df) == 0) {
                validate(need(FALSE, "No data available for the given Student ID"))
            }
            # Extract the score for the selected ID
            selected_id_score <- df %>% filter(ID == input$student_id) %>% pull(ScorewithoutP)
            
            # Generate the plot
            ggplot(data, aes(x = ScorewithoutP)) +
                geom_density(alpha = .2, fill = "#FF6666") +  # Density plot
                geom_vline(
                    xintercept = mean(df$ScorewithoutP, na.rm = TRUE),  # Blue dashed line for mean
                    color = "blue",
                    size = 2,
                    linetype = "dashed"
                ) +
                xlab("Score witout Participant Extra Score") +  # Add x-axis label
                ylab("Density") +  # Add y-axis label
                theme_minimal() +  # Apply a minimal theme
                facet_wrap(~Gender) +
                theme(
                    axis.text = element_text(size = 16),
                    axis.title = element_text(size = 16, face = "bold")
                )
        })
    })
    output$admin_comparison_section <- renderPlot({
        req(input$search_btn)  # Ensure the search button was clicked
        isolate({
            df <- individual_data()  # Fetch the data
            # Check if data has rows
            if (nrow(df) == 0) {
                validate(need(FALSE, "No data available for the given Student ID"))
            }
            # Extract the score for the selected ID
            selected_id_score <- df %>% filter(ID == input$student_id) %>% pull(ScorewithoutP)
            
            # Generate the plot
            ggplot(data, aes(x = ScorewithoutP)) +
                geom_density(alpha = .2, fill = "#FF6666") +  # Density plot
                geom_vline(
                    xintercept = mean(df$ScorewithoutP, na.rm = TRUE),  # Blue dashed line for mean
                    color = "blue",
                    size = 2,
                    linetype = "dashed"
                ) +
                xlab("Score witout Participant Extra Score") +  # Add x-axis label
                ylab("Density") +  # Add y-axis label
                theme_minimal() +  # Apply a minimal theme
                facet_wrap(~Section) +
                theme(
                    axis.text = element_text(size = 16),
                    axis.title = element_text(size = 16, face = "bold")
                )
        })
    })
    output$admin_comparison_seniority <- renderPlot({
        req(input$search_btn)  # Ensure the search button was clicked
        isolate({
            df <- individual_data()  # Fetch the data
            # Check if data has rows
            if (nrow(df) == 0) {
                validate(need(FALSE, "No data available for the given Student ID"))
            }
            # Extract the score for the selected ID
            selected_id_score <- df %>% filter(ID == input$student_id) %>% pull(ScorewithoutP)
            
            # Generate the plot
            ggplot(data, aes(x = ScorewithoutP)) +
                geom_density(alpha = .2, fill = "#FF6666") +  # Density plot
                geom_vline(
                    xintercept = mean(df$ScorewithoutP, na.rm = TRUE),  # Blue dashed line for mean
                    color = "blue",
                    size = 2,
                    linetype = "dashed"
                ) +
                xlab("Score witout Participant Extra Score") +  # Add x-axis label
                ylab("Density") +  # Add y-axis label
                theme_minimal() +  # Apply a minimal theme
                facet_wrap(~Class) +
                theme(
                    axis.text = element_text(size = 16),
                    axis.title = element_text(size = 16, face = "bold")
                )
        })
    })
    
#===============Student DATA===================#
# Student Information
    output$myinfo <- renderDT({
        processed_data() %>%
            filter(E.mail.address == user()) %>%
            mutate(
                Picture = paste0('<img src="face/', ID, '.png" height="50"/>')
            ) %>%
            select(Picture, E.mail.address, Student, 
                   Section, Class, Gender) %>%
            datatable(#filter="top", 
                selection="multiple",
                escape=FALSE,
                options = list(bFilter=0)) 
    })
    
    output$attendence <- renderTable(
        data %>%
            filter(E.mail.address == user()) %>%
            select(
                Attendence1,
                Attendence2,
                Attendence3,
                Attendence4,
                Attendence5,
                Attendence6,
                Attendence7,
                Attendence8,
                Attendence9,
                Attendence10
            )
    )

    output$quizs <- renderTable(
        data %>%
            filter(E.mail.address == user()) %>%
            select(
                quiz1,
                quiz2,
                quiz3,
                quiz4,
                quiz5
            )
    )
    
    output$assignments <- renderTable(data %>%
                                      filter(E.mail.address == user()) %>%
                                      select(assignment1,
                                             assignment2,
                                             assignment3,
                                             assignment4,
                                             assignment5))
    output$finalassignment <- renderTable(data %>%
                                      filter(E.mail.address == user()) %>%
                                      select(finalassignment))
    output$exams <- renderTable(data %>%
                                      filter(E.mail.address == user()) %>%
                                      select(midterm, final))
    output$participation <- renderTable(data %>%
                                      filter(E.mail.address == user()) %>%
                                      select(Participation))
    output$currentscorewithoutp <- renderTable(data %>%
                                      filter(E.mail.address == user()) %>%
                                      select(ScorewithoutP,
                                             Grade1))
    output$scorewhatyouneedwithoutp <- renderTable(93 - data %>%
                                      filter(E.mail.address == user()) %>%
                                      select(ScorewithoutP))
    
    # What-if
    output$whatif30 <- renderTable(data_sim30 %>%
                                      filter(E.mail.address == user()) %>%
                                      select(ScorewithoutP, Grade1))
    output$whatif50 <- renderTable(data_sim50 %>%
                                       filter(E.mail.address == user()) %>%
                                       select(ScorewithoutP, Grade1))
    output$whatif80 <- renderTable(data_sim80 %>%
                                       filter(E.mail.address == user()) %>%
                                       select(ScorewithoutP, Grade1))
    
    # Graph
    output$comparison_all <- renderPlot(
        data %>%
            ggplot(aes(x = ScorewithoutP)) +
            #geom_histogram(
            #    binwidth = 0.1,
            #    colour = "white",
            #    fill = "grey"
            #) +
            geom_density(
                aes(y = ..density.. * (nrow(data) * 0.1)),
                alpha = .2,
                fill = "#FF6666"
            ) +
            #xlim(1, 5) +
            geom_vline(
                xintercept = as.numeric(indvalue()),
                color = "red",
                size = 2,
                linetype = "dashed"
            ) +
            geom_vline(
                xintercept = mean(data$ScorewithoutP),
                color = "blue",
                size = 2,
                linetype = "dashed"
            ) +
            xlab("") +
            ylab("") +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold")
            )
    )
    output$comparison_gender <- renderPlot(
        data %>%
            ggplot(aes(x = ScorewithoutP)) +
            #geom_histogram(
            #    binwidth = 0.1,
            #    colour = "white",
            #    fill = "grey"
            #) +
            geom_density(
                aes(y = ..density.. * (nrow(data) * 0.1)),
                alpha = .2,
                fill = "#FF6666"
            ) +
            #xlim(1, 5) +
            geom_vline(
                xintercept = as.numeric(indvalue()),
                color = "red",
                size = 2,
                linetype = "dashed"
            ) +
            geom_vline(
                xintercept = mean(data$ScorewithoutP),
                color = "blue",
                size = 2,
                linetype = "dashed"
            ) +
            xlab("") +
            ylab("") +
            facet_wrap(~Gender) +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold")
            )
    )
    output$comparison_section <- renderPlot(
        data %>%
            ggplot(aes(x = ScorewithoutP)) +
            #geom_histogram(
            #    binwidth = 0.1,
            #    colour = "white",
            #    fill = "grey"
            #) +
            geom_density(
                aes(y = ..density.. * (nrow(data) * 0.1)),
                alpha = .2,
                fill = "#FF6666"
            ) +
            #xlim(1, 5) +
            geom_vline(
                xintercept = as.numeric(indvalue()),
                color = "red",
                size = 2,
                linetype = "dashed"
            ) +
            geom_vline(
                xintercept = mean(data$ScorewithoutP),
                color = "blue",
                size = 2,
                linetype = "dashed"
            ) +
            xlab("") +
            ylab("") +
            facet_wrap(~Section) +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold")
            )
    )
    output$comparison_seniority <- renderPlot(
        data %>%
            ggplot(aes(x = ScorewithoutP)) +
            #geom_histogram(
            #    binwidth = 0.1,
            #    colour = "white",
            #    fill = "grey"
            #) +
            geom_density(
                aes(y = ..density.. * (nrow(data) * 0.1)),
                alpha = .2,
                fill = "#FF6666"
            ) +
            #xlim(1, 5) +
            geom_vline(
                xintercept = as.numeric(indvalue()),
                color = "red",
                size = 2,
                linetype = "dashed"
            ) +
            geom_vline(
                xintercept = mean(data$ScorewithoutP),
                color = "blue",
                size = 2,
                linetype = "dashed"
            ) +
            xlab("") +
            ylab("") +
            facet_wrap(~Class) +
            theme(
                axis.text = element_text(size = 16),
                axis.title = element_text(size = 16, face = "bold")
            )
    )

}


# UI
ui <- secure_app(
    uiOutput("dynamic_ui"),  # Render the UI dynamically
    enable_admin = TRUE
)

shinyApp(ui, server)
