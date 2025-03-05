library(ggplot2)
library(dplyr)
library(ggridges)
library(tidyr)

# Load and clean data
data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdPz6D3keSL9E6toKu-AR1dmdL_yy00kX3GI41okczGCNegTY6FFKO7RIoEhnTDAYG_irvzf9lVQIb/pub?output=csv") %>%
    replace(is.na(.), 0)

# Prepare credentials
credentials <- data %>%
    select(E.mail.address, ID) %>%
    mutate(admin = FALSE) %>%
    rename(user = E.mail.address, password = ID)
admin <- c("admin", "admin", TRUE)
credentials <- rbind(credentials, admin)

# Condition Weight
attendenceweight <- 0.1
assignmentwegiht <- 0.2
quizweight <- 0.1
examweight <- 0.5
finalassignmentwegiht <- 0.1
Participationweight <- 0.1

attendenceweight_sim1 <- 0.1
assignmentwegiht_sim1 <- 0.2
quizweight_sim1 <- 0.1
examweight_sim1 <- 0.5
finalassignmentwegiht_sim1 <- 0.1
Participationweight_sim1 <- 0.1

attendenceweight_sim2 <- 0.1
assignmentwegiht_sim2 <- 0.2
quizweight_sim2 <- 0.1
examweight_sim2 <- 0.5
finalassignmentwegiht_sim2 <- 0.1
Participationweight_sim2 <- 0.1

attendenceweight_sim3 <- 0.1
assignmentwegiht_sim3 <- 0.2
quizweight_sim3 <- 0.1
examweight_sim3 <- 0.5
finalassignmentwegiht_sim3 <- 0.1
Participationweight_sim3 <- 0.1

# Create Simulation Data
# Replace zeros with normally distributed values (mean = specific number, sd = 10)
set.seed(123)  # Set seed for reproducibility

# Function to replace zeros with random normal values
replace_zeros_with_normal <- function(data, mean, sd) {
    data <- as.data.frame(data)  # Ensure the input is a data frame
    data[] <- lapply(data, function(col) {
        # Replace zeros with random normal values
        ifelse(col == 0, rnorm(sum(col == 0), mean = mean, sd = sd), col)
    })
    return(data)
}

# Generate simulation datasets
data_sim1 <- replace_zeros_with_normal(data, mean = 30, sd = 10)  # Replace with mean 30
data_sim2 <- replace_zeros_with_normal(data, mean = 50, sd = 10)  # Replace with mean 50
data_sim3 <- replace_zeros_with_normal(data, mean = 100, sd = 10) # Replace with mean 100

data_sim30 <- data
data_sim50 <- data
data_sim80 <- data
data_sim30[data_sim30 == 0] <- 30
data_sim50[data_sim50 == 0] <- 50
data_sim80[data_sim80 == 0] <- 80

# Simulation Data Final Score and Grade
data_sim1 <- data_sim1 %>%
    mutate(
        ScorewithoutP = rowSums(across(starts_with("Attendence"))) / 10 * attendenceweight_sim1 +
            rowMeans(across(c(assignment1, assignment2, assignment3, assignment4, assignment5))) * assignmentwegiht_sim1 +
            rowMeans(across(c(quiz1, quiz2, quiz3, quiz4, quiz5))) * quizweight_sim1 +
            rowMeans(across(c(midterm, final))) * examweight_sim1 +
            rowMeans(across(c(finalassignment))) * finalassignmentwegiht_sim1,
        Grade1 = cut(ScorewithoutP, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")),
        ScorewithPandC = ceiling(ScorewithoutP + Participation * Participationweight_sim1),
        Grade2 = cut(ScorewithPandC, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A"))
    )

data_sim2 <- data_sim2 %>%
    mutate(
        ScorewithoutP = rowSums(across(starts_with("Attendence"))) / 10 * attendenceweight_sim2 +
            rowMeans(across(c(assignment1, assignment2, assignment3, assignment4, assignment5))) * assignmentwegiht_sim2 +
            rowMeans(across(c(quiz1, quiz2, quiz3, quiz4, quiz5))) * quizweight_sim2 +
            rowMeans(across(c(midterm, final))) * examweight_sim2 +
            rowMeans(across(c(finalassignment))) * finalassignmentwegiht_sim2,
        Grade1 = cut(ScorewithoutP, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")),
        ScorewithPandC = ceiling(ScorewithoutP + Participation * Participationweight_sim2),
        Grade2 = cut(ScorewithPandC, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A"))
    )

data_sim3 <- data_sim3 %>%
    mutate(
        ScorewithoutP = rowSums(across(starts_with("Attendence"))) / 10 * attendenceweight_sim3 +
            rowMeans(across(c(assignment1, assignment2, assignment3, assignment4, assignment5))) * assignmentwegiht_sim3 +
            rowMeans(across(c(quiz1, quiz2, quiz3, quiz4, quiz5))) * quizweight_sim3 +
            rowMeans(across(c(midterm, final))) * examweight_sim3 +
            rowMeans(across(c(finalassignment))) * finalassignmentwegiht_sim3,
        Grade1 = cut(ScorewithoutP, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")),
        ScorewithPandC = ceiling(ScorewithoutP + Participation * Participationweight_sim3),
        Grade2 = cut(ScorewithPandC, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A"))
    )

data_sim30 <- data_sim30 %>%
    mutate(
        ScorewithoutP = rowSums(across(starts_with("Attendence"))) / 10 * attendenceweight +
            rowMeans(across(c(assignment1, assignment2, assignment3, assignment4, assignment5))) * assignmentwegiht +
            rowMeans(across(c(quiz1, quiz2, quiz3, quiz4, quiz5))) * quizweight +
            rowMeans(across(c(midterm, final))) * examweight +
            rowMeans(across(c(finalassignment))) * finalassignmentwegiht,
        Grade1 = cut(ScorewithoutP, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")),
        ScorewithPandC = ceiling(ScorewithoutP + Participation * Participationweight),
        Grade2 = cut(ScorewithPandC, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A"))
    )
data_sim50 <- data_sim50 %>%
    mutate(
        ScorewithoutP = rowSums(across(starts_with("Attendence"))) / 10 * attendenceweight +
            rowMeans(across(c(assignment1, assignment2, assignment3, assignment4, assignment5))) * assignmentwegiht +
            rowMeans(across(c(quiz1, quiz2, quiz3, quiz4, quiz5))) * quizweight +
            rowMeans(across(c(midterm, final))) * examweight +
            rowMeans(across(c(finalassignment))) * finalassignmentwegiht,
        Grade1 = cut(ScorewithoutP, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")),
        ScorewithPandC = ceiling(ScorewithoutP + Participation * Participationweight),
        Grade2 = cut(ScorewithPandC, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A"))
    )
data_sim80 <- data_sim80 %>%
    mutate(
        ScorewithoutP = rowSums(across(starts_with("Attendence"))) / 10 * attendenceweight +
            rowMeans(across(c(assignment1, assignment2, assignment3, assignment4, assignment5))) * assignmentwegiht +
            rowMeans(across(c(quiz1, quiz2, quiz3, quiz4, quiz5))) * quizweight +
            rowMeans(across(c(midterm, final))) * examweight +
            rowMeans(across(c(finalassignment))) * finalassignmentwegiht,
        Grade1 = cut(ScorewithoutP, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")),
        ScorewithPandC = ceiling(ScorewithoutP + Participation * Participationweight),
        Grade2 = cut(ScorewithPandC, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A"))
    )

# Calculate current scores and grades
data <- data %>%
    mutate(
        ScorewithoutP = rowSums(across(starts_with("Attendence"))) / 10 * attendenceweight +
            rowMeans(across(c(assignment1, assignment2, assignment3, assignment4, assignment5))) * assignmentwegiht +
            rowMeans(across(c(quiz1, quiz2, quiz3, quiz4, quiz5))) * quizweight +
            rowMeans(across(c(midterm, final))) * examweight +
            rowMeans(across(c(finalassignment))) * finalassignmentwegiht,
        Grade1 = cut(ScorewithoutP, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")),
        ScorewithPandC = ceiling(ScorewithoutP + Participation * Participationweight),
        Grade2 = cut(ScorewithPandC, breaks = c(-Inf, 60, 70, 77, 80, 84, 87, 90, 93, Inf),
                     labels = c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A"))
    )

# Create Simulation Table
simdata <- data.frame(
    `CG1` = data$Grade1,
    `CG2` = data$Grade2,
    `S1G1` = data_sim1$Grade1,
    `S1G2` = data_sim1$Grade2,
    `S2G1` = data_sim2$Grade1,
    `S2G2` = data_sim2$Grade2,
    `S3G1` = data_sim3$Grade1,
    `S3G2` = data_sim3$Grade2
)

rownames(simdata) <- data$ID

# Define consistent grade levels (ensure they match the labels used in the cut function)
grade_levels <- c("F", "D", "C", "C+", "B-", "B", "B+", "A-", "A")

# Apply `table` for each column and ensure consistent factor levels
cg1_table <- table(factor(simdata$CG1, levels = grade_levels))
cg2_table <- table(factor(simdata$CG2, levels = grade_levels))
s1g1_table <- table(factor(simdata$S1G1, levels = grade_levels))
s1g2_table <- table(factor(simdata$S1G2, levels = grade_levels))
s2g1_table <- table(factor(simdata$S2G1, levels = grade_levels))
s2g2_table <- table(factor(simdata$S2G2, levels = grade_levels))
s3g1_table <- table(factor(simdata$S3G1, levels = grade_levels))
s3g2_table <- table(factor(simdata$S3G2, levels = grade_levels))

# Combine results using rbind
combined_table <- rbind(cg1_table, cg2_table, s1g1_table, s1g2_table, s2g1_table, s2g2_table, s3g1_table, s3g2_table)

# Add row names for clarity
rownames(combined_table) <- c("CG1", "CG2", "S1G1", "S1G2", "S2G1", "S2G2","S3G1", "S3G2")

# Convert the matrix to a data frame
df <- as.data.frame(combined_table)

# Add a column to represent row names (grade sources)
df <- df %>%
    mutate(Grade_Source = rownames(combined_table)) %>%
    pivot_longer(
        cols = -Grade_Source,  # All columns except Grade_Source
        names_to = "Grade",    # New column for grade levels
        values_to = "Frequency" # New column for frequencies
    )

# Expand data to support ridgeline structure
df_expanded <- df %>%
    uncount(Frequency)  # Repeat rows by their frequency

# Ridgeline chart
g1<-ggplot(df_expanded, aes(x = Grade, y = Grade_Source, fill = Grade_Source)) +
    geom_density_ridges(scale = 2, alpha = 0.5, rel_min_height = 0.03) +
    labs(
        title = "Ridgeline Chart of Grade Distributions",
        x = "Grade",
        y = "Grade Source"
    ) +
    theme_ridges() +
    scale_fill_brewer(palette = "Set2")  # Use a nice color palette

g2<-ggplot(df_expanded, aes(x = Grade, y = Grade_Source, fill = Grade)) +
    geom_density_ridges(scale = 2, alpha = 0.5, rel_min_height = 0.03) +
    labs(
        title = "Ridgeline Chart of Grade Distributions",
        x = "Grade",
        y = "Grade Source"
    ) +
    theme_ridges() +
    scale_fill_brewer(palette = "Spectral")

# Use ggplot2 facet_wrap to display multiple distributions
g3<-df %>%
    filter(Grade_Source %in% c("CG1", "CG2", "S1G1", "S1G2", "S2G1", "S2G2", "S3G1", "S3G2")) %>%  
    ggplot(aes(x = Grade, y = Frequency, fill = Grade)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    facet_wrap(~ Grade_Source, ncol = 2) +  # Create a separate panel for each grade source
    labs(title = "Grade Distributions",
         x = "Grade",
         y = "Frequency") +
    scale_fill_brewer(palette = "Spectral") +
    theme_minimal()

overallave <- data %>% summarise(mean = mean(ScorewithoutP))
sectionave <- data %>% group_by(Section) %>% summarise(mean = mean(ScorewithoutP))
seniroityave <- data %>% group_by(Class) %>% summarise(mean = mean(ScorewithoutP))
genderave <- data %>% group_by(Gender) %>% summarise(mean = mean(ScorewithoutP))

