# install ggplot2 package
install.packages("ggplot2")

# read in dataset to student dropout data frame
StudentDropout <- read.csv("dataset.csv")

# create Retention column including enrolled with graduate as "0"; create new target column with dropout & graduate/enrolled
StudentDropout$Retention <- ifelse(StudentDropout$Target == "Dropout", 1, 0)
StudentDropout$NewTarget <- ifelse(StudentDropout$Target == "Dropout", "Dropout", "Graduate/Enrolled")

# create a plotColors variable to apply to all graphs
plotColors <- c("pink","lightblue")


## Marital Status ---- 
# uses ggplot to create a stacked histogram using marital status and retention data
library(ggplot2)
maritalStatusFrame <- data.frame(StudentDropout$Marital.status, StudentDropout$Retention)
ggplot(maritalStatusFrame, aes(x = StudentDropout$Marital.status, fill = factor(StudentDropout$Retention))) + geom_bar() + scale_fill_manual(values = plotColors) + labs(x = "Marital Status", y = "Number of Students", fill = "Retention") + ggtitle("Retention in Marital Status") + theme(plot.title = element_text(hjust = 0.5))


## Mother's Qualifications ----
# uses ggplot to create a stacked histogram using mother's qualifications and retention data
library(ggplot2)
motherQualFrame <- data.frame(StudentDropout$Mother.s.qualification, StudentDropout$Retention)
ggplot(motherQualFrame, aes(x = StudentDropout$Mother.s.qualification, fill = factor(StudentDropout$Retention))) + xlim(0, 34) + geom_bar() + scale_fill_manual(values = plotColors) + labs(x = "Mother's Qualifications", y = "Number of Students", fill = "Retention") + ggtitle("Retention in Mother's Qualifications") + theme(plot.title = element_text(hjust = 0.5))


## Father's Qualifications ----
# uses ggplot to create a stacked histogram using father's qualifications and retention data
library(ggplot2)
fatherQualFrame <- data.frame(StudentDropout$Father.s.qualification, StudentDropout$Retention)
ggplot(fatherQualFrame, aes(x = StudentDropout$Father.s.qualification, fill = factor(StudentDropout$Retention))) + xlim(0, 34) + geom_bar() + scale_fill_manual(values = plotColors) + labs(x = "Father's Qualifications", y = "Number of Students", fill = "Retention") + ggtitle("Retention in Father's Qualifications") + theme(plot.title = element_text(hjust = 0.5))


## Mother's Occupation ----
# uses ggplot to create a stacked histogram using mother's occupation and retention data
library(ggplot2)
motherOccFrame <- data.frame(StudentDropout$Mother.s.occupation, StudentDropout$Retention)
ggplot(motherOccFrame, aes(x = StudentDropout$Mother.s.occupation, fill = factor(StudentDropout$Retention))) + geom_bar() + scale_fill_manual(values = plotColors) + labs(x = "Mother's Occupation", y = "Number of Students", fill = "Retention") + ggtitle("Retention in Mother's Occupation") + theme(plot.title = element_text(hjust = 0.5))


## Father's Occupation ----
# uses ggplot to create a stacked histogram using father's occupation and retention data
library(ggplot2)
fatherOccFrame <- data.frame(StudentDropout$Father.s.occupation, StudentDropout$Retention)
ggplot(fatherOccFrame, aes(x = StudentDropout$Father.s.occupation, fill = factor(StudentDropout$Retention))) + geom_bar() + scale_fill_manual(values = plotColors) + labs(x = "Father's Occupation", y = "Number of Students", fill = "Retention") + ggtitle("Retention in Father's Occupation") + theme(plot.title = element_text(hjust = 0.5))


## Educational Special Needs ----
# uses ggplot to create a stacked bar graph using educational special needs data (change binary to categorical yes or no) and retention data
library(ggplot2)
StudentDropout$eduNeedsDiscrete <- ifelse(StudentDropout$Educational.special.needs == 0, "no", "yes")
eduSpecNeedsFrame <- data.frame(StudentDropout$eduNeedsDiscrete, StudentDropout$Retention)
ggplot(eduSpecNeedsFrame, aes(x = StudentDropout$eduNeedsDiscrete, fill = factor(StudentDropout$Retention))) + geom_bar() + scale_fill_manual(values = plotColors) + labs(x = "Educational Special Needs", y = "Number of Students", fill = "Retention") + ggtitle("Retention in Educational Special Needs") + theme(plot.title = element_text(hjust = 0.5))


## Unemployment Rate ----
# uses ggplot to create two boxplots comparing the unemployment rate with the new target column values of graduate/enrolled and dropouts
library(ggplot2)
unumploymentFrame <- data.frame(StudentDropout$Unemployment.rate, StudentDropout$NewTarget)
ggplot(unumploymentFrame, aes(x = StudentDropout$NewTarget, y = StudentDropout$Unemployment.rate), fill = factor(NewTarget)) + geom_boxplot(fill = c("lightblue", "pink")) + labs(x = "Retention", y = "Unemployment Rate") + ggtitle("Retention in Unemployment Rate") + theme(plot.title = element_text(hjust = 0.5))


## Age ----
# create a stem and leaf plot to analyze the frequency of various ages at enrollment; also calculate mean, median and standard deviation
plot.new()
stemPlot <- capture.output(stem(StudentDropout$Age.at.enrollment))
text(0,1, paste(stemPlot, collapse = '\n'), adj=c(0,1), family = 'mono')
ageMean <- mean(StudentDropout$Age.at.enrollment)
ageMedian <- median(StudentDropout$Age.at.enrollment)
ageSD = sd(StudentDropout$Age.at.enrollment)

# uses ggplot to create a stacked histogram comparing using age at enrollment and retention data
ageFrame <- data.frame(StudentDropout$Age.at.enrollment, StudentDropout$Retention)
ggplot(ageFrame, aes(x = StudentDropout$Age.at.enrollment, fill = factor(StudentDropout$Retention))) + geom_bar() + labs(x = "Age at Enrollment", y = "Number of Students", fill = "Retention") + scale_fill_manual(values = plotColors) + ggtitle("Retention in Age at Enrollment") + theme(plot.title = element_text(hjust = 0.5)) #+ stat_summary(fun.y = mean, geom = "point", shape = 18, size = 10, color = "red")

