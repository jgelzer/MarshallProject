library(shiny)
library(ggplot2)
library(shinydashboard)
library(grid)
library(markdown)
library(ggExtra)
library(dplyr)
demographics = read.csv("LAdemographics1.csv")
data = read.csv("data.csv")
projdata = read.csv("projdata.csv")

#server <- function(input, output) {
shinyServer(function(input, output, session) {
  output$table <- renderTable(subset(demographics, project.ZIP==input$zipcode)$PROJECT.NAME)
  
 ### Saving data:
 Rawdata <- reactive({
  input$refresh 
  input$refresh2 
     
  slope <- input$slope
  SD <- input$SD
  sample <- input$sample
  x <- round(1:sample + rnorm(n = sample, mean = 1, sd = 2), digits = 2)
  y <- round(slope * (x) + rnorm(n = sample, mean = 3, sd = SD ), digits = 2)
  mod <- lm(y ~ x, data.frame(y,x))
  ypred <- predict(mod)
  Rawdata <- data.frame(y, x, ypred)
 })

 SSdata <- reactive({
  dat <- Rawdata()
  Y <- mean(dat$y)
  mod <- lm(y ~ x, dat)
  ypred <- predict(mod)
  dat$ypred <- ypred
  SST <- sum((dat$y - Y)^2)
  SSE <- round(sum((dat$y - ypred)^2), digits = 5)
  SSA <- SST - SSE

  SSQ <- data.frame(SS = c("Total","Regression","Error"),
                    value = as.numeric(c(SST, SSA, SSE)/SST)*100)
  SSQ$SS <- factor(SSQ$SS, as.character(SSQ$SS))
  SSdata <- data.frame(SS = factor(SSQ$SS, as.character(SSQ$SS)),
                    value = as.numeric(c(SST, SSA, SSE)/SST)*100)

 })



 ### First output "graphs"
 output$total <- renderPlot({
   matches = subset(demographics, project.ZIP == input$zipcode)
   match = matches[1,]
   
   age = match[,21:24]
   agedf = data.frame(x=c("<25", "25-44", "45-64", "65+"), y=as.numeric(age))
   
   
   
   ggplot(agedf, aes( x=x, y=y))+
     geom_bar(stat = "identity",color="darkblue", fill="lightblue")+
     theme(plot.title = element_text(hjust=0.5, size=15))+
  #   labs(title = "Age by Zip Code", x = "Age (years)") 
   
  #cols <- c("#619CFF", "#00BA38", "#F8766D")
  #ggplot(Rawdata(), aes(x=x,y=y))+
   #geom_point(size=3) +
   #geom_segment(xend = Rawdata()[,2], yend = mean(Rawdata()[,1]),
    #            colour = "#619CFF")+
   #geom_hline(yintercept = mean(Rawdata()[,1]))+
  # theme(axis.title = element_text(size = 12),
   #      axis.text  = element_text(size = 12),
    #     panel.background=element_rect(fill="white",colour="black"))+
  # ggtitle("Age (years)")+
     ylab("Count")+
     xlab("Age (years)")
 })

 ### First output "graphs"
 output$regression <- renderPlot({
   matches = subset(demographics, project.ZIP == input$zipcode)
   match = matches[1,]
   
   race = match[,14:20]
   racedf = data.frame(x=c("Hispanic", "White", "Black", "Asian", "Mixed", "Native American", "Pacific Islander"), y=as.numeric(race))
   
   ggplot(racedf, aes(y = y, x = x))+
     geom_bar(stat = "identity",color="darkblue", fill="lightblue")+
     theme(plot.title = element_text(hjust=0.5, size=15))+
     # geom_point(size = 3, colour = "blue", alpha = .5)+
     #geom_smooth(method = "lm")+
    # theme(axis.title = element_text(size = 12),
        #   axis.text  = element_text(size = 12),
        #   panel.background=element_rect(fill="white",colour="black")) +
     ylab("Count")+
     xlab("Race")
     

 })

 ### First output "graphs"
 output$error <- renderPlot({
   cond <- demographics$project.ZIP == input$zipcode
   
   demographics$median <- as.numeric(gsub(",","",demographics$median))
   
   matches = subset(demographics, project.ZIP == input$zipcode)
   match = matches[1,]
   
   #cond <- abs(demographics$median - match[1, "income"]) < 9000
   
   inc_max = max(demographics$median)
   inc_min = min(demographics$median)
   bwidth = 18000
   num_bins = (inc_max - inc_min)/bwidth
   bin_id = ceiling((match[1, "median"] - inc_min)/bwidth)
   num_before = bin_id - 1
   num_after = num_bins - bin_id + 1
   
   colors <- c(rep("lightblue",num_before), rep("red",1), rep("lightblue",num_after))
   print(bin_id)
   print(colors)
   
   ggplot(demographics, aes(x=median )) + 
     geom_histogram( binwidth = 18000, fill=colors, color='darkblue') + 
     #geom_histogram(data=subset(demographics, cond==TRUE), fill = "red", binwidth = bwidth) +
     ylab("Count") + 
     xlab ("Income Bracket (as compared to total of LA)") + 
     ggtitle("Income Distribution of Los Angeles") 

 })

# output$variance <- renderPlot({
#  cols <- c("#619CFF", "#00BA38", "#F8766D")
#  ggplot(SSdata(), aes(y = value, x = SS, fill = SS))+
 #  geom_bar(stat = "identity")+
 #  scale_fill_manual(values = cols)+
 #  theme(axis.title = element_text(size = 20),
  #       axis.text.x  = element_text(size = 0),
 #        axis.text.y  = element_text(size = 16),
  #       panel.background=element_rect(fill="white",colour="black")) +
  # ylab("% of variance")+
 #  xlab("Sums of Squares")
#
 #})


 output$reg <- renderPlot({
   matches = subset(demographics, project.ZIP == input$zipcode)
   match = matches[1,]
   
   race = match[,13:19]
   racedf = data.frame(x=c("Hispanic", "White", "Black", "Asian", "Mixed", "Native American", "Pacific Islander"), y=as.numeric(race))
   
  ggplot(racedf, aes(y = y, x = x))+
    geom_bar(stat = "identity")+
    # geom_point(size = 3, colour = "blue", alpha = .5)+
   #geom_smooth(method = "lm")+
   theme(axis.title = element_text(size = 12),
         axis.text  = element_text(size = 12),
         panel.background=element_rect(fill="white",colour="black")) +
   ylab("Count")+
   xlab("Race")

 })

 ### Second output "anova"
 #output$anova <- renderTable({
 # anova(lm(y ~ x, Rawdata()))
 #})

 ### Second output "SS"
 output$summary <- renderTable({
  summary(lm(y ~ x, Rawdata()))

 })

 output$data <- renderPlot({
   data %>%
     filter(Project.Area == input$projectarea ) %>% 
     filter(Year == input$year) %>%
     ggplot(aes(x = Local.Artist)) + 
     geom_bar(color="darkblue", fill="lightblue") + #density plot tied to color entry
     ggtitle("Is the Artist Local?") + #inset input into title
     theme(plot.title = element_text(hjust=0.5, size=15))+
     xlab('Artist Type')
 })
 
 output$data1 <- renderPlot({
   data %>%
     filter(Project.Area == input$projectarea) %>% 
     filter(Year == input$year) %>%
     ggplot(aes(x = Location.Type)) + 
     geom_bar(color="darkblue", fill="lightblue") + #density plot tied to color entry
     ggtitle("Where are the projects located?") + #inset input into title
     theme(plot.title = element_text(hjust=0.5, size=15))+
     xlab('Location Type')
 })
 

 output$histogram1 <- renderPlot({
   data %>%
     filter(!Location.Type == "NA") %>%
     filter(!Year =="NA") %>%
     ggplot(aes(x= factor(Year), fill=Location.Type)) + geom_bar () +
     #ggtitle("Project Locations by Year") + 
     ylab("Number of Projects") + xlab("Year") +
     theme(plot.title = element_text(hjust = 0.5, size=15)) 
   
 #d1 <- ggplot(Rawdata(), aes(y = y, x = x))+
 # geom_point(size = 3, colour = "blue", alpha = .5)+
 # theme(axis.title = element_text(size = 20),
   #     axis.text  = element_text(size = 16),
   #     panel.background=element_rect(fill="white",colour="black")) +
  #ylab("Y")+
#  xlab("X")
 # ggMarginal(
 # d1,
  #type = 'histogram')
})
 
 output$histogram2 <- renderPlot({
   data %>%
     filter(!Local.Artist == "NA") %>%
     filter(!Year =="NA") %>%
     ggplot(aes (x=factor(Year), fill=Local.Artist)) + geom_bar() +
   #  ggtitle("Number of Projects by Year") +
     ylab("Number of Projects") + xlab("Year")+
     theme(plot.title = element_text(hjust = 0.5, size=15)) 
 })
 
 output$histogram3 <- renderPlot({
   data %>%
     filter(!Location.Type == "NA") %>%
     filter(!Year =="NA") %>%
     ggplot(aes (x=Location.Type)) + geom_bar(color="darkblue", fill="lightblue") +
    # ggtitle("Where are the Art Projects Located?  (Overall)") +
     theme(plot.title = element_text(hjust = 0.5, size=15)) 
   
 })
 
 output$histogram4 <- renderPlot({
   data %>%
     filter(!Local.Artist == "NA") %>%
     filter(!Year =="NA") %>%
     ggplot(aes (x=Local.Artist)) + geom_bar(color="darkblue", fill="lightblue") +
    # ggtitle("Is the Artist Local?  (Overall)") +
     theme(plot.title = element_text(hjust = 0.5, size=15)) 
 })
 
 output$crime <- renderPlot({
   rv = projdata %>%
     filter (ZIP == input$zipcode1)
   
   ggplot(rv, aes(Year, Total.Number.of.Crimes, group =1)) + 
     geom_line (color="darkblue") +
     theme(plot.title = element_text(hjust=0.5, size=15))+
     ylab("Total Number of Crimes")+
     xlab("Year")
 })
 
 output$housing <- renderPlot({
   rv = projdata %>%
     filter (ZIP == input$zipcode1)
   
   ggplot(rv, aes(Year, Housing.Prices, group =1)) + 
     geom_line (color="darkblue") +
     theme(plot.title = element_text(hjust=0.5, size=15))+
     ylab("Housing Prices")+
     xlab("Year")
 })
 
 output$business <- renderPlot({
   rv = projdata %>%
     filter (ZIP == input$zipcode1)
   
   ggplot(rv, aes(Year, New.Business, group =1)) + 
     geom_line (color="darkblue") +
    # theme(plot.title = element_text(hjust=0.5, size=15))+
     ylab("New Businesses")+
     xlab("Year")
 })
 
 output$projects <- renderPlot({
   rv = projdata %>%
     filter (ZIP == input$zipcode1)
   
   ggplot(rv, aes(Year, New.Projects, group =1)) + 
     geom_line (color="darkblue") +
     theme(plot.title = element_text(hjust=0.5, size=15))+
     ylab("New Projects")+
     xlab("Year")
 })
 
 observe({
   updateSelectInput(session, "zipcode",
                     choices = as.list(unique(demographics$project.ZIP))
   )})
 
 observe({
   updateSelectInput(session, "year",
                     choices = as.list(unique(data$Year))
   )})
 
 observe({
   updateSelectInput(session, "projectarea",
                     choices = as.list(unique(data$Project.Area))
   )})
 observe({
   updateSelectInput(session, "zipcode1",
                     choices = as.list(unique(projdata$ZIP))
   )})
 })
