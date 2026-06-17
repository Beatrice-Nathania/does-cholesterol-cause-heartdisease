library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(xfun)
library(markdown)
library(reshape2)
library(rsconnect)

rsconnect::setAccountInfo(name='kelompokaol', 
                          token='7B8F2E907ACC805AE168A3F2DC1E12B2', 
                          secret='YyLjDXnGzdUQcoRdGpnhX4KEmdEtP/LsXIgnYUqH')

#getwd()
#setwd("C:/BetrisN/Kuliah/Sems 2/DMV/After/Dataset AOL/shinyAOL")

#import the file
data_final <- read.csv("Heart_clean2.csv", sep =",")


# Prepare the data summary
bp_agre_data <- data_final %>%
  filter(Cholestrol_Cat1 == "High") %>%
  count(AgeCategory, HeartDisease) %>%
  group_by(AgeCategory) %>%
  mutate(
    perc = n / sum(n) * 100,
    Combined = interaction(HeartDisease, AgeCategory, sep = " "))

bp_HDBP_dataGB <- data_final %>%
  filter(Cholestrol_Cat1=="High")%>%
  count(HeartDisease, RestingBP_Cat) %>%
  group_by(RestingBP_Cat) %>%
  mutate(perc = n / sum(n) * 100,Combined = interaction(RestingBP_Cat, HeartDisease, sep = " - "))

hd_clo_data <- data_final %>%
  count(Cholestrol_Cat1, HeartDisease)%>%
  mutate(perc = n / sum(n) * 100,Combined = interaction(Cholestrol_Cat1, HeartDisease, sep = "--")
  )

sum_HR <- data_final%>%
  filter(Cholestrol_Cat1=="High")%>%
  count(HeartDisease, MaxHR_Cat)%>%
  group_by(MaxHR_Cat)%>%
  mutate(perc = round(n / sum(n) * 100),Combined = interaction( MaxHR_Cat,HeartDisease, sep = " - ")
  )

bp_stslop_data <- data_final %>%
  filter(Cholestrol_Cat1=="High")%>%
  count(HeartDisease,ST_Slope) %>%
  group_by(ST_Slope) %>%
  mutate(perc = n / sum(n) * 100,Combined = interaction(HeartDisease, ST_Slope, sep = " - "))

sum_OP <- data_final %>%
  filter(Cholestrol_Cat1=="High")%>%
  count(HeartDisease, Oldpeak_Cat)%>%
  group_by(Oldpeak_Cat) %>%
  mutate(perc = n / sum(n) * 100,Combined = interaction(Oldpeak_Cat, HeartDisease, sep = " - ")
  )

bp_type_data <- data_final %>%
  filter(Cholestrol_Cat1=="High")%>%
  count(HeartDisease, ChestPainType) %>%
  group_by(ChestPainType) %>%
  mutate(perc = n / sum(n) * 100, Combined = interaction(HeartDisease, ChestPainType, sep = " - "))

sum_HD_Angina <- data_final %>%
  filter(Cholestrol_Cat1=="High")%>%
  count(HeartDisease, ExerciseAngina) %>%
  group_by(ExerciseAngina) %>%
  mutate(perc = round(n / sum(n) * 100),Combined = interaction(ExerciseAngina, HeartDisease, sep = " - ")
  )

summary_HD <- data_final %>% 
  filter(Cholestrol_Cat1 == "High")%>%
  count(HeartDisease) %>%
  mutate(perc = n/sum(n) * 100)


sum_gender <- data_final %>%
  filter(Cholestrol_Cat1=="High")%>%
  count(HeartDisease, Sex)%>%
  group_by(Sex)%>%
  mutate(perc = round(n / sum(n) * 100), Combined = interaction(Sex,HeartDisease, sep = " - "))

summary_BSGB <- data_final %>%
  filter(Cholestrol_Cat1=="High")%>%
  count(HeartDisease, FastingBS)%>%
  group_by(FastingBS)%>%
  mutate(perc = round(n / sum(n) * 100),Combined = interaction(FastingBS, HeartDisease, sep = " - "))


#Prepare the data frame for Non-communicable disease
ncD_death <- data.frame(
  disease = c("Heart Disease","Covid 19", "Diabetes", "Stroke", "Chronic Respitory Disease"),
  deaths = c(20.5, 8.00, 6.7, 7, 3.5)
)

ncD_death_sort <- ncD_death[order(ncD_death$deaths, decreasing = TRUE),]

#prepare the data for correlation
datas_num<- data_final%>% filter(Cholestrol_Cat1 == "High")
names(datas_num)
datas_num <- datas_num[c("Age", "RestingBP_T", "Cholesterol_T","gender", "MaxHR", "Oldpeak_T", "outcome", "bloodSugar")]

corr_num <- cor(datas_num)

ordered <- names(sort(corr_num["outcome",], decreasing = TRUE))

order_corr <- corr_num[ordered, ordered]


#Prepare the UI for the app
ui <- fluidPage(theme = shinytheme("cerulean"),
                tags$head(
                  tags$link(href = "https://fonts.googleapis.com/css2?family=Quicksand:wght@400;500;600;700&display=swap", 
                            rel = "stylesheet"),
                  #Text Font, size and weight
                  tags$style(HTML("
                    body {
                      font-family: 'Quicksand', sans-serif;
                      background-color : #e0f3ff;
                      color ; black;
                    }
                    p{
                    font-size = 25px;
                    font-weight: 600;
                    }
                    ol {
                    font-weight: 600;
                    }
                    strong {
                      font-weight: 800;
                      color: darkred;
                      background-color: #FDEDEC; 
                      padding: 2px 5px;
                      border-radius: 3px;
                    }
                    h2,h3{
                      font-family: 'Quicksand', sans-serif;
                      font-weight: 700;
                    }
                    h4{
                      font-family: 'Quicksand', sans-serif;
                      font-weight: 600;
                    }
                    
                    "))
                ),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Is Cholesterol The Culprit Behind Heart Disease?",
                  header = tags$head(
                    tags$style(HTML("
                      .navbar-nav > li > a {
                         font-weight : 600;
                         padding-top: 20px;
                         padding-bottom: 20px;
                         }

     
                      .navbar-brand {
                         font-weight : 600;
                         padding-top: 17px;
                         padding-bottom: 15px;
                         }
                   "))
                  ),
                  
                  #panel with text description and interactive plot display
                  tabPanel("Knowing Heart Disease",
                           mainPanel(
                             includeMarkdown("heartD_Desc.md"),
                             includeMarkdown("bigthreat.md"),
                             plotOutput("death_r", width = "90%"),
                             includeMarkdown("Heartdiseasefact.md"),
                             plotlyOutput("heatmap", width =  "75%"),
                             includeMarkdown("age.md"),
                             selectInput("age_category", "Choose age category :",
                                         choices = bp_agre_data$AgeCategory, 
                                         selected = "75 or Older", multiple = TRUE),
                             
                             #display plot
                             plotlyOutput("plot1", width = "75%", height = "300px"),
                             includeMarkdown("gender.md"),
                             plotlyOutput("plot2", width = "75%", height = "300px"),
                             includeMarkdown("bloodpress.md"),
                             selectInput("pres_category", "Choose Blood Pressure category :",
                                         choices = bp_HDBP_dataGB$RestingBP_Cat, 
                                         selected = "Hypertension", multiple = TRUE),
                             plotlyOutput("bloodpres", width = "75%", height = "300px"),
                             includeMarkdown("bloodsugar2.md"),
                             plotlyOutput("bloodsu", width = "75%", height = "300px"),
                             includeMarkdown("cholestrol.md"),
                             selectInput("choles", "Choose Cholesterol Level :",
                                         choices = hd_clo_data$Cholestrol_Cat1, 
                                         selected = "High", multiple = TRUE),
                             plotlyOutput("chol", width = "75%", height = "300px"),
                             h3("Heart Disease Symptoms"),
                             includeMarkdown("symptoms1.md")
                             
                           ) # mainpanel
                  ), # Navbar 1, tabPanel
                  tabPanel("Patient Overview", 
                           mainPanel(
                             includeMarkdown("dataset.md"),
                             includeMarkdown("choldis.md"),
                             selectInput("chold", "Choose Cholesterol Level :",
                                         choices = data_final$Cholestrol_Cat1, 
                                         selected = "High", multiple = TRUE),
                             plotlyOutput("choldis",width = "75%"),
                             includeMarkdown("choldsfact.md"),
                             plotlyOutput("heartd",width = "75%"),
                             includeMarkdown("heartddis.md"),
                             includeMarkdown("patientsymptoms.md"),
                             plotlyOutput("angina",width = "77%"),
                             includeMarkdown("angina.md"),
                             selectInput("slope", "Choose slope category :",
                                         choices = bp_stslop_data$ST_Slope, 
                                         selected = "Flat", multiple = TRUE),
                             plotlyOutput("st",width = "77%"),
                             includeMarkdown("st_slope.md"),
                             selectInput("heartR", "Choose Heart Rate Level :",
                                         choices = sum_HR$MaxHR_Cat, 
                                         selected = "Low", multiple = TRUE),
                             plotlyOutput("hr", width = "77%"),
                             includeMarkdown("Hr.md"), 
                             plotlyOutput("oP",width = "77%"),
                             selectInput("Oldpea", "Choose Old Peak Category :",
                                         choices = sum_OP$Oldpeak_Cat, 
                                         selected = "Strong Coronary Heart disease", multiple = TRUE),
                             includeMarkdown("oldP.md"),
                             plotlyOutput("cP",width = "90%"),
                             selectInput("chespain", "Choose Chest Pain Type :",
                                         choices = bp_type_data$ChestPainType, 
                                         selected = "Asymptomatic", multiple = TRUE),
                             includeMarkdown("chestpain.md")
                           )
                  ),
                  tabPanel("Conclusion", 
                           mainPanel(
                             includeMarkdown("conclu.md")
                           )
                  ),
                ) # navbarPage
) # fluidPage


# Define server function and plot output
server <- function(input, output, session) {
  filtered_data3 <-reactive({
    input$heartR
    sum_HR%>% 
      filter(MaxHR_Cat %in% input$heartR) %>%
      droplevels()
  }) 
  output$hr <- renderPlotly({
    CustC <- c(
      "High - No Heart disease" = "lightgray", "Normal - No Heart disease" = "lightgray", "Low - No Heart disease" = "#ffcccc",
      "High - Had Heart Disease" = "#969696", "Normal - Had Heart Disease" = "#969696", "Low - Had Heart Disease" = "#f44336"
    )
    
    plot_ly(data = filtered_data3(), x = ~MaxHR_Cat, y = ~perc, color = ~Combined, colors= CustC, type = 'bar') %>%
      layout(
        title = "Patient Maximum heart rate divided by Heart disease",
        xaxis = list(title = "maximum heart rate"),
        yaxis = list(title = "percentage", ticksuffix="%"),
        barmode = 'stack',
        legend = list(title = list(text = "Max Heart Rate - Heart Disease Status"))
      )
  })
  filtered_data4 <-reactive({
    input$chespain
    bp_type_data%>% 
      filter(ChestPainType %in% input$chespain) %>%
      droplevels()
  })
  output$cP <- renderPlotly({
    custom_co <- c("No Heart disease - Typical Angina"= "lightgray", 
                   "No Heart disease - Non-anginal Pain"= "lightgray", "No Heart disease - Asymptomatic"= "#ffcccc",  
                   "No Heart disease - Typical Atypical"= "lightgray",  "Had Heart Disease - Typical Angina"     = "#969696",
                   "Had Heart Disease - Non-anginal Pain"   = "#969696",  "Had Heart Disease - Asymptomatic"       = "#f44336",  
                   "Had Heart Disease - Typical Atypical"   = "#969696"  
    )
    plot_ly(data = filtered_data4(), x = ~ChestPainType, y = ~perc,color = ~Combined, colors=custom_co, type = 'bar'
    ) %>%
      layout(title = "Patient percentage based on Chest pain type divided by Heart disease status",
             xaxis = list(title = "chest pain type"),
             yaxis = list(title = "percentage", ticksuffix = "%"),
             legend = list(title = list(text = "Heart disease status - types of chest pain")),barmode = 'stack'
      )
  })
  filtered_data5 <-reactive({
    input$Oldpea
    sum_OP%>% 
      filter(Oldpeak_Cat %in% input$Oldpea) %>%
      droplevels()
  })
  
  output$oP <- renderPlotly({
    
    
    
    custom_colorscl <- c("Healthy Heart - No Heart disease" = "#d9d9d9", 
                         "Potential Heart Issue - No Heart disease" = "lightgray", 
                         "Strong Coronary Heart disease - No Heart disease" = "#ffcccc",
                         "Healthy Heart - Had Heart Disease" = "#969696", 
                         "Potential Heart Issue - Had Heart Disease" = "#969696", 
                         "Strong Coronary Heart disease - Had Heart Disease" = "#f44336")
    
    
    plot_ly(data = filtered_data5(), x = ~Oldpeak_Cat, y = ~perc, color = ~Combined, colors=custom_colorscl,
            type = 'bar') %>%
      layout(
        title = "Percentage of Patient Oldpeak divided by Heart disease status",
        xaxis = list(title = "oldpeak indicator"),
        yaxis = list(title = "percentage", ticksuffix = "%"),
        legend = list(title = list(text = "oldpeak indicator - Heart Disease status")),
        barmode = 'stack'
      )
  })
  filtered_data6 <-reactive({
    input$slope
    bp_stslop_data%>% 
      filter(ST_Slope %in% input$slope) %>%
      droplevels()
  }) 
  output$st <- renderPlotly({
    customCol<-c("No Heart disease - Down"="lightgray", "No Heart disease - Flat"="#ffcccc", "No Heart disease - Up"="#d9d9d9",
                 "Had Heart Disease - Down"="#969696", "Had Heart Disease - Flat"="#f44336", "Had Heart Disease - Up"= "#969696")
    
    
    plot_ly(data = filtered_data6(), x = ~ST_Slope, y = ~perc, color = ~Combined, colors=customCol, type = 'bar'
    ) %>%
      layout(title = "Percentage of Patient with Heart disease status divided by ST slope",
             xaxis = list(title = "ST slope"),yaxis = list(title = "percentage", ticksuffix = "%"),
             legend = list(title = list(text = "ST slope - Heart disease status ")),
             barmode = 'stack'
      )
  })
  
  output$angina <- renderPlotly({
    custom_co <- c(
      "No - No Heart disease" = "#d9d9d9", "Yes - No Heart disease" = "#ffcccc",
      "No - Had Heart Disease" = "#969696","Yes - Had Heart Disease" = "#f44336"
    )
    
    plot_ly(sum_HD_Angina, x = ~ExerciseAngina,y = ~perc, color = ~Combined, colors=custom_co, type = 'bar') %>%
      layout(
        title = "Percentage of Patient Excersie angina based on Heart disease status",
        xaxis = list(title = "Heart disease status"),
        yaxis = list(title = "percentage", ticksuffix = "%"),
        legend = list(title = list(text = "Heart Disease Status - Excersise Angina")),
        barmode = 'stack'
      )
    
  })
  
  output$heartd <- renderPlotly({
    plot_ly(summary_HD, labels = ~HeartDisease, values = ~n, type = 'pie',textinfo = 'label+percent',
            marker = list(colors = c("#f44336", "#A8DADd")))%>%
      layout(
        title = "Patient Heart Disease Percentage in High Cholesterol Population",
        xaxis = list(title = "Heart Disease"),
        yaxis = list(title = "Persentage", ticksuffix = "%"),
        legend = list(title = list(text = "Heart Disease"))
      )
    
  })
  
  output$choldis <- renderPlotly({
    filtered_data <- data_final %>% 
      filter(Cholestrol_Cat1 %in% input$chold)
    custom_c= c("Normal"="#FFF89A","At Risk" = "#F6BD60","High" = "#FF4500")
    ggplot(filtered_data, aes(x =Cholestrol_Cat1, fill = Cholestrol_Cat1)) +geom_bar(position = "dodge")+
      scale_fill_manual(values = custom_c) +
      labs(
        title = "Amount of Patient with Cholesterol",
        x= "cholesterol Category",
        y = "Amount of Patient",
        fill = "Cholesterol Level"
      )
  })
  
  output$heatmap <- renderPlotly({
    heat <- ggplot(data = melt(order_corr), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      labs(title = "Factor that Cause Heart Disease", x = "Heart Disease Factors", y = "Heartdisease Factors", fill = "Correlation") +
      scale_fill_gradient(low = "#ffcccc", high = "#f44336") +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        panel.background = element_rect(fill = "white")
      )
    ggplotly(heat)
  })
  
  output$plot1 <- renderPlotly({
    custom_colo <- c("<30 Had heart Disease" = "#969696", "No Heart disease < 30"= "#d9d9d9",  
                     "Had Heart Disease 30-44" = "#969696","No Heart disease 30-44"  = "lightgray",
                     "Had Heart Disease 45-59" = "#f44336","No Heart disease 45-59"  = "#ffcccc",
                     "Had Heart Disease 60-74" = "#f44336","No Heart disease 60-74"  = "#ffcccc",   
                     "Had Heart Disease 75 or Older"= "#f44336", "No Heart disease 75 or Older"= "#ffcccc"
    )
    filtered_data <- bp_agre_data %>% 
      filter(AgeCategory %in% input$age_category)
    
    plot_ly(filtered_data, 
            x = ~AgeCategory, 
            y = ~perc, 
            color = ~Combined, 
            colors = custom_colo, type = 'bar') %>%
      layout(
        title = "Percentage of Patient Age with Heart Disease",
        xaxis = list(title = "Age"),
        yaxis = list(title = "Percentage", ticksuffix = "%"),
        legend = list(title = list(text = "Age - Heart Disease Status")), barmode = 'stack'
      )
  })
  
  output$plot2 <- renderPlotly({
    CustC <- c(
      "Male - No Heart disease" = "#ffcccc", "Male - Had Heart Disease" = "#f44336",
      "Female - No Heart disease" = "lightgray", "Female - Had Heart Disease"= "#969696" 
    )
    
    plot_ly(data = sum_gender, x = ~Sex, y = ~perc, color = ~Combined, colors= CustC, type = 'bar') %>%
      layout(
        title = "Percentage of Patient with Heart disease based on Gender",
        xaxis = list(title = "Gender"),
        yaxis = list(title = "Percentage",ticksuffix = "%"),
        barmode = 'stack',
        legend = list(title = list(text = "Gender - Heart Disease Status"))
      )
    
  })
  
  filtered_data <-reactive({
    input$pres_category
    bp_HDBP_dataGB%>% 
      filter(RestingBP_Cat %in% input$pres_category) %>%
      droplevels()
  })
  
  output$bloodpres <- renderPlotly({
    cusColor <- c(
      "Normal - No Heart disease" = "#d9d9d9", "Pre-Hypertensive - No Heart disease" = "lightgray",
      "Hypertension - No Heart disease" = "#ffcccc",
      "Normal - Had Heart Disease" = "#969696","Pre-Hypertensive - Had Heart Disease" = "#969696",
      "Hypertension - Had Heart Disease" = "#f44336"
    )
    plot_ly(data = filtered_data(), x = ~RestingBP_Cat, y = ~perc, 
            color = ~Combined, colors=cusColor, type = 'bar') %>%
      layout(
        title = "Percentage of Patient Blood pressure levels and Heart disease status",
        xaxis = list(title = "Blood pressure Levels"),
        yaxis = list(title = "percentage", ticksuffix = "%"),
        legend = list(title = list(text = "Blood pressure - Heart Disease")),
        barmode = 'stack'
      )
  })
  
  output$bloodsu <- renderPlotly({
    CustC <- c(
      "Otherwise - No Heart disease" = "#d9d9d9", "Blood sugar > 120ml/dl - No Heart disease" = "#ffcccc",
      "Otherwise - Had Heart Disease" = "#969696","Blood sugar > 120ml/dl - Had Heart Disease" = "#f44336"
    )
    
    plot_ly(data = summary_BSGB, x = ~FastingBS,
            y = ~perc,
            color = ~Combined,
            colors=CustC, type = 'bar') %>%
      layout(
        title = "Patient Blood Sugar Level divided by Heart disease",
        xaxis = list(title = "blood sugar level"),
        yaxis = list(title = "percentage", ticksuffix = "%"),
        barmode = 'stack',
        legend = list(title = list(text = "Heart Disease Status - Blood Sugar Level"))
      )
  })
  
  output$death_r <- renderPlot({
    ggplot(ncD_death_sort, aes(y = reorder(disease, deaths), x = deaths, fill = disease)) +
      geom_col(position = "dodge", width = 0.6)+
      geom_text(aes(label = deaths), 
                hjust = -0.1) +
      labs(title = "Non-Communicable Disease Death Rate - 2021",
           x = "Death Rate (in Million)",
           y = "Disease") +
      scale_fill_manual(
        values = c(
          "Heart Disease" = "#f44336",
          "Diabetes" = "#bdbdbd",
          "Covid 19" = "#bdbdbd",
          "Stroke" = "#bdbdbd",
          "Chronic Respiratory Disease" = "#bdbdbd"
        )
      ) +
      theme(
        panel.background = element_rect(fill = "white"),
        legend.position = "none"
      ) +
      coord_cartesian(clip = "off") 
  })
  
  filtered_data2 <- reactive({
    input$choles
    hd_clo_data%>% 
      filter(Cholestrol_Cat1 %in% input$choles) %>%
      droplevels()
  })
  
  output$chol <- renderPlotly({
    customCol <- c(
      "Normal--Had Heart Disease" = "#969696", "Normal--No Heart disease" = "#bdbdbd",
      "At Risk--Had Heart Disease" = "#969696", "At Risk--No Heart disease" = "#bdbdbd",
      "High--Had Heart Disease" = "#f44336", "High--No Heart disease" = "#ffcccc"
    )
    plot_ly(data =filtered_data2() , x = ~Cholestrol_Cat1, y = ~n, 
            color = ~Combined, colors=customCol,type = 'bar'
    ) %>%
      layout(
        title = "Patient Heart disease status divided by Cholesterol levels",
        xaxis = list(title = "Cholesterol level"),
        yaxis = list(title = "Amount"),
        legend = list(title = list(text = "Cholesterol levels - Heart disease status")),barmode = 'stack'
      )
  })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
