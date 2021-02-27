if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('deSolve')) install.packages('deSolve'); library('deSolve')
if (!require('shinyjs')) install.packages('shinyjs'); library('shinyjs')

ui <- fluidPage(
    
    useShinyjs(),
    actionButton("reset", "Clear"), 
    
    titlePanel("Covid-19 Vaccination Strategy"),
    
    sidebarPanel(
        h4("Introduction", style = "font-family: 'times'; font-si24pt"),
        p("This project is comparing two Covid-19 vaccination strategies to better minimize the infectious/death rate.", style = "font-family: 'times'; font-si18pt"), 
        
        br(),
        sliderInput("TransmissionRate", label = "Transmission Rate", min = 0, max = 2, value = 0.85, step = 0.01),
        sliderInput("LatentPeriod", label = "Latent Period", min = 0, max = 21, value = 14, step = 1),
        sliderInput("InfectiousPeriod.1", label = "Strategy 1: Infectious Period", min = 0, max = 21, value = 5, step = 1),
        sliderInput("InfectiousPeriod.2", label = "Strategy 2: Infectious Period", min = 0, max = 21, value = 12, step = 1),
        sliderInput("ContactRate.1", label = "Strategy 1: Number of contacts", min = 0, max = 50, value = 25, step = 5),
        sliderInput("ContactRate.2", label = "Strategy 2: Number of contacts", min = 0, max = 50, value = 10, step = 5),
    ),
    
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("ABOUT", 
                             br(),
                             p("This project is comparing two Covid-19 vaccination strategies done by the end of September 2021.", 
                               style = "font-family: 'times'; font-si16pt"),
                             
                             p("To define the best strategy, I put two priorities in my two strategies to minimize the infectious and death rate. 
                             One is to protect the people who are most vulnerable (key groups). Another is to protect who have the most exposures to the virus. (regional groups)", 
                               style = "font-family: 'times'; font-si16pt"),
                             
                             br(),
                             p("By Feb 25 2021, there have been 1707398 administered. 
                               According to Statistics Canada, Canada's population is about 38,005,238 in 2020. 
                               Thus, there are still 74,303,078 vaccine doses needed so that everyone can get two doses.", 
                               style = "font-family: 'times'; font-si16pt"), 
                             
                             br(),
                             strong("Strategy 1: Key group priority", style = "font-family: 'times'; font-si16pt"),
                             p("Key group(prioritized group) includes people aged 1-17, people aged 70+, Health care workers, 
                               Group living settings for seniors; Another group includes people aged 18-69.", 
                               style = "font-family: 'times'; font-si16pt"),
                             p("According to the data reported by Health Canada, the population for key group is about 5,535,991 and 10,323,052 vaccine doses needed.
                               In order to complete the vaccination plan by the end of September 2021, key group priorize to get vaccined from March 1 to March 30, 2021.", 
                               style = "font-family: 'times'; font-si16pt"),
                             
                             strong("Strategy 2: Region priority", style = "font-family: 'times'; font-si16pt"),
                             p("Prioritized group includes Alberta, Ontario, Quebec which have confirmed cases more than 100,000. 
                               Another group includes the other provinces.",
                               style = "font-family: 'times'; font-si16pt"),
                             p("According to Health Canada, the population for the priorized group is about 27,737,010.
                               In order to complete by the end of September 2021, the priorized group can get vaccined from March 1 to July 20, 2021.", 
                               style = "font-family: 'times'; font-si16pt"),
                             
                             br(), 
                             uiOutput("tab0", style = "font-family: 'times'; font-si16pt")
                             
                    ), 
                    
                    tabPanel("Strategy1", plotlyOutput("plot1"),
                             strong("Strategy 1: Key group priority", style = "font-family: 'times'; font-si16pt"),
                             p("Key group(prioritized group) includes people aged 1-17, people aged 70+, Health care workers, 
                                 Group living settings for seniors; Another group includes people aged 18-69.", 
                               style = "font-family: 'times'; font-si16pt"),
                             p("According to the data reported by Health Canada, the population for key group is about 5,535,991 and 10,323,052 vaccine doses needed.
                                 In order to complete the vaccination plan by the end of September 2021, key group priorize to get vaccined from March 1 to March 30, 2021.", 
                               style = "font-family: 'times'; font-si16pt"), 
                             
                             br(),
                             strong("Default Assumptions", style = "font-family: 'times'; font-si16pt"), 
                             p("Transmission Rate: 0.85 (According to Health Canada's updated reports)", style = "font-family: 'times'; font-si16pt"), 
                             p("Latent Period: 14 (Average latent period for all people.)", style = "font-family: 'times'; font-si16pt"), 
                             p("Infectious Period: 5 (Infectious Periods are usually from within one week to two weeks. 
                                   After key group gets vaccinated, people aged 18-69 could usually recover in a shorter period for they are stronger.)", 
                               style = "font-family: 'times'; font-si16pt"), 
                             p("Number of contacts: 25 (People aged 18-69 usually have more outdoor activities.)", style = "font-family: 'times'; font-si16pt")
                    ),
                    
                    tabPanel("Strategy2", plotlyOutput("plot2"), 
                             strong("Strategy 2: Region priority", style = "font-family: 'times'; font-si16pt"),
                             p("Prioritized group includes Alberta, Ontario, Quebec which have confirmed cases more than 100,000.
                                  Another group includes the other provinces.",
                               style = "font-family: 'times'; font-si16pt"),
                             p("According to Health Canada, the population for the priorized group is about 27,737,010.
                                 In order to complete by the end of September 2021, the priorized group can get vaccined from March 1 to July 20, 2021.", 
                               style = "font-family: 'times'; font-si16pt"), 
                             
                             br(),
                             strong("Default Assumptions", style = "font-family: 'times'; font-si16pt"), 
                             p("Transmission Rate: 0.85 (According to Health Canada's updated reports)", style = "font-family: 'times'; font-si16pt"), 
                             p("Latent Period: 14 (Average latent period for all people.)", style = "font-family: 'times'; font-si16pt"), 
                             p("Infectious Period: 12 (Average infectious period for all people.)", style = "font-family: 'times'; font-si16pt"), 
                             p("Number of contacts: 10 (Average number of contracts for all people.)", style = "font-family: 'times'; font-si16pt"), 
                    ),
                    
                    tabPanel("References", 
                             br(),
                             uiOutput("tab1", style = "font-family: 'times'; font-si16pt"),
                             uiOutput("tab2", style = "font-family: 'times'; font-si16pt"),
                             uiOutput("tab3", style = "font-family: 'times'; font-si16pt"), 
                             uiOutput("tab4", style = "font-family: 'times'; font-si16pt")
                             
                    )
                    
        )
    )
)