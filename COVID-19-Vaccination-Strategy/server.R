if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('plotly')) install.packages('plotly'); library('plotly')
if (!require('deSolve')) install.packages('deSolve'); library('deSolve')
if (!require('shinyjs')) install.packages('shinyjs'); library('shinyjs')

server <- function(input, output) {
    
    observeEvent(input$reset, {
        shinyjs::reset("TransmissionRate") 
        shinyjs::reset("LatentPeriod") 
        shinyjs::reset("InfectiousPeriod.1") 
        shinyjs::reset("InfectiousPeriod.2") 
        shinyjs::reset("ContactRate.1") 
        shinyjs::reset("ContactRate.2") 
        
    })
    
    ##############################################
    # Data preparation 
    
    strategy1 <- read.csv(file="/Users/yufeizou/COVID-19VaccinationStrategy/strategy1.csv", header=TRUE, sep=",")
    strategy2 <- read.csv(file="/Users/yufeizou/COVID-19VaccinationStrategy/strategy2.csv", header=TRUE, sep=",")
    
    # Strategy 1
    # Group 1: Aged 0-17,  Aged 70+, Health care workers targeted for priority vaccination, Group living settings for seniors
    # Group 2: Aged 18-69
    
    strategy1$totalpopulation <- (strategy1$numtotal_atleast1dose) / (strategy1$prop_atleast1dose/100)
    strategy1$totaldoses <- ((strategy1$numtotal_atleast1dose) / (strategy1$prop_atleast1dose/100)) * 2
    strategy1$doesneeded <- strategy1$totaldoses - strategy1$numtotal_atleast1dose
    
    strategy1[6,11] <- strategy1[1,11] + strategy1[3,11] + strategy1[4,11] + strategy1[5,11]
    strategy1[6,12] <- strategy1[1,12] + strategy1[3,12] + strategy1[4,12] + strategy1[5,12]
    strategy1[6,13] <- strategy1[1,13] + strategy1[3,13] + strategy1[4,13] + strategy1[5,13]
    
    per.1 <- 10323052 / 74303078 # Percentage of Group 1 doses needed: 0.1389317
    strategy1.group1.duaration <- 210 * 0.1389317 # 29.17566 days needed
    
    # Strategy 2
    # Group 1: Ontario, Quebec, Alberta
    # Group 2: British Columbia, Saskatchewan, Manitoba, Newfoundland and Labrador, New Brunswick, Nova Scotia, Prince Edward Island
    
    strategy2$doesneeded <- strategy2$total_population * 2 - strategy2$numtotal_all_administered
    
    per.1 <- 54269412 / 74308612 # Percentage of Group 1 doses needed: 0.7303247
    strategy2.group1.duaration <- 210 * 0.7303247 # 153 days needed
    
    
    ##############################################
    # Strategy 1
    
    output$plot1 <- renderPlotly({
        
        seir_model = function (current_timepoint, state_values, parameters) {
            S = state_values [1]        # susceptibles
            E = state_values [2]        # exposed
            I = state_values [3]        # infectious
            R = state_values [4]        # recovered
            
            with ( 
                as.list (parameters), 
                {
                    dS = (-beta * S * I)
                    dE = (beta * S * I) - (delta * E)
                    dI = (delta * E) - (gamma * I)
                    dR = (gamma * I)
                    
                    results = c (dS, dE, dI, dR)
                    list (results)
                }
            )
        }
        
        contact_rate = input$ContactRate.1                     # number of contacts per day
        transmission_probability = input$TransmissionRate       # transmission probability: 06 February 2021
        infectious_period = input$InfectiousPeriod.1                 # infectious period
        latent_period = input$LatentPeriod                    # latent period 2
        
        beta_value = contact_rate * transmission_probability
        gamma_value = 1 / infectious_period
        delta_value = 1 / latent_period
        
        Ro = beta_value / gamma_value
        
        # Disease dynamics parameters.
        parameter_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value)
        
        W1 = 28914752     # susceptible hosts: 31544348 - 5535991/2*0.95 = 28914752
        X1 = 30335        # infectious hosts: 30335
        Y1 = 3435613      # recovered hosts: 806017 + 5535991/2*0.95 = 3435613
        Z1 = 6460890      # exposed hosts: 38005238 * 0.17 # 6460890
        
        # Compute total population.
        N1 = W1 + X1 + Y1 + Z1
        
        # Initial state values for the differential equations.
        initial_values.1 = c (S1 = W1/N1, E1 = X1/N1, I1 = Y1/N1, R1 = Z1/N1)
        
        # Output timepoints.
        timepoints.1 = seq (1, 214, by=1)
        
        # Simulate the SEIR epidemic.
        output.1 = lsoda (initial_values.1, timepoints.1, seir_model, parameter_list)
        
        date <- data.frame(seq(as.Date("2021-03-01"), as.Date("2021-09-30"), by="days"))
        colnames(date) <- "date"
        output.1 <- cbind(date, output.1)
        
        m <- list(l = 50, r = 50, b = 75, t = 75, pad = 4)
        f14 <- list(family = "sans serif", size = 14, color = 'Black')
        f12 <- list(family = "sans serif", size = 12, color = 'Black')
        
        fig.1 <- plot_ly(output.1, x = ~date)
        fig.1 <- fig.1 %>% add_trace(y = ~S1, name = 'Susceptible', mode = 'markers') 
        fig.1 <- fig.1 %>% add_trace(y = ~E1, name = 'Exposed', mode = 'markers') 
        fig.1 <- fig.1 %>% add_trace(y = ~I1, name = 'Infectious', mode = 'markers')
        fig.1 <- fig.1 %>% add_trace(y = ~R1, name = 'Recovered', mode = 'markers')
        fig.1 <- fig.1 %>% layout(title = 'SEIR Epidemic',
                                  xaxis = list(title = 'Date (Month)', font = f12),
                                  yaxis = list(title = 'S E I R'), 
                                  font = f14, margin = m)
        
        fig.1
    })
    
    
    ##############################################
    # Strategy 2
    
    output$plot2 <- renderPlotly({
        
        seir_model = function (current_timepoint, state_values, parameters)
        {
            
            S = state_values [1]        # susceptibles
            E = state_values [2]        # exposed
            I = state_values [3]        # infectious
            R = state_values [4]        # recovered
            
            with ( 
                as.list (parameters),
                {
                    dS = (-beta * S * I)
                    dE = (beta * S * I) - (delta * E)
                    dI = (delta * E) - (gamma * I)
                    dR = (gamma * I)
                    
                    results = c (dS, dE, dI, dR)
                    list (results)
                }
            )
        }
        
        contact_rate = input$ContactRate.2                     # number of contacts per day
        transmission_probability = input$TransmissionRate       # transmission probability: 06 February 2021
        infectious_period = input$InfectiousPeriod.2                # infectious period
        latent_period = input$LatentPeriod                    # latent period 2
        
        beta_value = contact_rate * transmission_probability
        gamma_value = 1 / infectious_period
        delta_value = 1 / latent_period
        
        Ro = beta_value / gamma_value
        
        parameter_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value)
        
        W2 = 18369268        # susceptible hosts: 31544348 - 27737010/2*0.95 = 18369268
        X2 = 30335           # infectious hosts: 30335
        Y2 = 13981097        # recovered hosts: 806017 + 27737010/2*0.95 = 13981097
        Z2 = 6460890         # exposed hosts: 38005238 * 0.17 # 6460890
        
        # Compute total population.
        N2 = W2 + X2 + Y2 + Z2
        
        # Initial state values for the differential equations.
        initial_values.2 = c (S2 = W2/N2, E2 = X2/N2, I2 = Y2/N2, R2 = Z2/N2)
        
        # Output timepoints.
        timepoints.2 = seq (1, 214, by=1)
        
        # Simulate the SEIR epidemic.
        output.2 = lsoda (initial_values.2, timepoints.2, seir_model, parameter_list)
        
        date <- data.frame(seq(as.Date("2021-03-01"), as.Date("2021-09-30"), by="days"))
        colnames(date) <- "date"
        output.2 <- cbind(date, output.2)
        
        
        m <- list(l = 50, r = 50, b = 75, t = 75, pad = 4)
        f14 <- list(family = "sans serif", size = 14, color = 'Black')
        f12 <- list(family = "sans serif", size = 12, color = 'Black')
        
        fig.2 <- plot_ly(output.2, x = ~date)
        fig.2 <- fig.2 %>% add_trace(y = ~S2, name = 'Susceptible', mode = 'markers') 
        fig.2 <- fig.2 %>% add_trace(y = ~E2, name = 'Exposed', mode = 'markers') 
        fig.2 <- fig.2 %>% add_trace(y = ~I2, name = 'Infectious', mode = 'markers')
        fig.2 <- fig.2 %>% add_trace(y = ~R2, name = 'Recovered', mode = 'markers')
        fig.2 <- fig.2 %>% layout(title = 'SEIR Epidemic',
                                  xaxis = list(title = 'Date (Month)', font = f12),
                                  yaxis = list(title = 'S E I R'), 
                                  font = f14, margin = m)
        
        fig.2
        
    })
    
    url0 <- a("https://github.com/yuz220/COVID-19VaccinationStrategy.git", href="https://github.com/yuz220/COVID-19VaccinationStrategy.git")
    output$tab0 <- renderUI({
        tagList("View Code:", url0)})
    
    url1 <- a("https://med.stanford.edu/content/dam/sm/id/documents/COVID/AsymptCOVID_TransmissionShip.pdf", href="https://med.stanford.edu/content/dam/sm/id/documents/COVID/AsymptCOVID_TransmissionShip.pdf")
    output$tab1 <- renderUI({
        tagList("1:", url1)})
    
    url2 <- a("https://health-infobase.canada.ca/covid-19/vaccine-administration/", href="https://health-infobase.canada.ca/covid-19/vaccine-administration/")
    output$tab2 <- renderUI({
        tagList("2:", url2)})
    
    url3<- a("https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html#a1", href="https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html#a1")
    output$tab3 <- renderUI({
        tagList("3:", url3)})
    
    url4<- a("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501", href="https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501")
    output$tab4 <- renderUI({
        tagList("4:", url4)})
    
    
}