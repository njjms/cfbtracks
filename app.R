library(shiny)
library(tidyverse)
library(ggrepel)
library(shinyjs)
library(plotly)

setwd("C:/Users/njnic/Documents/CFBtracks")
wintalentdata <- read.csv("wintalentdata.csv")
wintalentdata <- wintalentdata[,-c(1,2)]
colnames(wintalentdata) <- c("School", "Season", "coaches", "Wins", "Talent Rating", "color", "Conference")

b10_teams <- c("Ohio State",
               "Michigan",
               "Penn State",
               "Maryland",
               "Nebraska",
               "Michigan State",
               "Wisconsin",
               "Iowa",
               "Northwestern",
               "Minnesota",
               "Indiana",
               "Illinois",
               "Rutgers",
               "Purdue")

sec_teams <- c("Alabama",
               "Georgia",
               "LSU",
               "Florida",
               "Auburn",
               "Tennessee",
               "Texas A&M",
               "South Carolina",
               "Ole Miss",
               "Mississippi State",
               "Arkansas",
               "Kentucky",
               "Vanderbilt",
               "Missouri")

b12_teams <- c("Texas",
               "Oklahoma",
               "TCU",
               "Baylor",
               "West Virginia",
               "Oklahoma State",
               "Iowa State",
               "Texas Tech",
               "Kansas State",
               "Kansas")

p12_teams <- c("USC",
               "Stanford",
               "Washington",
               "UCLA",
               "Oregon",
               "Arizona State",
               "California",
               "Utah",
               "Arizona",
               "Colorado",
               "Washington State",
               "Oregon State")

acc_teams <- c("Clemson",
               "Florida State",
               "Miami",
               "North Carolina",
               "Virginia Tech",
               "Pittsburgh",
               "Duke",
               "Louisville",
               "NC State",
               "Georgia Tech",
               "Syracuse",
               "Virginia",
               "Boston College",
               "Wake Forest")

ind_teams <- c("Notre Dame",
               "BYU",
               "UMass",
               "Army")

aac_teams <- c("Houston",
               "UCF",
               "South Florida",
               "Cincinnati",
               "SMU",
               "Memphis",
               "Temple",
               "Tulane",
               "East Carolina",
               "Connecticut",
               "Tulsa",
               "Navy")
               
cusa_teams <- c("Florida Atlantic",
                 "Louisiana Tech",
                 "Florida International",
                 "Marshall",
                 "Middle Tennessee",
                 "UT San Antonio",
                 "UAB",
                 "Southern Mississippi",
                 "Rice",
                 "North Texas",
                 "Western Kentucky",
                 "UTEP",
                 "Old Dominion",
                 "Charlotte")

sun_teams <- c("Appalachian State",
               "Arkansas State",
               "Coastal Carolina",
               "Georgia Southern",
               "Georgia State",
               "Louisiana",
               "Louisiana Monroe",
               "South Alabama",
               "Texas State",
               "Troy")
               
mid_teams <- c("Toledo",
               "Eastern Michigan",
               "Western Michigan",
               "Northern Illinois",
               "Miami (OH)",
               "Central Michigan",
               "Bowling Green",
               "Buffalo",
               "Ball State",
               "Akron",
               "Kent State",
               "Ohio")

mw_teams <- c("Boise State",
              "Colorado State", 
              "San Diego State",
              "Fresno State",
              "Nevada",
              "San Jose State",
              "New Mexico",
              "UNLV",
              "Utah State",
              "Wyoming",
              "Hawai'i",
              "Air Force")

ui <- navbarPage(
  
  "CFB",
  
  tabPanel("Visualize",
           
    sidebarPanel(
      
      # helpText("Filter through different football conferences using the dropdown selector. Select teams to display using the checkboxes."),
      # helpText("Teams from different conferences can be displayed at the same time."), 
      sliderInput("range", "Years to Examine", min = 2015, max = 2019, step = 1, value = c(2015, 2019), sep = ""),
      selectInput(inputId = "conference", label = "Select a conference",
                  choices = unique(wintalentdata$Conference)[order(unique(wintalentdata$Conference))]
                  ),
      conditionalPanel(
        condition = "input.conference == 'Big Ten'",
        checkboxInput("allb10", "Select/Deselect All Big Ten Teams", value = FALSE),
        checkboxGroupInput(inputId = "b10_teams", label = "Teams",
                           choices = b10_teams,
                           selected = c()
      )
    ),
      conditionalPanel(
        condition = "input.conference == 'SEC'",
        checkboxInput("allsec", "Select/Deselect All SEC Teams", value = FALSE),
        checkboxGroupInput(inputId = "sec_teams", label = "Teams",
                           choices = sec_teams,
                           selected = c()
      )
    ),
      conditionalPanel(
        condition = "input.conference == 'Big 12'",
        checkboxInput("allb12", "Select/Deselect All Big 12 Teams", value = FALSE),
        checkboxGroupInput(inputId = "b12_teams", label = "Teams",
                           choices = b12_teams,
                           selected = c()
      )
    ),
    conditionalPanel(
        condition = "input.conference == 'Pac-12'",
        checkboxInput("allp12", "Select/Deselect All Pac-12 Teams", value = FALSE),
        checkboxGroupInput(inputId = "p12_teams", label = "Teams",
                           choices = p12_teams,
                           selected = c()
      )
    ),
    conditionalPanel(
        condition = "input.conference == 'ACC'",
        checkboxInput("allacc", "Select/Deselect All ACC Teams", value = FALSE),
        checkboxGroupInput(inputId = "acc_teams", label = "Teams",
                           choices = acc_teams,
                           selected = c()
      )
    ),
    conditionalPanel(
        condition = "input.conference == 'FBS Independents'",
        checkboxInput("allind", "Select/Deselect All Independent Teams", value = FALSE),
        checkboxGroupInput(inputId = "ind_teams", label = "Teams",
                           choices = ind_teams,
                           selected = c()
      )
    ),
    conditionalPanel(
        condition = "input.conference == 'American Athletic'",
        checkboxInput("allaac", "Select/Deselect All AAC Teams", value = FALSE),
        checkboxGroupInput(inputId = "aac_teams", label = "Teams",
                           choices = aac_teams,
                           selected = c()
      )
    ),
    conditionalPanel(
        condition = "input.conference == 'Conference USA'",
        checkboxInput("allcusa", "Select/Deselect All Conference USA Teams", value = FALSE),
        checkboxGroupInput(inputId = "cusa_teams", label = "Teams",
                           choices = cusa_teams,
                           selected = c()
      )
    ),
    conditionalPanel(
        condition = "input.conference == 'Mid-American'",
        checkboxInput("allmid", "Select/Deselect All Mid-American Teams", value = FALSE),
        checkboxGroupInput(inputId = "mid_teams", label = "Teams",
                           choices = mid_teams,
                           selected = c()
      )
    ),
    conditionalPanel(
        condition = "input.conference == 'Sun Belt'",
        checkboxInput("allsun", "Select/Deselect All Sun Belt Teams", value = FALSE),
        checkboxGroupInput(inputId = "sun_teams", label = "Teams",
                           choices = sun_teams,
                           selected = c()
      )
    ),
    conditionalPanel(
        condition = "input.conference == 'Mountain West'",
        checkboxInput("allmw", "Select/Deselect All Mountain West Teams", value = FALSE),
        checkboxGroupInput(inputId = "mw_teams", label = "Teams",
                           choices = mw_teams,
                           selected = c()
      )
    ),
    actionButton("clear", "Clear All Selections")
    ),
    mainPanel(
      plotlyOutput(outputId = "plot", height = "700px")
    )
  ),
  
  tabPanel("How to Use",
           headerPanel(includeMarkdown("README.md")),
  
           mainPanel(
             img(src = "exampleplot.png", align = "center", height = "500px", width = "auto")
           )
  ),
  
  useShinyjs()
  
)
           
server <- function(input, output, session) {
  
  observe({
    updateCheckboxGroupInput(session,
                             'b10_teams',
                             choices = b10_teams,
                             selected = if(input$allb10) b10_teams)
  })
  
  observe({
    updateCheckboxGroupInput(session,
                             'sec_teams',
                             choices = sec_teams,
                             selected = if(input$allsec) sec_teams)
  })
  
  observe({
    updateCheckboxGroupInput(session,
                             'acc_teams',
                             choices =acc_teams,
                             selected = if(input$allacc) acc_teams)
  })
  
  
  observe({
    updateCheckboxGroupInput(session,
                             'b12_teams',
                             choices = b12_teams,
                             selected = if(input$allb12) b12_teams)
  })
  
  observe({
    updateCheckboxGroupInput(session,
                             'p12_teams',
                             choices = p12_teams,
                             selected = if(input$allp12) p12_teams)
  })
  
  
  observe({
    updateCheckboxGroupInput(session,
                             'ind_teams',
                             choices = ind_teams,
                             selected = if(input$allind) ind_teams)
  })
  
  observe({
    updateCheckboxGroupInput(session,
                             'mid_teams',
                             choices = mid_teams,
                             selected = if(input$allmid) mid_teams)
  })
  
  observe({
    updateCheckboxGroupInput(session,
                             'aac_teams',
                             choices = aac_teams,
                             selected = if(input$allaac) aac_teams)
  })
  
  observe({
    updateCheckboxGroupInput(session,
                             'sun_teams',
                             choices = sun_teams,
                             selected = if(input$allsun) sun_teams)
  })
  
  observe({
    updateCheckboxGroupInput(session,
                             'mw_teams',
                             choices = mw_teams,
                             selected = if(input$allmw) mw_teams)
  })
  
  observe({
    updateCheckboxGroupInput(session,
                             'cusa_teams',
                             choices = cusa_teams,
                             selected = if(input$allcusa) cusa_teams)
  })
  
  selectedteams <- reactive({
    wintalentdata %>% filter(School %in% c(input$b10_teams,
                                           input$b12_teams,
                                           input$sec_teams,
                                           input$acc_teams,
                                           input$p12_teams,
                                           input$mid_teams,
                                           input$aac_teams,
                                           input$sun_teams,
                                           input$mw_teams,
                                           input$cusa_teams,
                                           input$ind_teams) &
                               Season %in% seq(input$range[1], input$range[2], by = 1))
    
  })
  
  observeEvent(input$clear, {
    reset("b10_teams")
    reset("allb10")
    reset("acc_teams")
    reset("allacc")
    reset("b12_teams")
    reset("allb12")
    reset("p12_teams")
    reset("allp12")
    reset("sec_teams")
    reset("allsec")
    reset("ind_teams")
    reset("allind")
    reset("aac_teams")
    reset("allaac")
    reset("mid_teams")
    reset("allmid")
    reset("sun_teams")
    reset("allsun")
    reset("mw_teams")
    reset("allmw")
    reset("cusa_teams")
    reset("allcusa")
  })
  
  output$exampleplot <- renderPlotly({

      q <- wintalentdata %>%
        filter(School %in% c("Penn State", "USC", "Oregon State", "Army", "Boise State")) %>%
        ggplot() +
        geom_point(mapping = aes(x = Wins, y = `Talent Rating`, color = School), alpha = .5, size = 2) +
        geom_path(mapping = aes(x = Wins, y = `Talent Rating`, color = School, group = School), size = 1.0) +
        geom_text(subset(wintalentdata, School %in% c("Penn State", "USC", "Oregon State", "Army", "Boise State") & Season == 2018),
                  mapping = aes(x = Wins, y = `Talent Rating`-10, label = paste(School, Season)), size = 3) +
        scale_color_manual(
          values = as.character(unique(select(arrange(subset(wintalentdata, School %in% c("Penn State", "USC", "Oregon State", "Army", "Boise State")), School), c("School", "color")))$color)
        ) +
        scale_x_continuous("Number of Wins",
                           limits = c(0, 15),
                           breaks = seq(0, 15, 1)) +
        scale_y_continuous("Composite Talent Score",
                           breaks = seq(0, 1000, 100)) +
        theme(
          legend.position = "none",
          panel.grid.minor.x = element_blank()
        )

      ggplotly(q)

    }
  )
  
  output$plot <- renderPlotly({
   
    if (nrow(selectedteams()) == 0) {
      
      p <- ggplot() +
      scale_x_continuous("Number of Wins",
                         limits = c(0, 15),
                         breaks = seq(0, 15, 1)) +
      scale_y_continuous("Composite Talent Score",
                         breaks = seq(0, 1000, 100)) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank()
      )
      
      ggplotly(p)
      
    } else {
      
      p <- ggplot(selectedteams(), aes(text = str_c("Head Coach(es): ", coaches,
                                                    "<br>Season: ", Season))) +
      geom_point(mapping = aes(x = Wins, y = `Talent Rating`, color = School), alpha = .5, size = 2) +
      geom_path(mapping = aes(x = Wins, y = `Talent Rating`, color = School, group = School), size = 1.0) +
      geom_text(subset(selectedteams(), Season == input$range[2]),
                mapping = aes(x = Wins, y = `Talent Rating`-10, label = paste(School, Season)), size = 3) +
      scale_color_manual(
        values = as.character(unique(select(arrange(selectedteams(), School), c("School", "color")))$color)
      ) +
      scale_x_continuous("Number of Wins",
                         limits = c(0, 15),
                         breaks = seq(0, 15, 1)) +
      scale_y_continuous("Composite Talent Score",
                         breaks = seq(0, 1000, 100)) +
      theme(
        legend.position = "none",
        panel.grid.minor.x = element_blank()
      )
      
      ggplotly(p)
      
    }
  })
  
}

shinyApp(ui = ui, server = server)
