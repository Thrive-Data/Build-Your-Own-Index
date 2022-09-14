
#######################
### Build Your Own Index App - Version 3 (Strive Together Presentation)
#######################

### Set wd
#setwd("C:/Users/David/Documents/Thrive Work Stuff/BYMOC Data/byo_index_v3")

### Libraries
library(shiny)
library(shinyjs)
library(dplyr)
library(data.table)
library(tmap)
library(leaflet)
library(rgeos)
library(rgdal)
library(stringr)
library(sp)

library(sf)
library(ggplot2)


library(tidyverse)

library(DT)

### Load Shapefiles
community<-readOGR(dsn=".",layer="community")

### Load Data
mbk_community_ranking<-fread("mbk_community_ranking.csv")
mbk_community_ranking2<-fread("mbk_community_ranking2.csv")

mbk_rankings_blackhisp<-fread("mbk_rankings_blackhisp.csv")

### Load Edu outcomes and partner directory
cps_data<-fread("data.csv")
partner_info<-fread("partner_info3.csv")

### Merge
#mbk_ranking<-full_join(mbk_community_ranking,mbk_community_ranking2)

mbk_ranking<-mbk_community_ranking %>%
    inner_join(mbk_rankings_blackhisp)

#########################
### Clean Data
#########################

mbk_community_ranking<-mbk_ranking %>%
    mutate(total=pr_black_hisp +
               pr_poverty+pr_crime+pr_comm_disconnection+
               pr_no_internet+pr_covid_death_rate+
               pr_no_HI+pr_poor_mh+pr_life_expectency,
           overall_ranking=percent_rank(total),
           overall_ranking=round(overall_ranking,2)
    ) 

#########################
### Clean Data
#########################

### Merge in CPS Data
mbk_community_ranking<-mbk_community_ranking %>%
    left_join({
        cps_data %>%
            dplyr::select(community,hs_graduation_spr2020) %>%
            filter(community != "CPS") %>%
            mutate(pr_low_hs_grad=1-percent_rank(hs_graduation_spr2020))
            
    })

### Merge in Partner Data
mbk_community_ranking<-mbk_community_ranking %>%
    left_join({
        partner_info %>%
            dplyr::select("Organization / Affiliation","Community") %>%
            separate(col = Community,sep = ", ",into = paste("Community",c(1:25),sep = "")) %>%
            melt(id.vars="Organization / Affiliation") %>%
            filter(!is.na(value)) %>%
            mutate(value=toupper(value)) %>%
            
            group_by(community=value) %>%
            summarise(partner_count=n()) 
    }) %>%
    mutate(partner_count=ifelse(is.na(partner_count),0,partner_count)) %>%
    mutate(pr_partner_count=percent_rank(partner_count))

#########################
### Preliminaries
#########################

min_slider <- 0
max_slider <- 5
slider_start <- 0

#########################
### Define App
#########################

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Build Your Own Community Index"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 3,
            tags$h3("Add Weights"),
            
            fluidRow(
                shinyjs::useShinyjs(),
                column(6,actionButton("go","Create Map!")),
                column(6,actionButton("reset","Reset"))
            ),
            
            br(),
            
            h3("Organization Mission and Reach Indicators"),
            
            sliderInput("black_hisp",
                        "Weight for Percent Black/Hispanic Population",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            
            sliderInput("org_count",
                        "Weight for Number of Thrive Partners",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            
            h3("Structural Indicators"),
            
            sliderInput("poverty",
                        "Weight for Families with Children below Poverty Level",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            
            sliderInput("crime",
                        "Weight for Number of Violent Crimes",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            #sliderInput("education",
            #            "Weight for % of Adults without a College Degree",
            #            min = 0,
            #            max = 15,
            #            value = 5),
            #sliderInput("migration",
            #            "Weight for % Households that Lived in a Different Home Last Year",
            #            min = 0,
            #            max = 15,
            #            value = 5),
            sliderInput("disconnection",
                        "Weight for Community Disconnection Rate",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            
            sliderInput("hs_grad",
                        "Weight for Low High School Graduation Rate",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            
            #sliderInput("hs_disconnection",
            #            "Weight for High School Disconnection Rate",
            #            min = 0,
            #            max = 15,
            #            value = 5),
            
            br(),
            h3("COVID Impact Indicators"),
            sliderInput("covid",
                        "Weight for COVID Mortality Rate",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            sliderInput("internet",
                        "Weight for % of Households without Internet Access",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            
            br(),
            h3("Health Indicators"),
            sliderInput("health_insurance",
                        "Weight for % of People without Health Insurance",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            sliderInput("mental_health",
                        "Weight for % of Adults with Poor Mental Health",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start),
            sliderInput("life_expectency",
                        "Weight for Life Expentency",
                        min = min_slider,
                        max = max_slider,
                        value = slider_start)
            
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            width = 9,
            tabsetPanel(type = "tabs",
                        tabPanel("Build Your Own Index",
                                 dataTableOutput("test"),
                                 #fluidRow(
                                 #  leafletOutput("map")
                                 #),
                                 fluidRow(
                                     column(8,leafletOutput("map")),
                                     column(4,id="map_text",
                                            tags$style(type="text/css", HTML("#foo>*{float: left; margin-right: 15px; height: 20px;} #foo {height: 50px;}")),
                                            
                                            tags$p("The map displaying your rankings as determined by the weights you selected. Communities is darker shades of red are communities with higher rankings."))
                                 )
                        ),
                        
                        tabPanel("Methodology",
                                 h3("How are the indexes are calculated?"),
                                 
                                 tags$p("Indexes are calculated using percent ranks. Lets take the equal weights index as an example. For each measure (e.g. percent of the population that is Black and/or Hispanic; COVID mortality rate), communities are assigned a percent rank corresponding to which percentile it's in for that given measure.
                                    For example, Riverdale has the highest percentage of population that are Black and/or Hispanic at over 98% and so is assigned a percent rank of 1. 
                                    South Lawndale has about 95% of its population being Black and/or Hispanic, which is 18th most and has a corresponding 0.77 percent rank. 
                                    Edison Park has the least Black/Hispanic as a percent of the population at 10%, corresponding to a percent rank of 0. This is done for each measure, such that each community has a percent rank for each measure. 
                                    Next, the percent ranks are added together and the sum is given a percent rank following the process outlined above, resulting in the equal weights index."),
                                 tags$br(),
                                 tags$p("A similar procedure is used to calculate the custom indexes. In this case, before the percent ranks are summed they are multiplied by the user assigned weight.
                                    For example, suppose the user applied a weight of 2 to the COVID mortality rate and a weight of 1 to percent of families living in poverty and 0 for all others.
                                    Then the percent ranks for those measure will be multiplies by 2 and 1, respectively, and all others are multiplied by 0. Once these measures are summed they receice a percent rank, resulting in the custom ranking."),
                                 
                                 h3("Data Sources"),
                                 
                                 tags$p(tags$b("Percent Black/Hispanic Population:"),"2019 1-year American Community Survey, US Census"),
                                 tags$p(tags$b("Number of Partners:"),"Thrive Chicago proprietary Partner Directory"),
                                 tags$p(tags$b("Percent of Families with Children below Poverty Level:"),"2019 1-year American Community Survey, US Census"),
                                 tags$p(tags$b("Number of Violent Crimes:"),"Chicago Data Portal (2021 crimes only)."),
                                 tags$p(tags$b("Community Disconnection Rate:"),"Percent of 16-24 year old youth that are not in school or working. 2019 1-year American Community Survey, US Census/IPUMS"),
                                 tags$p(tags$b("High Graduation Rate:"),"HS Graduation Rate from Chicago Public Schools in Spring 2020. To&Through"),
                                 tags$p(tags$b("COVID Mortality Rate:"),"Chicago Data Portal"),
                                 tags$p(tags$b("Percent of Households without Internet Access:"),"2019 1-year American Community Survey, US Census"),
                                 tags$p(tags$b("Percent of People without Health Insurance:"),"2019 1-year American Community Survey, US Census"),
                                 tags$p(tags$b("Percent of Adults with Poor Mental Health:"),"500 Cities Project, CDC. (Data current as of 2017)"),
                                 tags$p(tags$b("Life Expentency:"),"Chicago Health Atlas")
                        )
            )
            
            
            
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    x<-
        eventReactive({
            input$black_hisp
            input$org_count
            input$poverty
            input$crime
            #input$education
            input$disconnection
            input$hs_grad
            #input$hs_disconnection
            #input$migration
            input$covid
            input$internet
            input$health_insurance
            input$mental_health
            input$life_expectency
        }
        ,{
            mbk_community_ranking %>%
                mutate(
                    overall_ranking2 = percent_rank(pr_poverty*input$poverty + pr_crime*input$crime + pr_black_hisp*input$black_hisp + #pr_no_bachelors*input$education + 
                                                        pr_comm_disconnection*input$disconnection + #pr_hs_disconnection*input$hs_disconnection + 
                                                        #pr_moved*input$migration +
                                                        pr_covid_death_rate*input$covid + pr_no_internet*input$internet +
                                                        pr_no_HI*input$health_insurance + pr_poor_mh*input$mental_health + 
                                                        pr_life_expectency*input$life_expectency +
                                                        pr_partner_count*input$org_count + pr_low_hs_grad*input$hs_grad),
                    overall_ranking2=round(overall_ranking2,2),
                    overall_ranking=round(overall_ranking,2),
                    ranking_diff=round(overall_ranking2-overall_ranking,2)
                )
        })
    
    output$test<-renderDT({x() %>% dplyr::select(Community=community,
                                                 `Your Ranking`=overall_ranking2 #,
                                                 #`Index Ranking`=overall_ranking,
                                                 #`Difference in Rankings`=ranking_diff
                                                 )
        })
    
    observeEvent(input$go,{
        output$map <- 
            renderLeaflet({
                community2<-merge(community,x(),by="area_numbe")
                
                tm<-tm_shape(community2) + tm_fill("overall_ranking2",style = "jenks",n=5,title = "Your Rankings",
                                                   id="community.x", 
                                                   popup.vars=c("Your Rankings"="overall_ranking2")) +
                    #tm_text("overall_ranking2",size=.8) +
                    tm_borders() + tm_layout(frame=F)
                
                tmap_leaflet(tm) 
                
            })
        

    })
    
    
    ######################
    ### Reset Button
    ######################
    
    shinyjs::hide("map_text")
    shinyjs::hide("original_map_text")
    shinyjs::hide("diff_map_text")
    
    observeEvent(input$reset, {
        #shinyjs::reset("map")
        #shinyjs::reset("original_map")
        #shinyjs::reset("diff_map")
        
        shinyjs::hide("map")
        shinyjs::hide("original_map")
        shinyjs::hide("diff_map")
        
        shinyjs::hide("map_text")
        
        shinyjs::reset("poverty")
        shinyjs::reset("crime")
        shinyjs::reset("education")
        shinyjs::reset("disconnection")
        shinyjs::reset("hs_disconnection")
        shinyjs::reset("migration")
        shinyjs::reset("covid")
        shinyjs::reset("internet")
        shinyjs::reset("health_insurance")
        shinyjs::reset("mental_health")
        shinyjs::reset("life_expectency")
        
        
        
    })
    
    observeEvent(input$go,{
        shinyjs::show("map")
        shinyjs::show("original_map")
        shinyjs::show("diff_map")
        
        shinyjs::show("map_text")
        shinyjs::show("original_map_text")
        shinyjs::show("diff_map_text")
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

