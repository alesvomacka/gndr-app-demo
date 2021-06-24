library(tidyverse)
library(scales)
library(ggrepel)
library(shiny)

rd_p_femres <- read_rds("data/rd_p_femres.rds")
rd_p_femres_sector <- unique(rd_p_femres$sectperf)
rd_p_femres_unit <- unique(rd_p_femres$unit)

paygap <- read_rds("data/pay_gap.rds")
paygap_sector <- unique(paygap$sectperf)

ui <- fluidPage(
        theme = bslib::bs_theme(bootswatch = "united"),
        mainPanel(
            tabsetPanel(
                id = "tabset",
                tabPanel("Home",
                         sidebarLayout(
                             sidebarPanel(
                                 "If something breaks, please contact me at alesvomacka@seznam.cz"
                             ),
                             mainPanel(
                                 h1("Home"),
                                 p("This is a demo for an interactive Shiny application for presenting data on gender and research.
                                 The main advantage is that readers are not constrained by the static format format of the classic
                                 reports and can explore the data in any way they desire. Classic static reports can be used to highlight
                                 importand events and trends that happen across time."),
                                 br(),
                                 p("There are two tabs in this application. One lets you explore the how the share of female reseachers in the Czech republic
                                   changed across time and different economic sectors. The second one explores the relationship between the 
                                   gender pay gap and the share of female researchers in the country.")
                             )
                         )),
                tabPanel("Share of female researchers by sector of performance",
                         sidebarLayout(
                            sidebarPanel(
                                selectInput("sector", "sector",
                                            choices = rd_p_femres_sector,
                                            selected = "All sectors"),
                                selectInput("unit", "unit",
                                            choices = rd_p_femres_unit)
                         ),
                            mainPanel(
                                h1("Share of female researchers by sector of performance"),
                                p("Researchers are professionals engaged in the conception or creation of new knowledge, products, processes, methods and systems, and in the management of the projects concerned.
                                The share of women researchers in Czech Republic among total researchers across sectors and in time is shown in the plot below.
                                "),
                                br(),
                                p("The share of women among researchers is relatively stable in time across all sectors, with the exception of the private non-profit sector,
                                  which saw a sharp raise between 2018 and 2019. The share of female researchers is also relatively low across most sectors, most notably in the bussiness enterprise sector."),
                                plotOutput("femres"),
                                br(),
                                p("The headcount, at its most basic, is simply the number of people employed by a business at a given time."),
                                br(),
                                p("Full-time equivalent (FTE), or whole time equivalent (WTE), is a unit that indicates the workload of an employed person (or student) in a way that makes workloads or class loads comparable across various contexts. FTE is often used to measure a worker's or student's involvement in a project, or to track cost reductions in an organization. An FTE of 1.0 is equivalent to a full-time worker or student, while an FTE of 0.5 signals half of a full work"),
                                br()
                            ))),
                tabPanel("Gender pay gap and researchers",
                         sidebarLayout(
                             sidebarPanel(
                                 selectInput("sector2", "Sector",
                                             choices = paygap_sector,
                                             selected = "All sectors")
                             ),
                         mainPanel(
                                 h1("Share of female researchers vs gender pay gap"),
                                 p("The gender pay gap measures the difference between average gross hourly earnings of male paid employees and of
                                   female paid employees as a percentage of average gross hourly earnings of male paid employees. 
                                   The indicator has been defined as unadjusted, because it gives an overall picture of gender inequalities in terms of pay and measures a concept which is broader than the concept of equal pay for equal work."),
                                 br(),
                                 p("The plot below shows the the gender pay gap against the share of female researchers across sectors."),
                                 plotOutput("pay"),
                                 br(),
                                 p("The relationship between the gender pay gap and share of female researchers is generaly weak and depends heavily
                                   on the sector in question. Across all sectors, there is a very weak positive relationship between
                                   country's gender pay gap and the share of of female researchers.")
                             )     
                         ))
            )
        )
)
            

server <- function(input, output, session) {
    output$femres <- renderPlot({
        rd_p_femres %>% filter(sectperf == input$sector & unit == input$unit & geo == "Czechia") %>%        
        ggplot(aes(x = time, y = values, group = geo, label = values)) +
        geom_line() +
        geom_point() +
        geom_text(aes(y = values + 3, scale = 1)) +
        labs(title = "Share of female researchers by sector of performance",
             x = "Time",
             y = input$unit,
             caption = "Data source: Eurostat") +
        scale_y_continuous(limits = c(0, 100)) +
        theme_classic()
    })
    
    output$pay <- renderPlot({
        paygap %>% 
            filter(sectperf == input$sector2) %>% 
            ggplot(aes(x = female_share, y = pay_gap, label = geo)) +
            geom_point() +
            geom_text_repel() +
            geom_smooth(method = "lm", se = F) +
            labs(title = "Share of female researchers vs country's gender pay gap",
                 x = "Share of female researchers",
                 y = "Unadjusted gender pay gap",
                 caption = "Data source> Eurostat, data come from 2006") +
            theme_classic()
            
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
