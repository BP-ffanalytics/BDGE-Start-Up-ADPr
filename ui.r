library(shiny)
library(dplyr)
library(jsonlite)
library(plotly)

source("global.r")


shinyUI(
  fixedPage(
    theme = "flatly.css",
    title = "BDGE Mock ADP",

    HTML('
      <nav class="navbar navbar-default navbar-static-top">
        <div class="container">
          <div>
            <ul class="nav navbar-nav col-xs-12">
              <li class="col-xs-8 col-md-9">
                <a href="#">BDGE Start Up ADPr<span class="hidden-xs">: Interactive Mock ADP</span></a>
              </li>
              <li class="col-xs-4 col-md-3 github-link">
                <a href="https://github.com/BP-ffanalytics/BDGE-Start-Up-ADPr" target="_blank">
                  <span class="hidden-xs">Code on </span>GitHub
                </a>
              </li>
            </ul>
          </div>
        </div>
      </nav>
    '),
	
    fixedRow(class = "primary-content",
	uiOutput("MainGraphicArea"),

      div(class = "col-sm-4 col-md-3",
        div(class = "shot-chart-inputs",
          # uiOutput("player_photo"),

          radioButtons(inputId = "datatype", 
					   label = "Data Type",
					   choices = list("Single Player" = 2, "All Data" = 1), 
					   selected = 1),
								
			
          radioButtons(inputId = "BDGEdat", 
					   label = "BDGE Mocks Only",
					   choices = list("Yes" = 1, "No" = 2), 
					   selected = 1),
					
			
          radioButtons(inputId = "Rookiedraft", 
					   label = "Rookie Draft",
					   choices = list("Yes" = 1, "No" = 2), 
					   selected = 2),
					
          radioButtons(inputId = "rooks", 
					   label = "Mocks with Rookies",
					   choices = list("Yes" = 1, "No" = 2), 
					   selected = 1),
					   
		  conditionalPanel(
			condition = "input.rooks == '1'",
				radioButtons(inputId = "nolabrooks", 
							 label = "Include non-player Rookie Picks",
							 choices = list("Yes" = 1, "No" = 2), 
							 selected = 2)
		  ),
					   
          dateRangeInput(inputId = "date_range",
                         label = "Date range",
                         start = ADP_metadata_DF$Date[1],
                         end = ADP_metadata_DF$Date[nrow(ADP_metadata_DF)]),

          checkboxGroupInput(inputId = "event",
							 choices = unique(ADP_metadata_DF$Event),
							 label = "NFL Event",
							 selected = unique(ADP_metadata_DF$Event)),
							 
          checkboxGroupInput(inputId = "leaguetype",
							 choices = unique(ADP_metadata_DF$League),
							 label = "League Type",
							 selected = unique(ADP_metadata_DF$League)),
							 
		  sliderInput(inputId = "MinPartic", 
					  label = "Minimum Participants", 
					  min = 7, 
					  max = 12, 
					  value = 7,
					  step=1),
     
		  uiOutput("TotalRoundsRender"),
    
		  
		 
						
        )
      )
    )
  )
)
