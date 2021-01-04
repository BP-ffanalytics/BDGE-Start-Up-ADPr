library(shiny)
library(plotly)
library(DT)

source("global.r")

shinyServer(function(input, output, session) {


	MetaDF <- reactive({

		# Subset based on BDGE league
		if(input$BDGEdat == 1) {
			MetaBDGE <- droplevels(ADP_metadata_DF[grepl("BDGE", ADP_metadata_DF$Mock),])
		} else {
			MetaBDGE <- ADP_metadata_DF
		}
		# Subset if rookie draft
		if(input$Rookiedraft == 1) {
			MetaRooks2 <- droplevels(MetaBDGE[MetaBDGE$League %in% "rookie",])			
			MetaDate <- droplevels(MetaRooks2[MetaRooks2$Date >= input$date_range[1] & MetaRooks2$Date <= input$date_range[2], ])
			MetaRooks2 <- MetaDate
						
			# need to fix if rookie draft is set to yes and no rooks is set to yes
		} else {	
			# if no rookie draft, check to see if rookies are included in mocks
			MetaRooks <- droplevels(MetaBDGE[!(MetaBDGE$Rookie == "rookie"),])
			if(input$rooks == 1) {
				# If yes, then no change to data
				MetaRooks2 <- MetaRooks
			} else {
				# If no rookies, then only keep data with rookies
				MetaRooks2 <- droplevels(MetaRooks[MetaRooks$Rookie %in% "No Rookie",])
				if(nrow(MetaRooks2) == 0) MetaRooks2 <- MetaRooks
			}
			
		}
		MetaDate <- droplevels(MetaRooks2[MetaRooks2$Date >= input$date_range[1] & MetaRooks2$Date <= input$date_range[2], ])
		MetaEvent <- droplevels(MetaDate[MetaDate$Event %in% input$event,])
		MetaLeag <- droplevels(MetaEvent[MetaEvent$League %in% input$leaguetype,])
		MetaPart <- droplevels(MetaLeag[MetaLeag$TotalParticipants >= input$MinPartic,])
		MetaDF <- MetaPart
		MetaDF
			
	})

	output$TotalRoundsRender <- renderUI({ 
		MetaDF <- MetaDF()		
		MetaDF <- MetaDF()		
		if(input$Rookiedraft == 1) {
		  list(
				
			  sliderInput(inputId = "MaxRound", 
						  label = "Maximum Rounds", 
						  min = min(MetaDF$TotalRounds), 
						  max = max(MetaDF$TotalRounds), 
						  value = c(min(MetaDF$TotalRounds), max(MetaDF$TotalRounds)),
						  step=1),
						  
			  sliderInput(inputId = "MinNumb", 
						  label = "Min/Max in Draft", 
						  min = 1, 
						  max = nrow(MetaDF), 
						  value = c(2, nrow(MetaDF)),
						  step=1
			  )
		  )
		} else {
		  list(
			  sliderInput(inputId = "MaxRound", 
						  label = "Maximum Rounds", 
						  min = min(MetaDF$TotalRounds), 
						  max = max(MetaDF$TotalRounds), 
						  value = c(min(MetaDF$TotalRounds), max(MetaDF$TotalRounds)),
						  step=1),
						  
			  sliderInput(inputId = "MinNumb", 
						  label = "Min/Max in Draft", 
						  min = 1, 
						  max = nrow(MetaDF), 
						  value = c(20, nrow(MetaDF)),
						  step=1
			  )
			)  
		}
		
	})
	
	PlayerPosFINAL <- reactive({		
		req(MetaDF())
		MetaDF <- MetaDF()
		MinNumb <- input$MinNumb
		MinNumb
		
		## Identify players with min and max draft inclusion

		PlayerPos_meta <- PlayerPos_DF[,colnames(PlayerPos_DF) %in% c("playerID","team","position","first_name","last_name","player",MetaDF$MockID)]
		PlayerPos_nometadat <- PlayerPos_meta[, !(colnames(PlayerPos_meta) %in% c("playerID","team","position","first_name","last_name","player"))]
		rownames(PlayerPos_nometadat) <- PlayerPos_meta$player
		# Sum actual picks (ie., non NA)
		NonNAcount <- sapply(1:(nrow(PlayerPos_nometadat)), function(x) {
			player <- unlist(PlayerPos_nometadat[x,1:(ncol(PlayerPos_nometadat))])
			NonNA <- ncol(PlayerPos_nometadat) - sum(player %in% NA)
			names(NonNA) <- rownames(PlayerPos_nometadat)[x]
			NonNA
		})
		
		PlayerPos_meta_wnNA <- full_join(PlayerPos_meta, data.frame(nonNA=NonNAcount, player=names(NonNAcount)), by="player")
		PlayerPos_metaMIN <- PlayerPos_meta_wnNA[PlayerPos_meta_wnNA$nonNA >= MinNumb[1],]
		PlayerPosFINAL <- PlayerPos_metaMIN[PlayerPos_metaMIN$nonNA <= MinNumb[2],!(colnames(PlayerPos_metaMIN) %in% c("playerID","team","position","first_name","last_name","nonNA"))]
		PlayerPosFINAL
	})

	Players <- reactive({
		req(PlayerPosFINAL())
		PlayerPosFINAL <- PlayerPosFINAL()
		## Extract players
		PlayerPosFINAL$player		
	})

	PlayerPos <- reactive({
		req(PlayerPosFINAL()) 
		PlayerPosFINAL <- PlayerPosFINAL()
		MetaDF <- MetaDF()
		PlayerPosFINAL[,!(colnames(PlayerPosFINAL) %in% c("playerID","team","position","first_name","last_name"))]
	})
	
	DraftStats <- reactive({
		req(PlayerPosFINAL())
		PlayerPosFINAL <- PlayerPosFINAL()
		DraftStats <- PlayerPosFINAL %>% 
			gather(Drafts, Position, -player) %>%
			group_by(player) %>%
			summarize(
				n = length(Position[!(Position %in% NA)]),
				ADP=mean(Position, na.rm=TRUE), 
				MDP=median(Position, na.rm=TRUE), 
				MIN=min(Position, na.rm=TRUE),
				MAX=max(Position, na.rm=TRUE),
				SD=sd(Position, na.rm=TRUE)
			) %>%
			arrange(desc(ADP)) %>% 
			mutate(
				error=qnorm(0.975)*SD/sqrt(n),
				Uconf = ifelse(n > 1, ADP + error, ADP),
				Lconf = ifelse(n > 1, ADP - error, ADP)
			) %>%
			mutate(
				Lconf = ifelse(Lconf < 0, 0, Lconf)
			) %>% as.data.frame()
			
		DraftStats$player <- factor(as.character(DraftStats$player), levels=as.character(DraftStats$player))
		DraftStats[order(DraftStats$player),]
		
		
	})

	AllADPdata <- reactive({
		req(DraftStats())
		DraftStats <- DraftStats()
		PlayerPos <- PlayerPos()
		DraftStats$Rank <- rank(DraftStats$ADP)
	
		DPmelt <- PlayerPos %>% reshape2::melt()
		DPmelt$player <- factor(DPmelt$player, levels=levels(DraftStats$player))
		DPmelt$label <- NA
		
		if(input$DPtype == "adp") {
			DPmelt2 <- do.call(rbind, lapply(levels(DPmelt$player), function(x) {
				DAT <- droplevels(DPmelt[DPmelt$player %in% x,])
				ADP <- mean(DAT$value, na.rm=TRUE)
				DAT$label <- paste0(DAT$player, ": ADP=", as.character(round(ADP, 1)), "; n=",length(DAT$value[!(DAT$value %in% NA)]))
				DAT
			}))
		} else {
			DPmelt2 <- do.call(rbind, lapply(levels(DPmelt$player), function(x) {
				DAT <- droplevels(DPmelt[DPmelt$player %in% x,])
				MDP <- median(DAT$value, na.rm=TRUE)
				DAT$label <- paste0(DAT$player, ": MDP=", as.character(round(MDP, 1)), "; n=",length(DAT$value[!(DAT$value %in% NA)]))
				DAT
			}))
		}
		DPmelt2$label <- factor(DPmelt2$label, levels=unique(DPmelt2[order(DPmelt2$player),"label"]))
		
		list(DS=DraftStats, DPmelt2=DPmelt2)
	})


	output$ADPplotly <- renderPlotly({
		req(AllADPdata())
		DS <- AllADPdata()$DS
		DPmelt2 <- AllADPdata()$DPmelt2
		DSdisplay <- droplevels(DS[DS$Rank %in% seq(1, input$numdisplay, 0.5),])
		if(input$DPposition == "all") {
			DSposition <- DSdisplay
		} else {
			if(input$DPposition == "onlyrooks") {
				DSposition <- DSdisplay[DSdisplay$player %in% PlayerMetadata[PlayerMetadata$DraftYear == 2020,"Player"],]
			} else {
				DSposition <- DSdisplay[DSdisplay$player %in% PlayerMetadata[PlayerMetadata$Position %in% input$DPposition,"Player"],]
			}
		}
		
		if(input$ADPplot == "dot"){
			p <- plot_ly(DSposition, 
						x = ~ADP, 
						y = ~player, 
						type = 'scatter',
						mode = "markers", 
						marker = list(color = "black",
									  size = 10
						)
			)
			# initiate a line shape object
			line <- list(
			  type = "line",
			  line = list(color = "black"),
			  xref = "x",
			  yref = "y"
			)
			lines <- list()
			for(i in 1:nrow(DSposition)){
			line[["x0"]] <- DSposition$Lconf[i]
			  line[["x1"]] <- DSposition$Uconf[i]
			  line[c("y0", "y1")] <- as.character(DSposition$player)[i]
			  lines <- c(lines, list(line))
			}
			p %>% layout(shapes = lines,
				xaxis = list(title = "Mock Start Up Draft Pick"),
				yaxis = list(title = ""),
				margin = list(l = 100)) 
		} else {
			DPmelt3 <- droplevels(DPmelt2[as.character(DPmelt2$player) %in% as.character(DSposition$player),])
			head(DPmelt3)
			p <- plot_ly(type = "box")
			for(i in 1:nlevels(DPmelt3$label)) {
				DPplayer <- droplevels(DPmelt3[DPmelt3$label %in% levels(DPmelt3$label)[i],])
				p <- p %>% add_boxplot(x = as.numeric(DPplayer$value), 
									   name = levels(DPmelt3$label)[i], 
									   boxpoints = 'outliers',
									   marker = list(color = 'black'),
											line = list(color = 'black'))
			}
			p %>% layout(showlegend = FALSE)

		}
	})
		
	 # render the table (with row names)
	output$ADPtable <- DT::renderDataTable({
		req(AllADPdata())
		DS <- AllADPdata()$DS
		DS <- DS[,c("player", "n", "ADP", "MDP", "MIN", "MAX")]
		DS$player <- factor(as.character(DS$player), levels=rev(as.character(DS$player)))
		DS$ADP <- round(DS$ADP, 2)
		colnames(DS) <- c("player", "n", "ADP", "MDP", "Min", "Max")
		datatable(DS, extensions='Buttons', rownames = FALSE,
					options = list(
						dom = 'lf<"floatright"B>rtip',
						buttons = c('excel', 'pdf', 'csv'),
							searching = TRUE,
							pageLength = 10,
							lengthMenu = c(10, 25, 50, 100, 250)
							)
		)
	})	
	
	
	output$MainGraphicArea <- renderUI({ 
		PlayerPosFINAL <- PlayerPosFINAL()
		PlayerList <- sort(PlayerPosFINAL$player)
		if(input$datatype == 1) {
		  list(
			div(class = "col-sm-8 col-md-9",
			# verbatimTextOutput("testTEXT"),
				div(class = "shot-chart-container",
				  div(class = "shot-chart-header",
					
					fixedRow(
						column(3, 
							selectInput(inputId = "ADPplot",
								  label = "Select Plot",
								  choices = c("Boxplots" = "box", "Dot Chart" = "dot"),
								  selected = "box",
								  selectize = FALSE)
						),
						column(3,
						
							numericInput(inputId = "numdisplay", 
									  label = "Displayed Players", 
									  value = 50)
						),
						column(3, 
							selectInput(inputId = "DPtype",
								  label = "Draft Pick",
								  choices = c("Average (ADP)" = "adp", "Median (MDP)" = "mdp"),
								  selected = "adp",
								  selectize = FALSE)
						),
						column(3, 
							selectInput(inputId = "DPposition",
								  label = "Player Position",
								  choices = c("All Positions" = "all", "Quarterbacks" = "QB",
									"Running Backs" = "RB", "Wide Receivers" = "WR", 
									"Tight Ends" = "TE", "Rookies Only" = "onlyrooks"),
								  selected = "adp",
								  selectize = FALSE)
						)
					)
					
				  ),

				  plotlyOutput("ADPplotly", width = 800, height = 800),
				  DT::dataTableOutput('ADPtable')
				  
			  )
		    )
		  )
		} else {
			list(
		# verbatimTextOutput("testTEXT"),
				div(class = "col-sm-8 col-md-9",
					div(class = "shot-chart-container",
					  div(class = "shot-chart-header",
						fixedRow(
							column(3,
								selectInput(inputId = "player_name",
								  label = "Player",
								  choices = PlayerList,
								  selected = PlayerList[grepl("Mahomes", PlayerList)],
								  selectize = FALSE)
							),
							column(6, 
								h2(textOutput("chart_header_player")),
								h3(textOutput("chart_header_team")),
								h4(textOutput("chart_header_draft"))
							),
							column(3,
								uiOutput(outputId = "teamlogo")
							)
						)
					  ),
					  br(),
					  br(),
						fixedRow(
							column(10,
		
								plotlyOutput("playerTIME", width = 850, height = "auto")	
							)
						),
						br(),
						br(),
						fixedRow(
							column(2,
								plotlyOutput("playerBP", width = 300, height = 400)	
							),
							column(2),
							column(3,
								plotlyOutput("playerBARS", width = 550, height = 400)	
							)
						)
					)
				)
			)
		}
		})			

	output$chart_header_player <- renderText({	
		player_name <- input$player_name
		player_name
	  })

	output$chart_header_team = renderText({	
		player_name <- input$player_name
		play <- PlayerMetadata[PlayerMetadata$Player %in% player_name,]
		if(play$Date == as.Date("2020-01-01")){
			DOB <- "Unknown"
		} else {
			DOB <- round(play[,"CurrentAge"],1)
		}
		if(play$CurrentTeamAbbr %in% NA) {
			team <- "Rookie"
		} else {
			if(play$CurrentTeamAbbr == "FA") {
				team <- "Free Agent"
			} else {
				team <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"team"]
			}
		}
		paste0(team,	", ", play[,"Position"], "; Age: ", DOB)
	})

	output$chart_header_draft = renderText({	
		player_name <- input$player_name
		if(player_name %in% PlayerMetadata$Player){
			play <- PlayerMetadata[PlayerMetadata$Player %in% player_name,]
			paste0(
				play[,"DraftYear"],	" Draft, Round ", play[,"Rnd"],
				"; Pick: ",
				play[,"Pick"]
			)
		} else {
			NULL
		}
	})
  
	# output$testTEXT <- renderPrint({	
	
	# })	
	
	output$teamlogo <- renderUI({	
		player_name <- input$player_name
		if(player_name %in% PlayerMetadata$Player){
			team <- PlayerMetadata[PlayerMetadata$Player %in% player_name,"CurrentTeamAbbr"]
			urllogo <- logos[logos$team_code %in% team,"url"]
			list(tags$img(src = urllogo))
		} else {
			NULL
		}
		
	})
 
	playerMOCKdata <- reactive({ 
		req(PlayerPosFINAL())
		PlayerPosFINAL <- PlayerPosFINAL()
		MetaDF <- MetaDF()
		player_name <- input$player_name	
		PlayerDFwide <- PlayerPosFINAL[PlayerPosFINAL$player %in% player_name,]
		rownames(PlayerDFwide) <- PlayerDFwide$player
		PlayerDFwide <- PlayerDFwide[,!(colnames(PlayerDFwide) %in% "player")]
		PlayerDFtall <- data.frame(MockID=colnames(PlayerDFwide), Pick = unlist(PlayerDFwide))
		MetaDF$MockID <- as.character(MetaDF$MockID)
		MockDF <- inner_join(MetaDF, PlayerDFtall, by="MockID")
		
		MockDF
	})	

	output$testTEXT <- renderPrint({	
		req(playerMOCKdata())
		playerMOCKdata <- playerMOCKdata()
		player_name <- input$player_name	
		playerMOCKdata <- playerMOCKdata[!(playerMOCKdata$Pick %in% NA),]
		playerMOCKdata$Position <- 1:nrow(playerMOCKdata)
		playerMOCKdata$ADP <- mean(playerMOCKdata$Pick)
		playerMOCKdata$MDP <- median(playerMOCKdata$Pick)
		if(player_name %in% PlayerMetadata$Player){
			play <- PlayerMetadata[PlayerMetadata$Player %in% player_name,]
			teamPcolor <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"primary"]
			teamScolor <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"secondary"]
		} else {
			
			teamPcolor <- "#E03A3E"
			teamScolor <- "#000000"
		}
		# list(teamPcolor,teamScolor)
		list(player_name,PlayerMetadata$player, head(PlayerMetadata))
	
	})	
	 
	output$playerTIME <- renderPlotly({
		req(playerMOCKdata())
		playerMOCKdata <- playerMOCKdata()
		player_name <- input$player_name	
		playerMOCKdata <- playerMOCKdata[!(playerMOCKdata$Pick %in% NA),]
		playerMOCKdata$Position <- 1:nrow(playerMOCKdata)
		playerMOCKdata$ADP <- mean(playerMOCKdata$Pick)
		playerMOCKdata$MDP <- median(playerMOCKdata$Pick)
		if(player_name %in% PlayerMetadata$Player){
			play <- PlayerMetadata[PlayerMetadata$Player %in% player_name,]
			teamPcolor <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"primary"]
			teamScolor <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"secondary"]
		} else {
			
			teamPcolor <- "#E03A3E"
			teamScolor <- "#000000"
		}
		fig <- plot_ly(playerMOCKdata, x = ~Position, name="")
		fig %>% add_trace(y = ~Pick, name = 'Pick', mode = 'lines+markers',
						  marker = list(size = 10,color = teamPcolor),
					      line = list(color=teamScolor)
				) %>% 
				add_trace(y = ~ADP, name = 'Average Draft Position', mode = 'lines', line = list(color = teamPcolor, width = 2, dash = 'dash')) %>% 
				add_trace(y = ~MDP, name = 'Median Draft Position', mode = 'lines', line = list(color = teamPcolor, width = 2, dash = 'dot')) %>%
				layout(xaxis = list(title = 'Mock Draft (Ordered by Date)'))
	
	})
	
	output$playerBP <- renderPlotly({
		req(playerMOCKdata())
		playerMOCKdata <- playerMOCKdata()
		player_name <- input$player_name	
		playerMOCKdata <- playerMOCKdata[!(playerMOCKdata$Pick %in% NA),]
		playerMOCKdata$Position <- 1:nrow(playerMOCKdata)
		playerMOCKdata$ADP <- mean(playerMOCKdata$Pick)
		playerMOCKdata$MDP <- median(playerMOCKdata$Pick)
		if(player_name %in% PlayerMetadata$Player){
			play <- PlayerMetadata[PlayerMetadata$Player %in% player_name,]
			teamPcolor <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"primary"]
			teamScolor <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"secondary"]
		} else {
			
			teamPcolor <- "#E03A3E"
			teamScolor <- "#000000"
		}
		fig <- plot_ly(playerMOCKdata, type = 'box')
		fig <- fig %>% add_boxplot(y = ~Pick, jitter = 0.3, pointpos = -1.8, boxpoints = 'all',
              marker = list(color = teamPcolor),
              line = list(color = teamScolor),
              name = "Picks")%>% layout(showlegend = FALSE)
	
	})
	
	output$playerBARS <- renderPlotly({
		req(DraftStats())
		DraftStats <- DraftStats()
		player_name <- input$player_name	
		DS <- DraftStats[DraftStats$player %in% player_name,c("n","ADP", "MDP", "MIN", "MAX", "SD")]
		DStall <- DS %>% gather(Stats, value, -SD)
		DStall$SD <- ifelse(grepl("ADP", DStall$Stats), DStall$SD, NA)
		DStall$Stats <- factor(DStall$Stats, levels=c("MIN","ADP", "MAX", "n", "MDP", "SD"))
		if(player_name %in% PlayerMetadata$Player){
			play <- PlayerMetadata[PlayerMetadata$Player %in% player_name,]
			teamPcolor <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"primary"]
			teamScolor <- nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"secondary"]
		} else {
			
			teamPcolor <- "#E03A3E"
			teamScolor <- "#000000"
		}
		
		
		fig <- plot_ly(data = DStall[DStall$Stats %in% "ADP",], x = ~Stats, y = ~value, type = 'bar', name="ADP",
					mode = 'markers',marker = list(color = teamPcolor),error_y = ~list(array = SD,color = teamScolor),
					text = ~value, textposition = 'auto') %>% 
				layout(xaxis = list(title = "")) %>% 
				add_trace(data = DStall[DStall$Stats %in% "MIN",], x = ~Stats, y = ~value, type = 'bar', name="Min",
					mode = 'markers',marker = list(color = teamPcolor),text = ~value, textposition = 'auto')%>% 
				add_trace(data = DStall[DStall$Stats %in% "MAX",], x = ~Stats, y = ~value, type = 'bar', name="Max",
					mode = 'markers',marker = list(color = teamPcolor),text = ~value, textposition = 'auto')
	})
		
})
