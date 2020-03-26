library(shiny)
library(plotly)
library(DT)

source("global.r")

shinyServer(function(input, output, session) {

	
	MetaDF <- reactive({
		if(input$BDGEdat == 1) {
			MetaBDGE <- droplevels(ADP_metadata_DF[grepl("BDGE", ADP_metadata_DF$Mock),])
		} else {
			MetaBDGE <- ADP_metadata_DF
		}
		if(input$rooks == 1) {
			MetaRooks <- MetaBDGE
		} else {
			MetaRooks <- droplevels(MetaBDGE[MetaBDGE$Rookie == "No Rookies",])
		}
		
		MetaDate <- droplevels(MetaRooks[MetaRooks$Date >= input$date_range[1] & MetaRooks$Date <= input$date_range[2], ])
		MetaEvent <- droplevels(MetaDate[MetaDate$Event %in% input$event,])
		MetaLeag <- droplevels(MetaEvent[MetaEvent$LeagueType %in% input$leaguetype,])
		MetaLeag
	})


	MockDF <- reactive({
		req(MetaDF())
		MetaDF <- MetaDF()
		if(nrow(MetaDF) == 0) {
			BDGEmocks <- ADPdata_DF[,grepl("BDGE", colnames(ADPdata_DF))]
			ADPmetasub <- ADPdata_DF[,colnames(ADPdata_DF) %in% c("Round", colnames(BDGEmocks))]
			NAlogic <- apply(ADPmetasub[,-1, drop=FALSE], 2, function(x) x %in% NA)
			LastPick <- apply(NAlogic, 2, function(x) which(x==TRUE)[1])-1
			DraftnumDF <- data.frame(Round=ADPmetasub[,"Round"], Pick=1:nrow(ADPmetasub), stringsAsFactor=FALSE)
			LastRound <- as.numeric(as.character(sapply(LastPick, function(x) DraftnumDF[DraftnumDF$Pick == x, "Round"])))
			names(LastRound) <- colnames(NAlogic)
			DraftRound <- LastRound[LastRound >= input$MaxRound[1] & LastRound <= input$MaxRound[2]]
			MockDF <- ADPmetasub[,colnames(ADPmetasub) %in% names(DraftRound), drop=FALSE]

		} else {
			ADPmetasub <- ADPdata_DF[,colnames(ADPdata_DF) %in% c("Round", MetaDF$Mock)]
			NAlogic <- apply(ADPmetasub[,-1, drop=FALSE], 2, function(x) x %in% NA)
			LastPick <- apply(NAlogic, 2, function(x) which(x==TRUE)[1])-1
			DraftnumDF <- data.frame(Round=ADPmetasub[,"Round"], Pick=1:nrow(ADPmetasub), stringsAsFactor=FALSE)
			LastRound <- as.numeric(as.character(sapply(LastPick, function(x) DraftnumDF[DraftnumDF$Pick == x, "Round"])))
			names(LastRound) <- colnames(NAlogic)
			DraftRound <- LastRound[LastRound >= input$MaxRound[1] & LastRound <= input$MaxRound[2]]
			ADPmetasub[,colnames(ADPmetasub) %in% names(DraftRound), drop=FALSE]
		}
	})

	MockDF_FINAL <- reactive({
		req(MockDF())
		MockDF <- MockDF()
		if(ncol(MockDF) == 0) {
			BDGEmocks <- ADPdata_DF[,grepl("BDGE", colnames(ADPdata_DF))]
		} else {
			BDGEmocks <- MockDF
		}
		BDGEmocks
	})

	Players <- reactive({
		req(MockDF_FINAL())
		MockDF_FINAL <- MockDF_FINAL()
		## Extract players
		unique(as.character(unlist(MockDF_FINAL)))[!(unique(as.character(unlist(MockDF_FINAL))) %in% NA)]		
	})

	PlayerPos <- reactive({
		req(Players()) 
		MockDF_FINAL <- MockDF_FINAL()
		Players <- Players()
		PlayerPos <- do.call("rbind", lapply(Players, function(plys) {
			Ddf <- as.data.frame(lapply(MockDF_FINAL,  function(dft) {
				dpos <- which(dft %in% plys)
				if(length(dpos) == 0) dpos <- NA
				dpos
			}))
			Ddf$Player <- plys
			Ddf
		}))
		PlayerPos
	})

	PlayerPosFINAL <- reactive({
		req(PlayerPos())
		PlayerPos <- PlayerPos()
		MetaDF <- MetaDF()
		MinNumb <- input$MinNumb
		# apply(PlayerPos[,!(grepl("Player", colnames(PlayerPos)))], 1, function(x) (x %in% NA))
		NAcount <- apply(PlayerPos[,!(grepl("Player", colnames(PlayerPos)))], 1, function(x) nrow(MetaDF) - sum(x %in% NA))
		PlayerPos_minn <- PlayerPos[NAcount >= MinNumb[1] & NAcount <= MinNumb[2],]
		if(input$nolabrooks == 1){
			PlayerPos_rookLab <- PlayerPos_minn
		} else {
			PlayerPos_rookLab <- PlayerPos_minn[!(grepl("ROOKIE", PlayerPos_minn$Player)),]
		}
		PlayerPos_rookLab
	})
	
	DraftStats <- reactive({
		req(PlayerPosFINAL())
		
		PlayerPosFINAL <- PlayerPosFINAL()
		DraftStats <- PlayerPosFINAL %>% 
			gather(Drafts, Position, -Player) %>%
			group_by(Player) %>%
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
			) %>% as.data.frame()	
		DraftStats$Player <- factor(as.character(DraftStats$Player), levels=as.character(DraftStats$Player))
		DraftStats <- DraftStats[order(DraftStats$Player),]
		DraftStats
		
	})
	
	AllADPdata <- reactive({
		req(DraftStats())
		DraftStats <- DraftStats()
		PlayerPos <- PlayerPos()
		DraftStats$Rank <- rank(DraftStats$ADP)
		DS <- droplevels(DraftStats[DraftStats$Rank %in% seq(1, input$numdisplay, 0.5),])
		
		if(input$rooks == 1) {
			if(input$onlyrooks == 2){
				DS <- droplevels(DS[!(DS$Player %in% PlayerMetadata$Player),])
			} else {
				DS <- DS
			}
		} else {
			DS <- DS
		}
		
		DP <- droplevels(PlayerPos[PlayerPos$Player %in% levels(DS$Player),])
		DPmelt <- PlayerPos %>% reshape2::melt()
		DPmelt$Player <- factor(DPmelt$Player, levels=levels(DS$Player))
		DPmelt$label <- NA
		if(input$rooks == 1) {
			if(input$onlyrooks == 2){
				DPmelt <- droplevels(DPmelt[!(DPmelt$Player %in% PlayerMetadata$Player),])
			} else {
				DPmelt <- DPmelt
			}
		} else {
			DPmelt <- DPmelt
		}
		if(input$DPtype == "adp") {
			DPmelt2 <- do.call(rbind, lapply(levels(DPmelt$Player), function(x) {
				DAT <- droplevels(DPmelt[DPmelt$Player %in% x,])
				ADP <- mean(DAT$value, na.rm=TRUE)
				DAT$label <- paste0(DAT$Player, ": ADP=", as.character(round(ADP, 1)), "; n=",length(DAT$value[!(DAT$value %in% NA)]))
				DAT
			}))
		} else {
			DPmelt2 <- do.call(rbind, lapply(levels(DPmelt$Player), function(x) {
				DAT <- droplevels(DPmelt[DPmelt$Player %in% x,])
				MDP <- median(DAT$value, na.rm=TRUE)
				DAT$label <- paste0(DAT$Player, ": MDP=", as.character(round(MDP, 1)), "; n=",length(DAT$value[!(DAT$value %in% NA)]))
				DAT
			}))
		}
		DPmelt2$label <- factor(DPmelt2$label, levels=unique(DPmelt2[order(DPmelt2$Player),"label"]))
		list(DS=DS, DPmelt2=DPmelt2)

		
	})
	
	output$ADPplotly <- renderPlotly({
		req(AllADPdata())
		DS <- AllADPdata()$DS
		DPmelt2 <- AllADPdata()$DPmelt2
		
		if(input$ADPplot == "dot"){
			p <- plot_ly(DS, 
						x = ~ADP, 
						y = ~Player, 
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
			for(i in 1:nrow(DS)){
			line[["x0"]] <- DS$Lconf[i]
			  line[["x1"]] <- DS$Uconf[i]
			  line[c("y0", "y1")] <- as.character(DS$Player)[i]
			  lines <- c(lines, list(line))
			}
			p %>% layout(shapes = lines,
				xaxis = list(title = "Mock Start Up Draft Pick"),
				yaxis = list(title = ""),
				margin = list(l = 100)) 
		} else {
		
			p <- plot_ly(type = "box")
			for(i in 1:nlevels(DPmelt2$label)) {
				DPplayer <- droplevels(DPmelt2[DPmelt2$label %in% levels(DPmelt2$label)[i],])
				p <- p %>% add_boxplot(x = as.numeric(DPplayer$value), 
									   name = levels(DPmelt2$label)[i], 
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
		DS <- DS[,c("Player", "n", "ADP", "MDP", "MIN", "MAX")]
		DS$ADP <- round(DS$ADP, 2)
		colnames(DS) <- c("Player", "n", "ADP", "MDP", "Min", "Max")
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
	
	# output$testTEXT <- renderPrint({ 
		# req(AllADPdata())
		# DS <- AllADPdata()$DS
		# DS <- DS[,c("Player", "n", "ADP", "MDP", "MIN", "MAX")]
		# DS$ADP <- round(DS$ADP, 2)
		# colnames(DS) <- c("Player", "n", "ADP", "MDP", "Min", "Max")
		# head(DS)
	# })	

	
	output$MainGraphicArea <- renderUI({ 
		PlayerList <- unique(sort(as.matrix(ADPdata_DF[,!(colnames(ADPdata_DF) %in% c("Round", "RoundPick"))])))
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
								  value = 100)
					),
					column(3, 
						selectInput(inputId = "DPtype",
							  label = "Draft Position",
							  choices = c("Average (ADP)" = "adp", "Median (MDP)" = "mdp"),
							  selected = "adp",
							  selectize = FALSE)
					),
					column(3,
						radioButtons(inputId = "onlyrooks", 
							   label = "Only Display Rookies",
							   choices = list("No" = 1, "Yes" = 2), 
							   selected = 1)
					)
					)
					
				  ),

				  plotlyOutput("ADPplotly", width = 800, height = 2000),
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
		if(player_name %in% PlayerMetadata$Player){
			play <- PlayerMetadata[PlayerMetadata$Player %in% player_name,]
			paste0(
				nflteams[nflteams$abbr == play[,"CurrentTeamAbbr"],"team"],	", ", play[,"Position"],
				"; Age: ",
				round(play[,"CurrentAge"],1)
			)
		} else {
			"Rookie, data not available until Draft"
		}
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
		PlayerDFwide <- PlayerPosFINAL[PlayerPosFINAL$Player %in% player_name,]
		PlayerDFwide
		rownames(PlayerDFwide) <- PlayerDFwide$Player
		PlayerDFwide <- PlayerDFwide[,!(colnames(PlayerDFwide) %in% "Player")]
		PlayerDFtall <- data.frame(RMock=colnames(PlayerDFwide), Pick = unlist(PlayerDFwide))
		MetaDF$RMock <- make.names(MetaDF$Mock)
		MockDF <- inner_join(MetaDF, PlayerDFtall, by="RMock")
		
		MockDF
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
		DS <- DraftStats[DraftStats$Player %in% player_name,c("n","ADP", "MDP", "MIN", "MAX", "SD")]
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
