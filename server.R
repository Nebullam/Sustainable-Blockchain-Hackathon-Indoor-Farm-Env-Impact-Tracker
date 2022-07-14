## Load libraries
library(shinydashboard)
library(DT)
library(qrcode)
library(tidyverse)
library(lubridate)

## Load Pinata Code
source("ipfs.R")

## JWT Key Path
pinKeyPath <- "jwt_key.txt"

function(input, output, session)
{
	####################################################
	## CID from URL
	####################################################
	useNode <- reactiveValues(useNode = 1,ipfsip="http://127.0.0.1:5001",jwt="")
	cidData <- reactiveValues(appURL = NULL, cid = NULL, cidData = NULL)
	observeEvent(session$clientData$url_search,{
													app_url <- paste0(
																		session$clientData$url_protocol,
																		"//",
																		session$clientData$url_hostname,
																		":",
																		session$clientData$url_port,
																		session$clientData$url_pathname
																	)
													cidData$appURL <- app_url
													if(!is.null(parseQueryString(session$clientData$url_search)$cid)) cidData$cid <- parseQueryString(session$clientData$url_search)$cid
													if(!is.null(parseQueryString(session$clientData$url_search)$tab))  updateTabItems(session, "tabs", parseQueryString(session$clientData$url_search)$tab)
											})
	observeEvent(input$nodesel,{useNode$useNode <- input$nodesel})	
	####################################################
	####################################################


	####################################################
	## Confiugre Local Node
	####################################################
	output$localUI <- renderUI({
									if(input$nodesel==2) return(NULL)
									list(
											textAreaInput("locAdd", label = h4("Enter IPFS RPC API Address"), value = useNode$ipfsip,width='800px',height='200px'),
											actionButton("locAdd_set", label = " Set API Address",icon("key"),style="color: #fff; background-color: #419871; border-color: #419871")
										)
								})
	observeEvent(input$locAdd_set,{useNode$ipfsip <- input$locAdd})
	output$localUIS <- renderUI({
									if(input$nodesel==2) return(NULL)
									node_status <- read_local_status(useNode$ipfsip)
									node_color <- ifelse(node_status,"olive","red")
									node_text <- ifelse(node_status,"Connected","Error")
									node_icon <- ifelse(node_status,"thumbs-up","thumbs-down")
									renderInfoBox({infoBox("IPFS Daemon",node_text, icon = icon(node_icon, lib = "glyphicon"),color = node_color)})
								})
	####################################################
	####################################################



	####################################################
	## Confiugre Pinata Key
	####################################################
	output$pinKeyUI <- renderUI({
									if(input$nodesel==1) return(NULL)
									useNode$jwt <- ifelse(file.exists(pinKeyPath),readLines(pinKeyPath),"")
									list(
											textAreaInput("jwt", label = h4("Enter Pinata API JWT Key"), value = useNode$jwt,width='800px',height='200px'),
											actionButton("jwt_set", label = " Set JWT Key",icon("key"),style="color: #fff; background-color: #419871; border-color: #419871")
										)
								})
	observeEvent(input$jwt_set,{
									useNode$jwt <- input$jwt
									writeLines(input$jwt,pinKeyPath)
								})
	output$pinataUIS <- renderUI({
									if(input$nodesel==1) return(NULL)
									node_status <- read_pinata_status(useNode$jwt)
									node_color <- ifelse(node_status,"olive","red")
									node_text <- ifelse(node_status,"Connected","Error")
									node_icon <- ifelse(node_status,"thumbs-up","thumbs-down")
									renderInfoBox({infoBox("Pinata",node_text, icon = icon(node_icon, lib = "glyphicon"),color = node_color)})
								})
	####################################################
	####################################################


	####################################################
	## Upload Farm Data
	####################################################
	cids <- reactiveValues(cids = NULL)
	output$upFarmUI <- renderUI({
									if(input$tabs!="upload") return(NULL)
									if(useNode$useNode==1 & !read_local_status(useNode$ipfsip)) return(code("Can't Reach IPFS Daemon. Set it in Config Panel."))
									if(useNode$useNode==2 & !read_pinata_status(useNode$jwt)) return(code("Pinata JWT Key Not Set, Set it in Config Panel."))
									list(
											fileInput("farmFile", label = h4("Select Farm Data File (.csv)")),
											actionButton("up_farmFile", label = "Upload Farm Data",icon("cloud-upload-alt"),style="color: #fff; background-color: #419871; border-color: #419871")
										)
								})
	observeEvent(input$up_farmFile,{
										if(useNode$useNode==1) 
										{
											progress <- shiny::Progress$new()
											progress$set(message = "Pinning Data on IPFS using IPFS Daemon", value = 5)
											on.exit(progress$close())
											cids$cids <- post_local_pin(input$farmFile$datapath,useNode$ipfsip)
										}
										if(useNode$useNode==2) 
										{
											progress <- shiny::Progress$new()
											progress$set(message = "Pinning Data on IPFS using Pinata", value = 5)
											on.exit(progress$close())
											cids$cids <- post_pinata_pin(input$farmFile$datapath,useNode$jwt)
										}
								})
	output$dataCidUI <- renderUI({
									if(input$tabs!="upload") return(NULL)
									if(is.null(cids$cids)) return(NULL)
									currCID <- cids$cids
									currCIDurl <- a("Click Here ...", href=paste0(cidData$appURL,"?cid=",currCID,"&tab=analyse"))
									list(
											h4("Uploaded Data's CID"),
											code(currCID),
											hr(),
											h4("Uploaded Data's Visualisation Link"),
											tagList(currCIDurl),
											hr(),
											h4("Uploaded Data's Visualisation Link (QR)")
										)
								})
	output$dataCidQR <- renderPlot({
									if(input$tabs!="upload") return(NULL)
									if(is.null(cids$cids)) return(NULL)
									currCID <- cids$cids
									currCIDurl <- paste0(cidData$appURL,"?cid=",currCID,"&tab=analyse")
									plot(qr_code(currCIDurl))
								},bg="transparent",height=200)
	####################################################
	####################################################


	####################################################
	## Download Farm Data
	####################################################
	output$downFarmUI <- renderUI({
									if(input$tabs!="analyse") return(NULL)
									textInput("farmcid", label = NULL, value = cidData$cid ,placeholder = "Enter Farm Data IPFS CID Here ....",width='800px')
								})
	observeEvent(input$down_farmFile,{
										if(useNode$useNode==1)
										{
											progress <- shiny::Progress$new()
											progress$set(message = "Fetching Data from IPFS using Daemon", value = 5)
											on.exit(progress$close())
											cidData$cidData <- read_local_pin(input$farmcid,useNode$ipfsip)
										}
										if(useNode$useNode==2)
										{
											progress <- shiny::Progress$new()
											on.exit(progress$close())
											progress$set(message = "Fetching Data from IPFS", value = 0)
											nidx <- length(gateway_list)
											for(idx in 1:nidx)
											{
												progress$inc(1/nidx, detail = paste("Trying Gateway ",gateway_list[idx]))
												t_data <- read_pinata_pin(input$farmcid,gateway=gateway_list[idx])
												if("data.frame" %in% class(t_data))
												{
													cidData$cidData <- t_data
													break()
												}
											}	
										}
								})

	####################################################
	####################################################


	####################################################
	## Visualise Farm Data
	####################################################
	output$visFarmUI <- renderUI({
									if(is.null(cidData$cidData)) return(NULL)
									tabBox( width = "100%",
											tabPanel("Yield over Time", uiOutput("vis1UI")),
											tabPanel("Yield vs Water", uiOutput("vis2UI")),
											tabPanel("Yield vs Fertilizer", uiOutput("vis3UI")),
											tabPanel("Yield vs Electricity", uiOutput("vis4UI"))
										)
								})
	####################################################
	####################################################


	####################################################
	## Yield Over Time
	####################################################
	output$vis1UI <- renderUI({
								if(is.null(cidData$cidData)) return(NULL)
								tabBox( width = "100%",side="right",
										tabPanel("Macro View", plotOutput("vis1")),
										tabPanel("By Product Type", plotOutput("vis1a")),
										tabPanel("By Farm", plotOutput("vis1b"))
								)
							})
	output$vis1 <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    mutate(`Harvest Date` = mdy(`Harvest Date`)) %>%
								    select(`Harvest Date`, Yield = `Actual Yield (lbs.)`) %>%
								    ggplot(aes(x = `Harvest Date`, y = Yield)) +
								    geom_point() +
								    geom_line() +
								    geom_smooth(color = "#419871") +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_date(date_labels = "%b %y", date_breaks = "2 months") +
								    labs(
								        title = "Yield over Time"
								    )
						})
	output$vis1a <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    mutate(`Harvest Date` = mdy(`Harvest Date`)) %>%
								    select(`Harvest Date`, Yield = `Actual Yield (lbs.)`, `Product Type`) %>%
								    ggplot(aes(x = `Harvest Date`, y = Yield)) +
								    geom_line() +
								    geom_smooth(color = "#419871") +
								    facet_wrap(~`Product Type`, scales = "free_y") +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_date(date_labels = "%b %y", date_breaks = "4 months") +
								    labs(
								        title = "Yield over Time by Product Type"
								    ) +
								    theme(
								        axis.text.x = element_text(angle = 20, hjust = 1)
								    )
						})
	output$vis1b <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    mutate(`Harvest Date` = mdy(`Harvest Date`)) %>%
								    select(`Harvest Date`, Yield = `Actual Yield (lbs.)`, `Farm`) %>%
								    ggplot(aes(x = `Harvest Date`, y = Yield)) +
								    geom_line() +
								    geom_smooth(color = "#419871") +
								    facet_wrap(~`Farm`, scales = "free_y") +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_date(date_labels = "%b %y", date_breaks = "4 months") +
								    labs(
								        title = "Yield over Time by Farm"
								    ) +
								    theme(
								        axis.text.x = element_text(angle = 20, hjust = 1)
								    )
						})
	####################################################
	####################################################


	####################################################
	## Yield vs Water
	####################################################
	output$vis2UI <- renderUI({
								if(is.null(cidData$cidData)) return(NULL)
								tabBox( width = "100%",side="right",
										tabPanel("Macro View", plotOutput("vis2")),
										tabPanel("By Product Type", plotOutput("vis2a")),
										tabPanel("By Farm", plotOutput("vis2b"))
								)
							})
	output$vis2 <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    select(Yield = `Actual Yield (lbs.)`, Water = `Water Usage (Gals.)`) %>%
								    ggplot(aes(x = `Water`, y = Yield)) +
								    geom_point() +
								    geom_smooth(color = "#419871") +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    labs(
								        title = "Yield vs Water Usage"
								    )
						})
	output$vis2a <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    select(Yield = `Actual Yield (lbs.)`, Water = `Water Usage (Gals.)`, `Product Type`) %>%
								    ggplot(aes(x = `Water`, y = Yield, colour = `Product Type`)) +
								    geom_point() +
								    geom_smooth(color = "#419871") +
								    facet_wrap(~`Product Type`) +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    labs(
								        title = "Yield vs Water Usage by Product Type"
								    ) +
								    theme(legend.position = "off")
						})
	output$vis2b <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    select(Yield = `Actual Yield (lbs.)`, Water = `Water Usage (Gals.)`, `Farm`) %>%
								    ggplot(aes(x = `Water`, y = Yield, colour = `Farm`)) +
								    geom_point() +
								    geom_smooth(color = "#419871") +
								    facet_wrap(~`Farm`) +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    labs(
								        title = "Yield vs Water Usage by Farm"
								    ) +
								    theme(legend.position = "off")
						})
	####################################################
	####################################################


	####################################################
	## Yield vs Fertiliser
	####################################################
	output$vis3UI <- renderUI({
								if(is.null(cidData$cidData)) return(NULL)
								tabBox( width = "100%",side="right",
										tabPanel("Macro View", plotOutput("vis3")),
										tabPanel("By Product Type", plotOutput("vis3a")),
										tabPanel("By Farm", plotOutput("vis3b"))
								)
							})
	output$vis3 <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
							    select(Yield = `Actual Yield (lbs.)`, Fertilizer = `Fertilizer Usage (lbs)`) %>%
							    ggplot(aes(x = `Fertilizer`, y = Yield)) +
							    geom_point() +
							    geom_smooth(color = "#419871") +
							    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
							    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
							    labs(
							        title = "Yield vs Fertilizer Usage"
							    )
						})
	output$vis3a <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    select(Yield = `Actual Yield (lbs.)`, Fertilizer = `Fertilizer Usage (lbs)`, `Product Type`) %>%
								    ggplot(aes(x = `Fertilizer`, y = Yield, colour = `Product Type`)) +
								    geom_point() +
								    geom_smooth(color = "#419871") +
								    facet_wrap(~`Product Type`) +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    labs(
								        title = "Yield vs Fertilizer Usage by Product Type"
								    ) +
								    theme(legend.position = "off")
						})
	output$vis3b <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    select(Yield = `Actual Yield (lbs.)`, Fertilizer = `Fertilizer Usage (lbs)`, `Farm`) %>%
								    ggplot(aes(x = `Fertilizer`, y = Yield, colour = `Farm`)) +
								    geom_point() +
								    geom_smooth(color = "#419871") +
								    facet_wrap(~`Farm`) +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    labs(
								        title = "Yield vs Fertilizer Usage by Farm"
								    ) +
								    theme(legend.position = "off")
						})
	####################################################
	####################################################


	####################################################
	## Yield vs Electricity
	####################################################
	output$vis4UI <- renderUI({
								if(is.null(cidData$cidData)) return(NULL)
								tabBox( width = "100%",side="right",
										tabPanel("Macro View", plotOutput("vis4")),
										tabPanel("By Product Type", plotOutput("vis4a")),
										tabPanel("By Farm", plotOutput("vis4b"))
								)
							})
	output$vis4 <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    select(Yield = `Actual Yield (lbs.)`, Electricity = `Electricity Consumed (kwh)`) %>%
								    ggplot(aes(x = `Electricity`, y = Yield)) +
								    geom_point() +
								    geom_smooth(color = "#419871") +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    labs(
								        title = "Yield vs Electricity Usage"
								    )
						})
	output$vis4a <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    select(Yield = `Actual Yield (lbs.)`, Electricity = `Electricity Consumed (kwh)`, `Product Type`) %>%
								    ggplot(aes(x = `Electricity`, y = Yield, colour = `Product Type`)) +
								    geom_point() +
								    geom_smooth(color = "#419871") +
								    facet_wrap(~`Product Type`) +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    labs(
								        title = "Yield vs Electricity Usage by Product Type"
								    ) +
								    theme(legend.position = "off")
						})
	output$vis4b <- renderPlot({
								if(is.null(cidData$cidData)) return(NULL)
								prod <- cidData$cidData
								prod %>%
								    select(Yield = `Actual Yield (lbs.)`, Electricity = `Electricity Consumed (kwh)`, `Farm`) %>%
								    ggplot(aes(x = `Electricity`, y = Yield, colour = `Farm`)) +
								    geom_point() +
								    geom_smooth(color = "#419871") +
								    facet_wrap(~`Farm`) +
								    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
								    labs(
								        title = "Yield vs Electricity Usage by Farm"
								    ) +
								    theme(legend.position = "off")
						})
	####################################################
	####################################################

}