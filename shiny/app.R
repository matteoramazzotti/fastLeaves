library(ape)
library(shiny)

tree1<-read.tree("joined.uniq.tre")
tree2<-read.tree("joined.tre")
tree<-tree1
ui <- fluidPage(

	titlePanel("Covid19 Strains Interactive Tree Viewer"),

	sidebarLayout(
		sidebarPanel(width=2,
			selectInput(inputId="treefile","File:", choices = c("Full","Unique")),
			selectInput(inputId="treetype","Type:", choices = c("cladogram","radial","unrooted","fan")),
			numericInput(inputId="labsize","Label size:",1,step="0.1",width=100),
			numericInput(inputId="edgecol","Branch transparency %:",value=30,max=100,min=10,width=180),
			numericInput(inputId="wsize","Width:",value=1000,step=10,width=100),
			numericInput(inputId="hsize","Height:",value=2000,step=10,width=100),
			textAreaInput(inputId="deltips","Filter strains*", width = 150, height = 100),
			textAreaInput(inputId="colors","Color strains*", width = 150, height = 100),
			"*add search_keys one per line, prepend ! to exclude",
			"*format to use: search_key|color"
		),
		mainPanel(
			verbatimTextOutput("summary"),
			plotOutput("plot",width="auto",height="auto")
		)
	)
)

server <- function(input, output, session) {
	getTree <- reactive({
		if (input$treefile == "Unique") {
			tree<-read.tree("joined.uniq.tre")
		} else {
			tree<-read.tree("joined.tre")
		}
		tree	
	})
	dropTips <- reactive({
		tree<-getTree()
		lines<-unlist(strsplit(input$deltips,"\n"))
		if (length(lines) > 0) {
			todel<-NULL
			tokeep<-NULL
			for (i in lines) {
				if (grepl("^!",i)) {
					i<-gsub("^!","",i)
					#cat(i,file=stderr())
					todel<-unique(c(todel,grep(i,tree$tip.label,value=T)))
				} else {
					i<-gsub("^+","",i)
					tokeep<-unique(c(tokeep,grep(i,tree$tip.label,value=T)))
				}
			}
			sel<-setdiff(tokeep,todel)
			#cat(sel,file=stderr())
			tree<-keep.tip(tree,sel)
		}
		tree
	})
	getColors <- reactive({
		tree<-dropTips()
		colors<-rep("black",length(tree$tip.label))
		#cat(colors,file=stderr())
		lines<-unlist(strsplit(input$colors,"\n"))
		if (length(lines) > 0) {
			for (i in 1:length(lines)) {
				p<-unlist(strsplit(lines[i],"\\|"))
				#cat(paste0(p[1]," - ",p[2]),file=stderr())
				#cat(tree$tip.label,file=stderr())
				colors[grep(p[1],tree$tip.label)]<-p[2]
			}
		}
		#cat(colors,file=stderr())
		colors
	})
	getConfig<- reactive({
		c(input$treetype,input$labsize,input$wsize,input$hsize)
	})
	output$plot<-renderPlot({	
		plot(dropTips(),main="",type=input$treetype,edge.color=rgb(0,0,0,max=1,alpha=input$edgecol/100),tip.color=getColors(),cex=input$labsize,no.margin = TRUE)
	},
	width= reactive({input$wsize}),
	height= reactive({input$hsize}),
	)
}

shinyApp(ui, server)
