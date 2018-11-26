#' Select assessment
#'
#' Initializes the select assessment shiny tab
#'
#'
#' @author Nathan Vaughan
#' @keywords interface management
initSelectAssessment<-function(){
  tabPanel("Select Assessment",

           tags$div(id="selectAssessment",

           # Application title
           titlePanel(h1("Stock Assessment Decision Support Tool")),
           titlePanel(h4("Beta Version 0.4")),
           fluidRow(column(8,strong("Welcome to the NOAA stock assessment decision support tool (DST)."))),
           fluidRow(column(8,strong("The DST is designed to enable real-time update of fishery quota projections
                             in response to proposed management changes."))),
           fluidRow(column(8,strong("This software is still in BETA production
                             with new features being added and bugs being discovered. Please contact
                             nathan.vaughan@noaa.gov with any questions, feature requests, and bug reports."))),
           bsModal(id="welcomeMessage",title="Welcome to the Stock Synthesis Decision Support Tool (Beta Version 0.4)",trigger="closeWelcome",size="large",
                   tags$div(img(src="Welcome.png",height=141,width=514,style="margin:-15px -15px -0px -15px"))),
           bsModal(id="loadingAssessment",title="Loading base assessment data now",trigger="closeWelcome",size="large",
                   tags$div(img(src=paste0(getwd(),"/www/Welcome.png"),height=141,width=514,style="margin:-15px -15px -0px -15px"))),

           # Choose the Assessment to investigate this will determine the source
           # directory for stock synthesis assessment files
           fluidRow(
             column(4,selectInput("chooseAssessment", label = h3("Assessment"),
                                   choices = list("Collecting assessment list" = "BL"))),
             column(2),
             column(2,hidden(textInput("readTestOut",label="checker output",value=""))),
             column(4,hidden(textInput("readTest",label="input checker",value="")))),
           fluidRow(
             column(4,
                    hidden(actionButton("trig1",label="Initialize Interface")),
                    hidden(tags$div(id="trigB",strong("Almost ready, preparing for model initialization"))),
                    hidden(tags$div(id="trigA",strong("Loading and building assessment model")))),

             column(4,hidden(textInput("testCode",label="code to test",value=""))),
             column(4,hidden(actionBttn("redrawSelectPlots",label="draw selection plots")))),

           hidden(tags$div(id="UploadInterface",
                           fluidRow(
                             column(4,textInput(inputId="newAssessName",label="Name for new assessment",value="DefaultSpecies")),
                             column(4),
                             column(4)),
                           fluidRow(
                             column(4,h4("Required input files")),
                             column(4),
                             column(4)),
                           fluidRow(
                             column(4,fileInput(inputId = "readStarter",label="Select Starter File",placeholder = "No starter file selected",accept =c(".ss"))),
                             column(4,fileInput(inputId = "readData",label="Select Data File",placeholder = "No data file selected",accept=c(".dat"))),
                             column(4,fileInput(inputId = "readControl",label="Select Control File",placeholder = "No control file selected",accept=c(".ctl")))),
                           fluidRow(
                             column(4,h4("Optional input files")),
                             column(4),
                             column(4)),
                           fluidRow(
                             column(4,fileInput(inputId = "readForecast",label="Select Forecast File",placeholder = "No forecast file selected",accept =c(".ss"))),
                             column(4,fileInput(inputId = "readParameter",label="Select Parameter File",placeholder = "No parameter file selected",accept =c(".par"))),
                             column(4,fileInput(inputId = "readDisplay",label="Select Display File",placeholder = "No display file selected"))),
                           fluidRow(
                             column(4,actionButton(inputId = "createNewAssess",label="Build New Assessment")),
                             column(4),
                             column(4))))
        )
    )
}

#' Implement management
#'
#' Initializes the management settings shiny tab
#'
#'
#' @author Nathan Vaughan
#' @keywords interface management
initImplementManagement<-function(){
  tabPanel("Management Action",

           tags$div(id="ImplementManagement",
                    tags$div(id="ResetcurrSizeLimits",
                    fluidRow(id="resetValsRow",
                    column(id="resetValsRowCol1",width=3,
                    column(id="resetValsRowCol2",width=1),
                    column(id="resetValsRowCol3",width=11,actionButton(inputId="resetVals",label="Reset Selection Values"))),
                    column(id="resetValsRowCol4",width=3),
                    column(id="resetValsRowCol6",width=3,textInput(inputId = "runFolder",label="Forecast name",value="")),
                    column(id="resetValsRowCol5",width=3,actionButton(inputId="updateAssessment",label = "Update Forecast"))
                  )
                )
           )
  )
}

#' forecast
#'
#' Initializes the assessment results shiny tab
#'
#'
#' @author Nathan Vaughan
#' @keywords interface management
initQuotaForecast<-function(){
  tabPanel("Forecast Results",

           tags$div(id="QuotaForecast")

  )
}

#' Kobe Choices
#'
#' Initializes the kobe matrix assessment selections
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
initKobeChoices<-function(input, output, session){

  #get the current  working directory
  curr<-getwd()

  #Read all the directories in Assessments folder
  assessments<-c("","",list.dirs(paste(curr,"/Assessments",sep=""),recursive = FALSE))

  #Get the folder name (this is the species)
  split<-strsplit(assessments,"/Assessments/")
  for(i in 1:(length(assessments)))
  {
    split[[i]]<-split[[i]][2]
  }

  #Get the full folder path (this will be used as a base directory)
  split2<-strsplit(assessments,"djsfhfsdu")

  split[[1]]<-"Please select an assessment"
  split2[[1]]<-"DNU"
  split[[2]]<-"Upload new assessment"
  split2[[2]]<-"UN"

  names(split2)<-split
  split2$`Upload new assessment`<-NULL
  split2$`Please select an assessment`<-NULL
  split2$Tutorial<-NULL

  updateSelectInput(session,"CompAssessments",choices=split2)
}

#' Kobe Choices
#'
#' Initializes the assessment selections
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
initAssessments<-function(input, output, session){

  #get the current  working directory
  curr<-getwd()

  #Read all the directories in Assessments folder
  assessments<-c("","",list.dirs(paste(curr,"/Assessments",sep=""),recursive = FALSE))

  #Get the folder name (this is the species)
  split<-strsplit(assessments,"/Assessments/")
  for(i in 1:(length(assessments)))
  {
    split[[i]]<-split[[i]][2]
  }

  #Get the full folder path (this will be used as a base directory)
  split2<-strsplit(assessments,"djsfhfsdu")

  split[[1]]<-"Please select an assessment"
  split2[[1]]<-"DNU"
  split[[2]]<-"Upload new assessment"
  split2[[2]]<-"UN"
  names(split2)<-split

  updateSelectInput(session,"chooseAssessment",choices=split2)
}
