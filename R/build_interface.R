#' Select assessment
#'
#' Imports and organizes data for selected assessment
#' controls building the interface and setting default
#' values from original assessment.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
runSelectAssessment<-function(input, output, session){
  removeUI("#ManagementTabPanelDiv>*",multiple=TRUE,immediate=TRUE)
  removeUI("#ManagementTabPanelDiv",multiple=TRUE,immediate=TRUE)
  removeUI("#GlobalInputs>*",multiple=TRUE,immediate=TRUE)
  removeUI("#GlobalInputs",multiple=TRUE,immediate=TRUE)
  removeUI("#QuotaPanel>*",multiple=TRUE,immediate=TRUE)
  removeUI("#QuotaPanel",multiple=TRUE,immediate=TRUE)
  removeUI("#currOutput>*",multiple=TRUE,immediate=TRUE)
  removeUI("#currOutput",multiple=TRUE,immediate=TRUE)

  observerPool<<-as.list(rep(0,50))
  prevGroupAlloc<<-0
  temp.max.group<<-0
  temp.val.group<<-0
  temp.max.fleet<<-0
  temp.val.fleet<<-0
  temp.max.fleet.seas<<-0
  temp.val.fleet.seas<<-0
  temp.hide<<-0
  starter.msy<<-starter.run<<-starter.orig<<-0
  data.msy<<-data.run<<-data.orig<<-0
  forecast.msy<<-forecast.run<<-forecast.orig<<-0
  control.msy<<-control.run<<-control.orig<<-0
  output.msy<<-output.run<<-output.orig<<-0
  pars.msy<<-pars.run<<-pars.orig<<-0
  dir.msy<<-dir.orig<<-dir.run<<-dir.base<<-0
  age.selex<<-0
  size.selex<<-0
  size.reten<<-0
  size.mort<<-0
  comb.selex<<-0
  comb.dead<<-0
  comb.retain<<-0
  comb.Lc<<-0
  ALK<<-0
  size.50retain<<-0
  disp.Desc<<-0
  addedTabs<<-0
  filledTabs<<-0
  numCols<<-0
  rel_F_orig<<-0
  rel_C_orig<<-0
  rel_F_new<<-0
  rel_C_new<<-0
  prevGroupAlloc<<-0
  prevTotalAlloc<<-0
  sliderResolution<<-6
  baseCatch<<-0
  newCatch<<-0
  msyCatch<<-0
  weightNames<<-c("MT","1000's Lbs")
  weightScale<<-c(1,2.20462)
  lengthNames<<-c("cm","Inches")
  lengthScale<<-c(1,0.393701)
  groups<<-0
  areaNames<<-0
  seasonNames<<-0
  fleetNames<<-0
  groupNames<<-0
  numFutCatchYrs<<-3
  sleepTime<<-3
  managementLimits<<-0
  InflDisc.Val<<-0
  SlopeDisc.Val<<-0
  targetValue<<-0
  HasRun<<-rep(0,25)
  targetNames<<-c("SPR","Spawning Biomass Ratio","Maximize Catch","Use previous estimate")
  targetNames2<<-c("SPR Target Value","Ignore ","Spawning Biomass Ratio")
  targetNames3<<-c("Implement Optimal","Fixed Fraction Fmsy","Constant Catch")

  dir.base<<-input$chooseAssessment
  if(identical(dir.base,"DNU")){
    hideElement(id="UploadInterface")
  }else if(identical(dir.base,"UN")){
    showElement(id="UploadInterface")
  }else{
    withProgress(message="Loading base data now",value=0,{
      #Build an original directory if not yet existing.
      #This will retain the data as it was first input from the assessment.
      #This folder will be read as a reference without being edited.
      hideElement(id="UploadInterface")
      dir.orig<<-paste0(dir.base,"/original")
      if(!dir.exists(dir.orig))
      {
        incProgress(0.1, detail = "Building base directory and formating assessment")

        prepareAssessment(input, output, session, dir.base)
        files.base<-list.files(dir.base)
        dir.create(dir.orig)
        file.copy(paste0(dir.base,"/",files.base),dir.orig)
        unlink(paste0(dir.base,"/",files.base))

      }else{
        incProgress(0.1, detail = "Base directory present")}
      #Build a run directory.
      #This will retain the data used to run SS in the tool.
      #This folder is where all edits will be stored when testing management decisions.
      dir.run<<-paste0(dir.base,"/Run")
      if(!dir.exists(dir.run))
      {
        incProgress(0.1, detail = "Building base directory and formating assessment")
        dir.create(dir.run)
        files.orig<-list.files(dir.orig)
        file.copy(paste0(dir.orig,"/",files.orig),dir.run)
      }else{
        incProgress(0.1, detail = "Run directory present")}
      #else{
      #   files.run<-list.files(dir.run)
      #   unlink(paste0(dir.run,"/",files.run))
      #   files.orig<-list.files(dir.orig)
      #   file.copy(paste0(dir.orig,"/",files.orig),dir.run)
      # }

      #files.base<-list.files(dir.base)
      #unlink(dir.base,"/",files.base)

      #Read in all the SS values you need from assessment directory
      incProgress(0, detail = "Reading starter file")
      starter.run<<-starter.orig<<-rd_starter(paste0(dir.orig,"/starter.ss"))
      incProgress(0, detail = "Reading data file")
      data.run<<-data.orig<<-rd_data(file=paste0(dir.orig,"/",starter.orig$datfile))
      incProgress(0, detail = "Reading control file")
      control.run<<-control.orig<<-rd_ctl(file=paste(dir.orig,"/",starter.orig$ctlfile,sep=""),data.orig)
      incProgress(0, detail = "Reading forecast file")
      forecast.run<<-forecast.orig<<-rd_forecast(file=paste0(dir.orig,"/forecast.ss"),Nfleets=data.orig$Nfleet,Nareas = data.orig$N_areas,Nseas = data.orig$nseas)
      for(j in 1:6){
        if(forecast.run$Bmark_years[j]<=0){
          forecast.orig$Bmark_years[j]<<-forecast.run$Bmark_years[j]<<-data.run$endyr+forecast.run$Bmark_years[j]
        }
      }
      for(j in 1:4){
        if(forecast.run$Fcast_years[j]<=0){
          forecast.orig$Fcast_years[j]<<-forecast.run$Fcast_years[j]<<-data.run$endyr+forecast.run$Fcast_years[j]
        }
      }
      incProgress(0, detail = "Reading parameter file")
      pars.run<<-pars.orig<<-rd_par(file=paste(dir.orig,"/ss3.par",sep=""))
      disp.Desc<<-rd_display(file=paste(dir.orig,"/Display_Descriptions.txt",sep=""),forecast.orig,data.orig)
      numCols<<-200
      output.read<-FALSE
      output.temp<-NULL
      incProgress(0.1, detail = "Reading output file")
      while(output.read==FALSE){
        try({output.temp<-rd_output(dir.orig, covar=F, ncol=numCols)})
        if(is.null(output.temp)){
          output.read<-FALSE
          numCols<<-numCols+100
        }else{
          output.read<-TRUE
          output.run<<-output.orig<<-output.temp
        }
      }

      #ReadEffort()
      incProgress(0.1, detail="reading base catch")
      baseCatch<<-ReadCatch(output.orig, (data.orig$styr-1):(data.orig$endyr+forecast.orig$Nforecastyrs))
      projCatch<<-aggregate(baseCatch,list(year=baseCatch[,1],fleet=baseCatch[,4],season=baseCatch[,2]),sum)
      projCatch$Fleet<<-forecast.orig$fleet_assignment_to_allocation_group[projCatch$fleet]
      projCatch<<-projCatch[,c(1,2,3,7,8,9,10,11,13)]
      projCatch[,5:6]<<-projCatch[,5:6]*weightScale[disp.Desc$Units[1]]#2.20462
      incProgress(0.1, message = "Building management controls interface",detail="")

    })
  }

}

#' read management data
#'
#' read management data values from assessment to populate
#' the default values in the shiny interface
#' these values identify the future allocation, selectivity,
#' and management target values for the fishery.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface building
readManagementData<-function(input,output,session){
  incProgress(0.1, detail="Getting started determining base selection patterns")
  weightNames<<-c("MT","1000's Lbs")
  weightScale<<-c(1,2.20462)
  lengthNames<<-c("cm","Inches")
  lengthScale<<-c(1,0.393701)
  numFutCatchYrs<<-3
  temp.max.group<<-rep(0,length(unique(forecast.orig$fleet_assignment_to_allocation_group[forecast.orig$fleet_assignment_to_allocation_group!=0])))
  temp.val.group<<-rep(0,length(unique(forecast.orig$fleet_assignment_to_allocation_group)))
  temp.max.fleet<<-rep(0,data.orig$Nfleet)
  temp.val.fleet<<-rep(0,data.orig$Nfleet)
  temp.max.fleet.seas<<-matrix(0,nrow=data.orig$nseas,ncol=data.orig$Nfleet)
  temp.val.fleet.seas<<-matrix(0,nrow=data.orig$nseas,ncol=data.orig$Nfleet)
  age.selex<<-output.orig$ageselex[output.orig$ageselex$factor=="Asel",]
  age.selex<<-age.selex[age.selex$year==max(age.selex$year),]
  age.selex<<-age.selex[age.selex$seas==1,]
  age.selex<<-age.selex[age.selex$gender==1,]
  age.selex<<-age.selex[age.selex$morph==1,c(2,3,8:length(age.selex[1,]))]
  size.selex<<-output.orig$sizeselex[output.orig$sizeselex$gender==1,]
  size.reten<<-size.selex[size.selex$Factor=="Ret",c(2,3,6:length(size.selex[1,]))]
  size.reten<<-size.reten[size.reten$year==max(size.reten$year),]
  size.mort<<-size.selex[size.selex$Factor=="Mort",c(2,3,6:length(size.selex[1,]))]
  size.mort<<-size.mort[size.mort$year==max(size.mort$year),]
  size.selex<<-size.selex[size.selex$Factor=="Lsel",c(2,3,6:length(size.selex[1,]))]
  size.selex<<-size.selex[size.selex$year==max(size.selex$year),]
  comb.selex<<-as.matrix(size.selex)
  comb.dead<<-comb.selex
  comb.retain<<-comb.selex
  comb.Lc<<-size.reten
  ALK<<-output.orig$ALK[,,1]
  groups<<-sort(unique(forecast.orig$fleet_assignment_to_allocation_group))
  if(length(groups[groups==0])>0){groups<<-c(groups[-1],0)}
  areaNames<<-disp.Desc$AreaNames[,2]
  seasonNames<<-disp.Desc$SeasonNames[,2]
  fleetNames<<-disp.Desc$FleetNames[,2]
  groupNames<<-disp.Desc$GroupNames[match(groups,disp.Desc$GroupNames[,1]),2]
  InflDisc.Val<<-rep(0,data.orig$Nfleet)
  SlopeDisc.Val<<-rep(0,data.orig$Nfleet)
  incProgress(0.1, detail="Calculating relative fleet intensity")
  prevTotalAlloc<<-as.integer(forecast.orig$basis_for_fcast_catch_tuning)
  prevGroupAlloc<<-rep(2,length(groups))
  filledTabs <<- rep(FALSE,(length(groups)))

  rel_F_orig <<- matrix(
    NA,nrow=data.orig$nseas,ncol=data.orig$Nfleet,byrow=TRUE)
  names(rel_F_orig) <<- paste0("Fleet ",1:data.orig$Nfleet)
  aggreg.timeSeries.F<<-output.orig$timeseries[(output.orig$timeseries$Area==1 & output.orig$timeseries$Seas==1),]
  for(iseas in 1:data.orig$nseas){
    aggreg.timeSeries.F[,(8+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(1:data.orig$Nfleet))]<<-aggregate(output.orig$timeseries[output.orig$timeseries$Seas==iseas,(8+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(1:data.orig$Nfleet))],list(output.orig$timeseries[output.orig$timeseries$Seas==iseas,2]),sum)[,-1]
    rel_F_orig[iseas,]<<-as.numeric(apply(aggreg.timeSeries.F[(aggreg.timeSeries.F$Yr>=forecast.orig$Fcast_years[3] & aggreg.timeSeries.F$Yr<=forecast.orig$Fcast_years[4]),(8+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(1:data.orig$Nfleet))],2,sum)/(forecast.orig$Fcast_years[4]-forecast.orig$Fcast_years[3]+1))
  }

  rel_DB_orig <<- matrix(
    NA,nrow=data.orig$nseas,ncol=data.orig$Nfleet,byrow=TRUE)
  names(rel_DB_orig) <<- paste0("Fleet ",1:data.orig$Nfleet)
  aggreg.timeSeries.DB<<-output.orig$timeseries[(output.orig$timeseries$Area==1 & output.orig$timeseries$Seas==1),]
  for(iseas in 1:data.orig$nseas){
    aggreg.timeSeries.DB[,(10+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))]<<-aggregate(output.orig$timeseries[output.orig$timeseries$Seas==iseas,(10+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))],list(output.orig$timeseries[output.orig$timeseries$Seas==iseas,2]),sum)[,-1]
    rel_DB_orig[iseas,]<<-as.numeric(apply(aggreg.timeSeries.DB[(aggreg.timeSeries.DB$Yr>=forecast.orig$Fcast_years[3] & aggreg.timeSeries.DB$Yr<=forecast.orig$Fcast_years[4]),(10+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))],2,sum)/(forecast.orig$Fcast_years[4]-forecast.orig$Fcast_years[3]+1))*weightScale[disp.Desc$Units[1]]
  }

  rel_RB_orig <<- matrix(
    NA,nrow=data.orig$nseas,ncol=data.orig$Nfleet,byrow=TRUE)
  names(rel_RB_orig) <<- paste0("Fleet ",1:data.orig$Nfleet)
  aggreg.timeSeries.RB<<-output.orig$timeseries[(output.orig$timeseries$Area==1 & output.orig$timeseries$Seas==1),]
  for(iseas in 1:data.orig$nseas){
    aggreg.timeSeries.RB[,(11+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))]<<-aggregate(output.orig$timeseries[output.orig$timeseries$Seas==iseas,(11+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))],list(output.orig$timeseries[output.orig$timeseries$Seas==iseas,2]),sum)[,-1]
    rel_RB_orig[iseas,]<<-as.numeric(apply(aggreg.timeSeries.RB[(aggreg.timeSeries.RB$Yr>=forecast.orig$Fcast_years[3] & aggreg.timeSeries.RB$Yr<=forecast.orig$Fcast_years[4]),(11+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))],2,sum)/(forecast.orig$Fcast_years[4]-forecast.orig$Fcast_years[3]+1))*weightScale[disp.Desc$Units[1]]
  }

  rel_DN_orig <<- matrix(
    NA,nrow=data.orig$nseas,ncol=data.orig$Nfleet,byrow=TRUE)
  names(rel_DN_orig) <<- paste0("Fleet ",1:data.orig$Nfleet)
  aggreg.timeSeries.DN<<-output.orig$timeseries[(output.orig$timeseries$Area==1 & output.orig$timeseries$Seas==1),]
  for(iseas in 1:data.orig$nseas){
    aggreg.timeSeries.DN[,(13+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))]<<-aggregate(output.orig$timeseries[output.orig$timeseries$Seas==iseas,(13+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))],list(output.orig$timeseries[output.orig$timeseries$Seas==iseas,2]),sum)[,-1]
    rel_DN_orig[iseas,]<<-as.numeric(apply(aggreg.timeSeries.DN[(aggreg.timeSeries.DN$Yr>=forecast.orig$Fcast_years[3] & aggreg.timeSeries.DN$Yr<=forecast.orig$Fcast_years[4]),(13+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))],2,sum)/(forecast.orig$Fcast_years[4]-forecast.orig$Fcast_years[3]+1))
  }

  rel_RN_orig <<- matrix(
    NA,nrow=data.orig$nseas,ncol=data.orig$Nfleet,byrow=TRUE)
  names(rel_RN_orig) <<- paste0("Fleet ",1:data.orig$Nfleet)
  aggreg.timeSeries.RN<<-output.orig$timeseries[(output.orig$timeseries$Area==1 & output.orig$timeseries$Seas==1),]
  for(iseas in 1:data.orig$nseas){
    aggreg.timeSeries.RN[,(14+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))]<<-aggregate(output.orig$timeseries[output.orig$timeseries$Seas==iseas,(14+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))],list(output.orig$timeseries[output.orig$timeseries$Seas==iseas,2]),sum)[,-1]
    rel_RN_orig[iseas,]<<-as.numeric(apply(aggreg.timeSeries.RN[(aggreg.timeSeries.RN$Yr>=forecast.orig$Fcast_years[3] & aggreg.timeSeries.RN$Yr<=forecast.orig$Fcast_years[4]),(14+output.orig$ngpatterns+2*output.orig$ngpatterns*output.orig$nsexes+8*(0:(data.orig$Nfleet-1)))],2,sum)/(forecast.orig$Fcast_years[4]-forecast.orig$Fcast_years[3]+1))
  }
}

#' Build forecast choices
#'
#' Build the forcast choices section of the shiny interface
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface building
buildForecastChoices<-function(input, output, session){

  removeUI("#GlobalInputs>*",multiple=TRUE,immediate=TRUE)
  removeUI("#GlobalInputs",multiple=TRUE,immediate=TRUE)
  removeUI("#currSizeLimits>*",multiple=TRUE,immediate=TRUE)
  removeUI("#currSizeLimits",multiple=TRUE,immediate=TRUE)
  newDiv("ImplementManagement","currSizeLimits")
  newDiv("currSizeLimits","GlobalInputs")
  newFluidRow("GlobalInputs","ManagementYear")
  newColumn("ManagementYear","ManagementYearCol1",width=2)
  newColumn("ManagementYear","ManagementYearCol4",width=2)
  newColumn("ManagementYear","ManagementYearCol5",width=2)
  newColumn("ManagementYear","ManagementYearCol6",width=2)
  newColumn("ManagementYear","ManagementYearCol7",width=4)
  newColumn("ManagementYearCol1","ManagementYearCol2",width=1)
  newColumn("ManagementYearCol1","ManagementYearCol3",width=11)
  newSlider("ManagementYearCol3","ManagementYearInput",titleText="Management Starting Year",min=data.orig$endyr,max=(as.integer(format(Sys.Date(), "%Y"))+10),value=(as.integer(format(Sys.Date(), "%Y"))+1),step=1,sep="")

  #input$ManagementYearInput<<-(as.integer(format(Sys.Date(), "%Y"))+1)
  targetNames<<-c("SPR","Spawning Biomass Ratio","Maximize Catch","Use previous estimate")
  targetNames2<<-c("SPR Target Value","Ignore ","Spawning Biomass Ratio")
  targetNames3<<-c("Implement Optimal","Fixed Fraction Fmsy","Constant Catch","Kobe Matrix")
  #targetNames3<<-c("Implement Optimal","Fixed Fraction Fmsy","Constant Catch","Kobe Matrix")
  targetValue<<-c(forecast.orig$SPRtarget,1,forecast.orig$Btarget)

  dir.msy<<-paste0(dir.base,"/msy")
  if(dir.exists(dir.msy)){
    newRadio("ManagementYearCol4","ForecastTarget",label="Optimal Fishing Target",selected=forecast.orig$MSY,choiceNames=targetNames,choiceValues=c(1,3,2,4),inline=FALSE)
  }else{
    newRadio("ManagementYearCol4","ForecastTarget",label="Optimal Fishing Target",selected=forecast.orig$MSY,choiceNames=targetNames[1:3],choiceValues=c(1,3,2),inline=FALSE)
  }
  newSlider("ManagementYearCol5","TargetValue",titleText=targetNames2[forecast.orig$MSY],min=0,max=1,value=targetValue[forecast.orig$MSY],step=0.01)

  newSlider("ManagementYearCol5","TargetYears",titleText="Optimal target year range",min=data.orig$endyr,max=(data.orig$endyr+forecast.orig$Nforecastyrs),value=disp.Desc$TargetYears,step=1,sep="")

  newRadio("ManagementYearCol6","ForecastApplied",label="Implemented Fishing",selected=1,choiceNames=targetNames3,choiceValues=c(1,2,3,4),inline=FALSE)

  newRadio("ManagementYearCol6","Rebuild",label="With rebuilding target",selected=disp.Desc$ImplRebuild,choiceNames=c("yes","no"),choiceValues=c(1,2),inline=TRUE,hidden=TRUE)

  newSlider("ManagementYearCol7","ABCfrac","F Fraction of Fmsy",min=0,max=2,value=disp.Desc$ABCFrac,step=0.01,hidden=TRUE)

  newtextInput("ManagementYearCol7","ConstCatchInput",disp.Desc$ConstCatch,label=paste0("Constant Catch Value (",weightNames[disp.Desc$Units[1]],")"),hidden=FALSE)

  newSlider("ManagementYearCol7","ConstCatchfrac","Catch Fraction of MSY",min=0,max=2,value=disp.Desc$ABCFrac,step=0.01, hidden=FALSE)

  newtextInput("ManagementYearCol7","ConstCatchRebuild","",label=paste0("Constant Catch Value (",weightNames[disp.Desc$Units[1]],")"), hidden=FALSE)

  newSlider("ManagementYearCol7","RebuildTargetFraction","Rebuild Fraction of MSY",min=0,max=1,value=disp.Desc$RebuildFrac,step=0.01,hidden=TRUE)
  newSlider("ManagementYearCol7","RebuildTargetYear",titleText="Rebuild target year",min=data.orig$endyr,max=(data.orig$endyr+forecast.orig$Nforecastyrs),value=(as.integer(format(Sys.Date(), "%Y"))+disp.Desc$RebuildYears),step=1,sep="", hidden=TRUE)

  newSelectInput("ManagementYearCol7","CompAssessments","Kobe Comparison Assessments",choices = list("Collecting assessment list" = "BL"),multiple=TRUE,hidden=FALSE)

  newtextInput("ManagementYearCol7","KobeMinCatch",label="Min Catch",value="0")
  newtextInput("ManagementYearCol7","KobeMaxCatch",label="Max Catch",value="100000")
  newtextInput("ManagementYearCol7","KobeNumCatch",label="Num Catches",value="11")
  newOutputtext("ManagementYearCol7","KobeCatches",inline=TRUE)

  output$KobeCatches<-renderText(paste0("Tested catches will be: ",paste0(seq(as.numeric(input$KobeMinCatch),as.numeric(input$KobeMaxCatch),length.out = as.numeric(input$KobeNumCatch)),collapse = ","),"."))

  newValueInput("ManagementYearCol7","KobeNextCatch",label="",min=1,max=100,step=1,value=1,hidden=TRUE)
}

#' Build tab panels
#'
#' Build the tab panels for each group
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface building
buildManagementTabPanels<-function(input, output, session){
  removeUI("#ManagementTabPanelDiv>*",multiple=TRUE,immediate=TRUE)
  removeUI("#ManagementTabPanelDiv",multiple=TRUE,immediate=TRUE)
  newDiv("currSizeLimits","ManagementTabPanelDiv")
  tabTitles<<-c("Allocations and Catch",paste0(groupNames," Selectivity"))
  tabValues<<-c("Allocations",paste0("Group",groups))
  tabDivIds<-c("AllocationManagement",paste0("Group",groups,"Management"))
  tabUIElements<-paste0("tags$div(id='",tabDivIds,"')")
  tabs<-paste0("tabPanel(title='",tabTitles,"',{",tabUIElements,"}),",collapse = "")
  tabset<-paste0("tabsetPanel(",tabs,"id='ManagementTabset',selected='",tabTitles[1],"')")

  insertUI(
    selector = "#ManagementTabPanelDiv",
    where = "beforeEnd",
    ui = eval(parse(text=tabset)),
    immediate=FALSE
  )
}

#' Build tab elements
#'
#' Build the UI elements for each tab of the shiny interface
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface building
buildTabElements<-function(input, output, session){
  newFluidRow(paste0('AllocationManagement'),paste0('AllocationTitles'))
  newColumn(paste0('AllocationTitles'),paste0('AllocationTitlesCol2'),width=4)
  newColumn(paste0('AllocationTitles'),paste0('AllocationTitlesCol'),width=8)
  newColumn(paste0('AllocationTitlesCol'),paste0('AllocationTitlesCol3'),width=6)
  newColumn(paste0('AllocationTitlesCol'),paste0('AllocationTitlesCol4'),width=6)

  newtext(paste0('AllocationTitlesCol2'),paste0('Grouptitle'),inputText=paste0('Group specific allocation fractions by:'))
  newRadio('AllocationTitlesCol2','GroupAllocUnits',label=NULL,selected=forecast.orig$basis_for_fcast_catch_tuning,choiceValues=c(2,3,5,6),choiceNames = c('Dead Weight','Retained Weight','Dead Numbers','Retained Numbers'),inline=TRUE)
  #newActionButton('AllocationTitlesCol2','ScaleAlloc',label='Re-scale Allocations')
  newSlider('AllocationTitlesCol2','RelAllocYears',titleText ='Match historic allocation',min=data.orig$styr,max=data.orig$endyr,value=c(forecast.orig$Fcast_years[3],forecast.orig$Fcast_years[4]),step=1,sep="")
  newtext(paste0('AllocationTitlesCol3'),paste0('Fleettitle'),inputText=paste0('Fleet allocation fractions within group'))
  newRadio('AllocationTitlesCol3','displaySeason',label='Show:',selected=1,choiceValues=c(1,2),choiceNames =c('Annual','Seasons'),inline=TRUE)
  newtext(paste0('AllocationTitlesCol4'),paste0('CatchHisttitle'),inputText=paste0('Fixed fleet inputs'))

  incProgress(0.1,detail="start creating tab elements")
  for(j in 1:length(groups)){
    incProgress(0.5/length(groups),detail=paste0("Creating Elements for group ",groupNames[j]))
    newFluidRow(paste0("AllocationManagement"),paste0("Group",j,"Allocation"))
    newColumn(paste0("Group",j,"Allocation"),paste0("Group",j,"AllocationCol2"),width=3)
    newColumn(paste0("Group",j,"Allocation"),paste0("Group",j,"AllocationCol22"),width=1)
    newColumn(paste0("Group",j,"Allocation"),paste0("Group",j,"AllocationCol3"),width=8)
    if(groups[j]!=0){
      temp.max.group[j]<<-1
      temp.val.group[j]<<-forecast.orig$allocation_among_groups[j]
      newValueInput(paste0("Group",j,"AllocationCol2"),paste0("Group",j,"AllocationSlide"),label=paste0(groupNames[j]," Catch Allocation Fraction"), value=temp.val.group[j], step=(10^-sliderResolution))
      newCheckbox(paste0("Group",j,"AllocationCol22"),paste0("Group",j,"AllocationFixed"))
      #newSlider(paste0("Group",j,"AllocationCol2"),paste0("Group",j,"AllocationSlide"),titleText=paste0(groupNames[j]," Catch Allocation Fraction"), min=0, max=temp.max.group[j], value=temp.val.group[j],step=(10^-sliderResolution))
    }else{
      newtext(paste0("Group",j,"AllocationCol2"),paste0("UnallocatedGrouptitle"),inputText=paste0(groupNames[j]," has no fixed catch allocation"))
      insertUI(selector = paste0("#Group",j,"AllocationCol2"),where="beforeEnd",ui=br(),immediate = TRUE)
    }

    prevGroupAlloc[j]<<-2
    groupfleets<-which(forecast.orig$fleet_assignment_to_allocation_group==groups[j])

    if(length(groupfleets)==1){
      newtext(paste0("Group",j,"AllocationCol2"),paste0("Group",j,"FleetAllocTitle"),inputText=paste0("Allocate fleet fraction of group allocation by relative:"),hidden=TRUE)
      newRadio(paste0("Group",j,"AllocationCol2"),paste0("Group",j,"FleetAllocUnits"),label=NULL,selected=2,choiceValues=c(1,2),choiceNames = c("Catch","F"),inline=TRUE,hidden=TRUE)
    }else{
      newtext(paste0("Group",j,"AllocationCol2"),paste0("Group",j,"FleetAllocTitle"),inputText=paste0("Allocate fleet fraction of group allocation by relative:"))
      newRadio(paste0("Group",j,"AllocationCol2"),paste0("Group",j,"FleetAllocUnits"),label=NULL,selected=2,choiceValues=c(1,2),choiceNames = c("Catch","F"),inline=TRUE)
    }

    for(i in groupfleets){
      incProgress((0.2/data.orig$Nfleet),detail=paste0("Creating Elements for group ",groupNames[j]," Fleet ",fleetNames[i]))
      newFluidRow(paste0("Group",j,"AllocationCol3"),paste0("Fleet",i,"Allocation"))
      newColumn(paste0("Fleet",i,"Allocation"),paste0("Fleet",i,"AllocationCol"),width=5)
      newColumn(paste0("Fleet",i,"Allocation"),paste0("Fleet",i,"AllocationCol2"),width=1)
      newColumn(paste0("Fleet",i,"Allocation"),paste0("Fleet",i,"catchHist"),width=6)
      currfleet<-which(groupfleets==i)
      fleets<-groupfleets[-c(currfleet:length(groupfleets))]
      temp.max.fleet[i]<<-1

      if(sum(rel_F_orig[,groupfleets])==0){
        temp.val.fleet[i]<<-0
      }else{
        temp.val.fleet[i]<<-sum(rel_F_orig[,i])/sum(rel_F_orig[,groupfleets])
      }
      if(length(groupfleets==1)){
        newValueInput(paste0("Fleet",i,"AllocationCol"),paste0("Fleet",i,"AllocationSlide"),label=paste0(fleetNames[i],": Annual Fraction of ",groupNames[j]), min=0, max=temp.max.fleet[i], value=temp.val.fleet[i],step=(10^-sliderResolution),hidden=TRUE)
        newCheckbox(paste0("Fleet",i,"AllocationCol2"),paste0("Fleet",i,"AllocationFixed"),hidden=TRUE)
      }else{
        newValueInput(paste0("Fleet",i,"AllocationCol"),paste0("Fleet",i,"AllocationSlide"),label=paste0(fleetNames[i],": Annual Fraction of ",groupNames[j]), min=0, max=temp.max.fleet[i], value=temp.val.fleet[i],step=(10^-sliderResolution))
        newCheckbox(paste0("Fleet",i,"AllocationCol2"),paste0("Fleet",i,"AllocationFixed"))
      }

      for(k in 1:data.orig$nseas){
        newFluidRow(paste0("Group",j,"AllocationCol3"),paste0("Fleet",i,"Seas",k,"Allocation"))
        newColumn(paste0("Fleet",i,"Seas",k,"Allocation"),paste0("Fleet",i,"Seas",k,"AllocationCol"),width=5)
        newColumn(paste0("Fleet",i,"Seas",k,"Allocation"),paste0("Fleet",i,"Seas",k,"AllocationCol2"),width=1)
        newColumn(paste0("Fleet",i,"Seas",k,"Allocation"),paste0("Fleet",i,"Seas",k,"catchHist"),width=6)

        temp.max.fleet.seas[k,i]<<-1
        temp.val.fleet.seas[k,i]<<-rel_F_orig[k,i]/sum(rel_F_orig[,groupfleets])
        newValueInput(paste0("Fleet",i,"Seas",k,"AllocationCol"),paste0("Fleet",i,"Seas",k,"AllocationSlide"),label=paste0(fleetNames[i],":",seasonNames[k]," Fraction of ",groupNames[j]), min=0, max=temp.max.fleet.seas[k,i], value=temp.val.fleet.seas[k,i],step=(10^-sliderResolution),hidden=FALSE)
        newCheckbox(paste0("Fleet",i,"Seas",k,"AllocationCol2"),paste0("Fleet",i,"Seas",k,"AllocationFixed"))
      }

      newFluidRow(paste0("Group",groups[j],"Management"),paste0("fleet",i,"SizeLimPanel"))
      newColumn(paste0("fleet",i,"SizeLimPanel"),paste0("fleet",i,"selexPlot"),width=8)
      newColumn(paste0("fleet",i,"SizeLimPanel"),paste0("fleet",i,"selexControls"),width=4)
      newDiv(paste0("fleet",i,"selexPlot"),paste0("fleet",i,"selexPlotPanel"),placement = "afterBegin")
      newDiv(paste0("fleet",i,"selexControls"),paste0("fleet",i,"sizeLimSlide"),placement = "afterBegin")
      newDiv(paste0("fleet",i,"sizeLimSlide"),paste0("fleet",i,"sizeLimSlope"),placement = "afterEnd")
      newDiv(paste0("fleet",i,"sizeLimSlope"),paste0("fleet",i,"retentionMax"),placement = "afterEnd")
      newDiv(paste0("fleet",i,"retentionMax"),paste0("fleet",i,"discMort"),placement = "afterEnd")
      newDiv(paste0("Fleet",i,"catchHist"),paste0("fleet",i,"catchHistTitle"),placement = "afterBegin")
      newtext(paste0("fleet",i,"catchHistTitle"),paste0("fleet",i,"manageHeader"),paste0(""))
      newDiv(paste0("fleet",i,"catchHistTitle"),paste0("fleet",i,"catchHistTitle2"),placement = "afterEnd")

      fleetData<-forecast.orig$ForeCatch[forecast.orig$ForeCatch[,3]==i,,drop=FALSE]
      CatchCol<-5
      if(length(fleetData[,1])==0){
        if(data.orig$units_of_catch[i]==1){
          newtext(paste0("fleet",i,"catchHistTitle2"),paste0("fleet",i,"catchHistHeader"),paste0("Retained Catch in Weight (",weightNames[disp.Desc$Units[1]],") \n"))
          CatchCol<-5
        }else if (data.orig$units_of_catch[i]==2){
          newtext(paste0("fleet",i,"catchHistTitle2"),paste0("fleet",i,"catchHistHeader"),paste0("Retained Catch in Numbers (1000's) \n"))
          CatchCol<-7
        }else{
          print("ERROR:This fleet doesn't seem to have catch units specified")
        }
      }else{
        if(fleetData[1,5]==2){
          fleetData[,4]<-fleetData[,4]*weightScale[disp.Desc$Units[1]]#2.20462
          newtext(paste0("fleet",i,"catchHistTitle2"),paste0("fleet",i,"catchHistHeader"),paste0("Total Dead Weight (",weightNames[disp.Desc$Units[1]],") \n"))
          CatchCol<-5:6
        }else if (fleetData[1,5]==5){
            newtext(paste0("fleet",i,"catchHistTitle2"),paste0("fleet",i,"catchHistHeader"),paste0("Total Dead Numbers (1000's) \n"))
            CatchCol<-7:8
        }else if(fleetData[1,5]==3){
            fleetData[,4]<-fleetData[,4]*weightScale[disp.Desc$Units[1]]#2.20462
            newtext(paste0("fleet",i,"catchHistTitle2"),paste0("fleet",i,"catchHistHeader"),paste0("Retained Catch in Weight (",weightNames[disp.Desc$Units[1]],") \n"))
            CatchCol<-5
        }else if (fleetData[1,5]==6){
            newtext(paste0("fleet",i,"catchHistTitle2"),paste0("fleet",i,"catchHistHeader"),paste0("Retained Catch in Numbers (1000's) \n"))
            CatchCol<-7
        }else if(fleetData[1,5]==99){
          newtext(paste0("fleet",i,"catchHistTitle2"),paste0("fleet",i,"catchHistHeader"),paste0("Harvest Rate (F) \n"))
          CatchCol<-9
        }
      }

      newDiv(paste0("Fleet",i,"catchHist"),paste0("fleet",i,"recentCatches"),placement = "beforeEnd")
      newDiv(paste0("Fleet",i,"catchHist"),paste0("fleet",i,"recentCatchesAnnual"),placement = "beforeEnd")
      #newDiv(paste0("Fleet",i,"Seas",k,"catchHist"),paste0("fleet",i,"Seas",k,"recentCatchesSeasonal"),placement = "beforeEnd",hidden=FALSE)

      for(hh in 1:((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs)){
        newRows<-seq(1,((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs),3)
        if(is.element(hh,newRows)){
          hhh<-which(newRows==hh)
          newFluidRow(paste0("fleet",i,"recentCatchesAnnual"),paste0("fleet",i,"recentCatchVals",hhh))
          newColumn(paste0("fleet",i,"recentCatchVals",hhh),paste0("fleet",i,"recentCatchQuantTitlerow",hhh,"col1"),width=4)
          newColumn(paste0("fleet",i,"recentCatchVals",hhh),paste0("fleet",i,"recentCatchQuantTitlerow",hhh,"col2"),width=4)
          newColumn(paste0("fleet",i,"recentCatchVals",hhh),paste0("fleet",i,"recentCatchQuantTitlerow",hhh,"col3"),width=4)
        }
        currYr<-(data.orig$endyr+hh)
        if(hh==((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs)){
          CatchHistYr<-fleetData[fleetData[,1]==currYr,,drop=FALSE]
          if(length(CatchHistYr[,1])==data.orig$nseas){
            newtextInput(paste0("fleet",i,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"recentCatchQuantAll"),value=paste0(sum(CatchHistYr[,4])),label=paste0("All Later Years"))
          }else{
            newtextInput(paste0("fleet",i,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"recentCatchQuantAll"),value=paste0(""),label=paste0("All Later Years"))
          }
        }else{
          CatchHistYr<-fleetData[fleetData[,1]==currYr,,drop=FALSE]
          if(length(CatchHistYr[,1])==data.orig$nseas){
            newtextInput(paste0("fleet",i,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"recentCatchQuant",hh),value=paste0(sum(CatchHistYr[,4])),label=paste0(currYr))
          }else{
            if(currYr<(as.integer(format(Sys.Date(), "%Y"))+1)){
              newtextInput(paste0("fleet",i,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"recentCatchQuant",hh),value=paste0(sum(projCatch[projCatch$year==currYr & projCatch$fleet==i,CatchCol])),label=paste0(currYr))
            }else{
              newtextInput(paste0("fleet",i,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"recentCatchQuant",hh),value=paste0(""),label=paste0(currYr))
            }
          }
        }
      }

      for(k in 1:data.orig$nseas){
        for(hh in 1:((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs)){
          newRows<-seq(1,((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs),3)
          if(is.element(hh,newRows)){
            hhh<-which(newRows==hh)
            newFluidRow(paste0("Fleet",i,"Seas",k,"catchHist"),paste0("fleet",i,"seas",k,"recentCatchVals",hhh))
            newColumn(paste0("fleet",i,"seas",k,"recentCatchVals",hhh),paste0("fleet",i,"seas",k,"recentCatchQuantTitlerow",hhh,"col1"),width=4)
            newColumn(paste0("fleet",i,"seas",k,"recentCatchVals",hhh),paste0("fleet",i,"seas",k,"recentCatchQuantTitlerow",hhh,"col2"),width=4)
            newColumn(paste0("fleet",i,"seas",k,"recentCatchVals",hhh),paste0("fleet",i,"seas",k,"recentCatchQuantTitlerow",hhh,"col3"),width=4)
          }
          currYr<-(data.orig$endyr+hh)
          if(hh==((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs)){
            CatchHistYrSeas<-fleetData[fleetData[,1]==currYr & fleetData[,2]==k,,drop=FALSE]
            if(length(CatchHistYrSeas[,1])==1){
              newtextInput(paste0("fleet",i,"seas",k,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"seas",k,"recentCatchQuantAll"),value=paste0(sum(CatchHistYrSeas[,4])),label=paste0(seasonNames[k]," All Later Years"),hidden=FALSE)
            }else{
              newtextInput(paste0("fleet",i,"seas",k,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"seas",k,"recentCatchQuantAll"),value=paste0(""),label=paste0(seasonNames[k]," All Later Years"),hidden=FALSE)
            }
          }else{
            CatchHistYrSeas<-fleetData[fleetData[,1]==currYr & fleetData[,2]==k,,drop=FALSE]
            if(length(CatchHistYrSeas[,1])==1){
              newtextInput(paste0("fleet",i,"seas",k,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"seas",k,"recentCatchQuant",hh),value=paste0(sum(CatchHistYrSeas[,4])),label=paste0(seasonNames[k]," ",currYr),hidden=FALSE)
            }else{
              if(currYr<(as.integer(format(Sys.Date(), "%Y"))+1)){
                newtextInput(paste0("fleet",i,"seas",k,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"seas",k,"recentCatchQuant",hh),value=paste0(sum(projCatch[projCatch$year==currYr & projCatch$fleet==i  & projCatch$season==k,CatchCol])),label=paste0(seasonNames[k]," ",currYr),hidden=FALSE)
              }else{
                newtextInput(paste0("fleet",i,"seas",k,"recentCatchQuantTitlerow",hhh,"col",(hh-newRows[hhh]+1)),paste0("fleet",i,"seas",k,"recentCatchQuant",hh),value=paste0(""),label=paste0(seasonNames[k]," ",currYr),hidden=FALSE)
              }
            }
          }
        }
      }

      lcRow.temp<-grep(paste0("Size Retention  ",1,"  fleet/Survey  ",i),row.names(control.orig$Select_Params))[1]
      #print(lcRow.temp)
      #print(control.orig$Select_Params[lcRow.temp,13])
      if(control.orig$Select_Params[lcRow.temp,13]==0){
        Lc.Val.Temp<-control.orig$Select_Params[lcRow.temp,3]
      }else if(control.orig$Select_Params[lcRow.temp,13]<0){
        if(control.orig$Select_Params_TV$Cust_Blk==0){
          Lc.Val.Temp<-control.orig$Select_Params_TV$BLK_Params[1,3]
        }else{
          tempCont<-control.orig$Select_Params[1:lcRow.temp,,drop=FALSE]
          tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
          numTVLc.temp<-length(tempCont[,13])
          rowsTVLc<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVLc.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          Lc.Val.Temp<-rowsTVLc[1,3]
        }
      }else if(control.orig$Select_Params[lcRow.temp,13]>0){
        if(control.orig$BlockPatterns[[control.orig$Select_Params[lcRow.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcRow.temp,13]]]<data.orig$endyr){
          Lc.Val.Temp<-control.orig$Select_Params[lcRow.temp,3]
        }else{
          if(control.orig$Select_Params_TV$Cust_Blk==0){
            rowsTVLc<-control.orig$Select_Params_TV$BLK_Params
          }else{
            tempCont<-control.orig$Select_Params[1:lcRow.temp,,drop=FALSE]
            tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
            numTVLc.temp<-length(tempCont[,13])
            rowsTVLc<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVLc.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          }
          if(control.orig$Select_Params[lcRow.temp,14]==0){
            Lc.Val.Temp<-control.orig$Select_Params[lcRow.temp,13]*exp(rowsTVLc[length(rowsTVLc[,1]),3])
          }else if(control.orig$Select_Params[lcRow.temp,14]==1){
            Lc.Val.Temp<-control.orig$Select_Params[lcRow.temp,13]+rowsTVLc[length(rowsTVLc[,1]),3]
          }else if(control.orig$Select_Params[lcRow.temp,14]==2){
            Lc.Val.Temp<-rowsTVLc[length(rowsTVLc[,1]),3]
          }else if(control.orig$Select_Params[lcRow.temp,14]==3){
            Lc.Val.Temp<-control.orig$Select_Params[lcRow.temp,13]+sum(rowsTVLc[,3])
          }
        }
      }
      maxSize<-100
      if(data.orig$lbin_method==1){
        maxSize<-max(data.orig$lbin_vector)
      }else if(data.orig$lbin_method==2){
        maxSize<-data.orig$maximum_size
      }else if(data.orig$lbin_method==3){
        maxSize<-max(data.orig$lbin_vector_pop)
      }

      newSlider(paste0("fleet",i,"sizeLimSlide"),paste0("fleet",i,"Lc"),titleText = paste0("Lc (",lengthNames[disp.Desc$Units[2]],")"),min=0,max=ceiling(maxSize*lengthScale[disp.Desc$Units[2]]),value=Lc.Val.Temp*lengthScale[disp.Desc$Units[2]],step=1)

      lcSlope.temp<-grep(paste0("Size Retention  ",2,"  fleet/Survey  ",i),row.names(control.orig$Select_Params))[1]
      if(control.orig$Select_Params[lcSlope.temp,13]==0){
        Slope.Val.Temp<-control.orig$Select_Params[lcSlope.temp,3]
      }else if(control.orig$Select_Params[lcSlope.temp,13]<0){
        if(control.orig$Select_Params_TV$Cust_Blk==0){
          Slope.Val.Temp<-control.orig$Select_Params_TV$BLK_Params[1,3]
        }else{
          tempCont<-control.orig$Select_Params[1:lcSlope.temp,,drop=FALSE]
          tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
          numTVSlope.temp<-length(tempCont[,13])
          rowsTVSlope<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVSlope.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          Slope.Val.Temp<-rowsTVSlope[1,3]
        }
      }else if(control.orig$Select_Params[lcSlope.temp,13]>0){
        if(control.orig$BlockPatterns[[control.orig$Select_Params[lcSlope.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcSlope.temp,13]]]<data.orig$endyr){
          Slope.Val.Temp<-control.orig$Select_Params[lcSlope.temp,3]
        }else{
          if(control.orig$Select_Params_TV$Cust_Blk==0){
            rowsTVSlope<-control.orig$Select_Params_TV$BLK_Params
          }else{
            tempCont<-control.orig$Select_Params[1:lcSlope.temp,,drop=FALSE]
            tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
            numTVSlope.temp<-length(tempCont[,13])
            rowsTVSlope<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVSlope.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          }
          if(control.orig$Select_Params[lcSlope.temp,14]==0){
            Slope.Val.Temp<-control.orig$Select_Params[lcSlope.temp,13]*exp(rowsTVSlope[length(rowsTVSlope[,1]),3])
          }else if(control.orig$Select_Params[lcSlope.temp,14]==1){
            Slope.Val.Temp<-control.orig$Select_Params[lcSlope.temp,13]+rowsTVSlope[length(rowsTVSlope[,1]),3]
          }else if(control.orig$Select_Params[lcSlope.temp,14]==2){
            Slope.Val.Temp<-rowsTVSlope[length(rowsTVSlope[,1]),3]
          }else if(control.orig$Select_Params[lcSlope.temp,14]==3){
            Slope.Val.Temp<-control.orig$Select_Params[lcSlope.temp,13]+sum(rowsTVSlope[,3])
          }
        }
      }
      if(Slope.Val.Temp==0)
      {
        Slope.Val.Temp<-0.01
      }
      if(Slope.Val.Temp>0){
        vals.temp<-unique(c(sort(c(Slope.Val.Temp,0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20,50,100,200,500,1000,2000)),Inf,-2000,-1000,-500,-200,-100,-50,-20,-10,-5,-2,-1,-0.5,-0.2,-0.1,-0.05,-0.02,-0.01))
      }else{
        vals.temp<-unique(c(0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20,50,100,200,500,1000,2000,Inf,sort(c(Slope.Val.Temp,-2000,-1000,-500,-200,-100,-50,-20,-10,-5,-2,-1,-0.5,-0.2,-0.1,-0.05,-0.02,-0.01))))
      }
      #newTextSlider(paste0("fleet",i,"sizeLimSlope"),paste0("fleet",i,"Lcslope"),label = "Lc slope", choices=paste(vals.temp),selected = paste(Slope.Val.Temp))
      #newTextSlider(paste0("fleet",i,"sizeLimSlope"),paste0("fleet",i,"Lcslope"),label = "Lc slope", choices=c(0,10,200,Inf,-200,-10,-0.01),selected = 0)
      #showElement(paste0("fleet",i,"Lcslope"))
      newSlider(paste0("fleet",i,"sizeLimSlope"),paste0("fleet",i,"Lcslope"),titleText = "Lc slope",min=0.01,max=1000,value=Slope.Val.Temp,step=0.1)

      lcMaxRet.temp<-grep(paste0("Size Retention  ",3,"  fleet/Survey  ",i),row.names(control.orig$Select_Params))[1]
      if(control.orig$Select_Params[lcMaxRet.temp,13]==0){
        MaxRet.Val.Temp<-control.orig$Select_Params[lcMaxRet.temp,3]
      }else if(control.orig$Select_Params[lcMaxRet.temp,13]<0){
        if(control.orig$Select_Params_TV$Cust_Blk==0){
          MaxRet.Val.Temp<-control.orig$Select_Params_TV$BLK_Params[1,3]
        }else{
          tempCont<-control.orig$Select_Params[1:lcMaxRet.temp,,drop=FALSE]
          tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
          numTVMaxRet.temp<-length(tempCont[,13])
          rowsTVMaxRet<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVMaxRet.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          MaxRet.Val.Temp<-rowsTVMaxRet[1,3]
        }
      }else if(control.orig$Select_Params[lcMaxRet.temp,13]>0){
        if(control.orig$BlockPatterns[[control.orig$Select_Params[lcMaxRet.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcMaxRet.temp,13]]]<data.orig$endyr){
          MaxRet.Val.Temp<-control.orig$Select_Params[lcMaxRet.temp,3]
        }else{
          if(control.orig$Select_Params_TV$Cust_Blk==0){
            rowsTVMaxRet<-control.orig$Select_Params_TV$BLK_Params
          }else{
            tempCont<-control.orig$Select_Params[1:lcMaxRet.temp,,drop=FALSE]
            tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
            numTVMaxRet.temp<-length(tempCont[,13])
            rowsTVMaxRet<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVMaxRet.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          }
          if(control.orig$Select_Params[lcMaxRet.temp,14]==0){
            MaxRet.Val.Temp<-control.orig$Select_Params[lcMaxRet.temp,13]*exp(rowsTVMaxRet[length(rowsTVMaxRet[,1]),3])
          }else if(control.orig$Select_Params[lcMaxRet.temp,14]==1){
            MaxRet.Val.Temp<-control.orig$Select_Params[lcMaxRet.temp,13]+rowsTVMaxRet[length(rowsTVMaxRet[,1]),3]
          }else if(control.orig$Select_Params[lcMaxRet.temp,14]==2){
            MaxRet.Val.Temp<-rowsTVMaxRet[length(rowsTVMaxRet[,1]),3]
          }else if(control.orig$Select_Params[lcMaxRet.temp,14]==3){
            MaxRet.Val.Temp<-control.orig$Select_Params[lcMaxRet.temp,13]+sum(rowsTVMaxRet[,3])
          }
        }
      }
      newSlider(paste0("fleet",i,"retentionMax"),paste0("fleet",i,"retentionMaxSlider"),titleText = "Max Retention",min=0,max=1,value=MaxRet.Val.Temp,step=0.001)

      lcMaxDisc.temp<-grep(paste0("Size Discard  ",3,"  fleet/Survey  ",i),row.names(control.orig$Select_Params))[1]
      if(control.orig$Select_Params[lcMaxDisc.temp,13]==0){
        MaxDisc.Val.Temp<-control.orig$Select_Params[lcMaxDisc.temp,3]
      }else if(control.orig$Select_Params[lcMaxDisc.temp,13]<0){
        if(control.orig$Select_Params_TV$Cust_Blk==0){
          MaxDisc.Val.Temp<-control.orig$Select_Params_TV$BLK_Params[1,3]
        }else{
          tempCont<-control.orig$Select_Params[1:lcMaxDisc.temp,,drop=FALSE]
          tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
          numTVMaxDisc.temp<-length(tempCont[,13])
          rowsTVMaxDisc<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVMaxDisc.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          MaxDisc.Val.Temp<-rowsTVMaxDisc[1,3]
        }
      }else if(control.orig$Select_Params[lcMaxDisc.temp,13]>0){
        if(control.orig$BlockPatterns[[control.orig$Select_Params[lcMaxDisc.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcMaxDisc.temp,13]]]<data.orig$endyr){
          MaxDisc.Val.Temp<-control.orig$Select_Params[lcMaxDisc.temp,3]
        }else{
          if(control.orig$Select_Params_TV$Cust_Blk==0){
            rowsTVMaxDisc<-control.orig$Select_Params_TV$BLK_Params
          }else{
            tempCont<-control.orig$Select_Params[1:lcMaxDisc.temp,,drop=FALSE]
            tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
            numTVMaxDisc.temp<-length(tempCont[,13])
            rowsTVMaxDisc<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVMaxDisc.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          }
          if(control.orig$Select_Params[lcMaxDisc.temp,14]==0){
            MaxDisc.Val.Temp<-control.orig$Select_Params[lcMaxDisc.temp,13]*exp(rowsTVMaxDisc[length(rowsTVMaxDisc[,1]),3])
          }else if(control.orig$Select_Params[lcMaxDisc.temp,14]==1){
            MaxDisc.Val.Temp<-control.orig$Select_Params[lcMaxDisc.temp,13]+rowsTVMaxDisc[length(rowsTVMaxDisc[,1]),3]
          }else if(control.orig$Select_Params[lcMaxDisc.temp,14]==2){
            MaxDisc.Val.Temp<-rowsTVMaxDisc[length(rowsTVMaxDisc[,1]),3]
          }else if(control.orig$Select_Params[lcMaxDisc.temp,14]==3){
            MaxDisc.Val.Temp<-control.orig$Select_Params[lcMaxDisc.temp,13]+sum(rowsTVMaxDisc[,3])
          }
        }
      }
      newSlider(paste0("fleet",i,"discMort"),paste0("fleet",i,"discMortSlider"),titleText = "Discard Mortality",min=0,max=1,value=MaxDisc.Val.Temp,step=0.001)

      lcInflDisc.temp<-grep(paste0("Size Discard  ",1,"  fleet/Survey  ",i),row.names(control.orig$Select_Params))[1]
      if(control.orig$Select_Params[lcInflDisc.temp,13]==0){
        InflDisc.Val.Temp<-control.orig$Select_Params[lcInflDisc.temp,3]
      }else if(control.orig$Select_Params[lcInflDisc.temp,13]<0){
        if(control.orig$Select_Params_TV$Cust_Blk==0){
          InflDisc.Val.Temp<-control.orig$Select_Params_TV$BLK_Params[1,3]
        }else{
          tempCont<-control.orig$Select_Params[1:lcInflDisc.temp,,drop=FALSE]
          tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
          numTVInflDisc.temp<-length(tempCont[,13])
          rowsTVInflDisc<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVInflDisc.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          InflDisc.Val.Temp<-rowsTVInflDisc[1,3]
        }
      }else if(control.orig$Select_Params[lcInflDisc.temp,13]>0){
        if(control.orig$BlockPatterns[[control.orig$Select_Params[lcInflDisc.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcInflDisc.temp,13]]]<data.orig$endyr){
          InflDisc.Val.Temp<-control.orig$Select_Params[lcInflDisc.temp,3]
        }else{
          if(control.orig$Select_Params_TV$Cust_Blk==0){
            rowsTVInflDisc<-control.orig$Select_Params_TV$BLK_Params
          }else{
            tempCont<-control.orig$Select_Params[1:lcInflDisc.temp,,drop=FALSE]
            tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
            numTVInflDisc.temp<-length(tempCont[,13])
            rowsTVInflDisc<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVInflDisc.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          }
          if(control.orig$Select_Params[lcInflDisc.temp,14]==0){
            InflDisc.Val.Temp<-control.orig$Select_Params[lcInflDisc.temp,13]*exp(rowsTVInflDisc[length(rowsTVInflDisc[,1]),3])
          }else if(control.orig$Select_Params[lcInflDisc.temp,14]==1){
            InflDisc.Val.Temp<-control.orig$Select_Params[lcInflDisc.temp,13]+rowsTVInflDisc[length(rowsTVInflDisc[,1]),3]
          }else if(control.orig$Select_Params[lcInflDisc.temp,14]==2){
            InflDisc.Val.Temp<-rowsTVInflDisc[length(rowsTVInflDisc[,1]),3]
          }else if(control.orig$Select_Params[lcInflDisc.temp,14]==3){
            InflDisc.Val.Temp<-control.orig$Select_Params[lcInflDisc.temp,13]+sum(rowsTVInflDisc[,3])
          }
        }
      }
      InflDisc.Val[i]<<-InflDisc.Val.Temp

      lcSlopeDisc.temp<-grep(paste0("Size Discard  ",2,"  fleet/Survey  ",i),row.names(control.orig$Select_Params))[1]
      if(control.orig$Select_Params[lcSlopeDisc.temp,13]==0){
        SlopeDisc.Val.Temp<-control.orig$Select_Params[lcSlopeDisc.temp,3]
      }else if(control.orig$Select_Params[lcSlopeDisc.temp,13]<0){
        if(control.orig$Select_Params_TV$Cust_Blk==0){
          SlopeDisc.Val.Temp<-control.orig$Select_Params_TV$BLK_Params[1,3]
        }else{
          tempCont<-control.orig$Select_Params[1:lcSlopeDisc.temp,,drop=FALSE]
          tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
          numTVSlopeDisc.temp<-length(tempCont[,13])
          rowsTVSlopeDisc<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVSlopeDisc.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          SlopeDisc.Val.Temp<-rowsTVSlopeDisc[1,3]
        }
      }else if(control.orig$Select_Params[lcSlopeDisc.temp,13]>0){
        if(control.orig$BlockPatterns[[control.orig$Select_Params[lcSlopeDisc.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcSlopeDisc.temp,13]]]<data.orig$endyr){
          SlopeDisc.Val.Temp<-control.orig$Select_Params[lcSlopeDisc.temp,3]
        }else{
          if(control.orig$Select_Params_TV$Cust_Blk==0){
            rowsTVSlopeDisc<-control.orig$Select_Params_TV$BLK_Params
          }else{
            tempCont<-control.orig$Select_Params[1:lcSlopeDisc.temp,,drop=FALSE]
            tempCont<-tempCont[tempCont[,13]!=0,,drop=FALSE]
            numTVSlopeDisc.temp<-length(tempCont[,13])
            rowsTVSlopeDisc<-control.orig$Select_Params_TV$BLK_Params[grep(paste0("TV_Param ",numTVSlopeDisc.temp," _Block "),row.names(control.orig$Select_Params_TV$BLK_Params)),,drop=FALSE]
          }
          if(control.orig$Select_Params[lcSlopeDisc.temp,14]==0){
            SlopeDisc.Val.Temp<-control.orig$Select_Params[lcSlopeDisc.temp,13]*exp(rowsTVSlopeDisc[length(rowsTVSlopeDisc[,1]),3])
          }else if(control.orig$Select_Params[lcSlopeDisc.temp,14]==1){
            SlopeDisc.Val.Temp<-control.orig$Select_Params[lcSlopeDisc.temp,13]+rowsTVSlopeDisc[length(rowsTVSlopeDisc[,1]),3]
          }else if(control.orig$Select_Params[lcSlopeDisc.temp,14]==2){
            SlopeDisc.Val.Temp<-rowsTVSlopeDisc[length(rowsTVSlopeDisc[,1]),3]
          }else if(control.orig$Select_Params[lcSlopeDisc.temp,14]==3){
            SlopeDisc.Val.Temp<-control.orig$Select_Params[lcSlopeDisc.temp,13]+sum(rowsTVSlopeDisc[,3])
          }
        }
      }
      SlopeDisc.Val[i]<<-SlopeDisc.Val.Temp

      temp1<-as.matrix(age.selex[i,3:length(age.selex[1,])])
      temp2<-t(t(ALK)*temp1[1,])
      temp3<-(apply(temp2,1,sum)/apply(ALK,1,sum))[length(ALK[,1]):1]/max(apply(temp2,1,sum)/apply(ALK,1,sum))
      comb.selex[i,3:length(size.selex[1,])]<<-temp3*as.matrix(size.selex)[i,3:length(size.selex[1,])]/max(temp3*as.matrix(size.selex)[i,3:length(size.selex[1,])])

      local({
        my_i<-i
        id<-paste0("fleet",my_i,"selexPloting")
        location<-paste0("fleet",my_i,"selexPlotPanel")
        newPlot(location=location,id=id)
        main=paste0(fleetNames[my_i]," Selectivity")
        xlab=paste0("Length(",lengthNames[disp.Desc$Units[2]],")")
        ylab='Selectivity'
        xlim=c(0,ceiling(as.numeric(names(comb.selex[1,])[length(comb.selex[1,])])*lengthScale[disp.Desc$Units[2]]))
        ylim=c(0,1.1)

        eval(parse(text=paste0("
                               observerPool[[(17+my_i)]]<<-observeEvent(input$trig1,ignoreInit=TRUE,{
                               observe({
                               withProgress(message='Creating selectivity plots',value=0.5,{
                               #print('triggered oberserver 10')
                               req(input$fleet",my_i,"Lc,input$fleet",my_i,"Lcslope,input$fleet",my_i,"retentionMaxSlider,input$fleet",my_i,"discMortSlider)
                               # if(!is.null(input$fleet",my_i,"Lc)){
                               #if(!is.null(input$fleet",my_i,"Lcslope)){
                               #if(!is.null(input$fleet",my_i,"retentionMaxSlider)){
                               #if(!is.null(input$fleet",my_i,"discMortSlider)){

                               #print('Observer 10 Tried to print selection plot')
                               temp.size.mort<-size.mort
                               temp.Lc<-(input$fleet",my_i,"Lc)/lengthScale[disp.Desc$Units[2]]
                               temp.LcSlope<-as.numeric(input$fleet",my_i,"Lcslope)
                               temp.LcMaxRet<-input$fleet",my_i,"retentionMaxSlider
                               temp.DiscMortMax<-input$fleet",my_i,"discMortSlider

                               comb.Lc[my_i,3:length(comb.Lc[1,])]<<-temp.LcMaxRet/(1+exp(-(as.numeric(names(comb.Lc[1,3:length(comb.Lc[1,])]))-temp.Lc)/temp.LcSlope))
                               temp.size.mort[my_i,3:length(comb.Lc[1,])]<-temp.DiscMortMax/(1+exp(-(as.numeric(names(comb.Lc[1,3:length(comb.Lc[1,])]))-InflDisc.Val[my_i])/SlopeDisc.Val[my_i]))


                               comb.retain[my_i,3:length(size.selex[1,])]<<-comb.selex[my_i,3:length(size.selex[1,])]*as.matrix(comb.Lc)[my_i,3:length(size.selex[1,])]
                               comb.dead[my_i,3:length(size.selex[1,])]<<-comb.selex[my_i,3:length(size.selex[1,])]*(1-as.matrix(comb.Lc)[my_i,3:length(size.selex[1,])])*as.matrix(temp.size.mort)[my_i,3:length(size.selex[1,])]

                               output$",id,"<-renderPlot({
                               plot(NA,main=main,xlab=xlab,ylab=ylab,xlim=xlim,ylim=ylim)
                               temp.x<-c(as.numeric(names(comb.selex[1,3:length(comb.selex[1,])]))*lengthScale[disp.Desc$Units[2]])
                               temp.x<-temp.x[c(1,1:length(temp.x),length(temp.x))]
                               text(x=xlim[1],y=c(1.1),labels=c('Selected'),col=c('dark blue'),pos=4)
                               text(x=mean(xlim),y=c(1.1),labels=c('Retained'),col=c('dark green'))
                               text(x=xlim[2],y=c(1.1),labels=c('Dead Discards'),col=c('dark red'),pos=2)
                               polygon(y=c(0,comb.selex[my_i,c(3:length(comb.selex[my_i,]))],0),x=temp.x,col=","'dark blue'",")
                               polygon(y=c(0,comb.retain[my_i,c(3:length(comb.selex[my_i,]))]+comb.dead[my_i,c(3:length(comb.selex[my_i,]))],0),x=temp.x,col=","'dark red'",")
                               polygon(y=c(0,comb.retain[my_i,c(3:length(comb.selex[my_i,]))],0),x=temp.x,col=","'dark green'",")
                               })


                               })

                               #}
                               #}
                               #}
                               #}
                               })
                               })",collapse = "")))
          })
      }
    }
  }
