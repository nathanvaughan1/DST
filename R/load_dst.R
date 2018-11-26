#' load DST
#'
#' Loads all the base observers and interface
#' of the shiny dst.
#'
#' @param app_objects a list of global parameters maintained in the app
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
load_dst <- function(input,output,session,app_objects){

insertUI(
  selector = "#GlobalApp",
  where = "beforeEnd",
  ui = {
    navbarPage(title=tags$div(img(src=paste0("NOAAfish.png"),height=1.06*47,width=1.06*210,style="margin:-15px -15px -0px -15px")),
               id="mainNavBar",
               #Initialize all tabs in navbarPage
               #
               initSelectAssessment(),
               initImplementManagement(),
               initQuotaForecast(),
               useShinyjs(),
               tags$style(type="text/css", ".recalculating {opacity: 1.0;}")#,
               #bsTooltip("chooseAssessment","This is a dropdown menu from which to select the assesment you wish to investigate"),
               #bsTooltip("selectAssessmentTitle","This is a dropdown menu from which to select the assesment you wish to investigate")
    )
  }
)

#Construct initial tabs
observeEvent(input$ForecastApplied,{
  initKobeChoices(input, output, session)
})

#Watch for the selection of a new assessment and rebuild decision tabs
#Wait for a new assessment to be selected
observeEvent(input$chooseAssessment,{
  #print("running observer 23")

  if(length(observerPool)>0){
    for(i in 1:length(observerPool)){
      if(typeof(observerPool[[i]])=="environment"){
        observerPool[[i]]$destroy()
      }
    }
  }

  ##Reset all tracked variable values to defaults
  observerPool<<-as.list(rep(0,50))
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
  aggreg.timeSeries.F<<-0
  aggreg.timeSeries.DB<<-0
  aggreg.timeSeries.DN<<-0
  aggreg.timeSeries.RB<<-0
  aggreg.timeSeries.RN<<-0
  prevGroupAlloc<<-0
  prevTotalAlloc<<-0
  sliderResolution<<-6
  weightNames<<-c("MT","1000's Lbs")
  weightScale<<-c(1,2.20462)
  lengthNames<<-c("cm","Inches")
  lengthScale<<-c(1,0.393701)
  baseCatch<<-0
  newCatch<<-0
  msyCatch<<-0
  projCatch<<-0
  groups<<-0
  areaNames<<-0
  seasonNames<<-0
  fleetNames<<-0
  groupNames<<-0
  numFutCatchYrs<<-3
  sleepTime<<-3
  HasRun<<-rep(0,25)
  managementLimits<<-0
  InflDisc.Val<<-0
  SlopeDisc.Val<<-0
  triggers<<-list()
  targetValue<<-0
  tabTitles<<-0
  tabValues<<-0
  showInitTab<<-FALSE
  targetNames<<-c("SPR","Spawning Biomass Ratio","Maximize Catch","Use previous estimate")
  targetNames2<<-c("SPR Target Value","Ignore ","Spawning Biomass Ratio")
  targetNames3<<-c("Implement Optimal","Fixed Fraction Fmsy","Constant Catch")

  ##Identify if assessment has been selected yet or if user wants to upload new assessment files

  if(identical(input$chooseAssessment,"BL")){
    ##No assessment selected yet
    initAssessments(input, output, session)
    hideElement(id="UploadInterface")
    hideElement(id="QuotaPanel")
    hideElement(id="ImplementManagement")
  }else if(identical(input$chooseAssessment,"DNU")){
    ##No assessment selected yet
    hideElement(id="UploadInterface")
    hideElement(id="QuotaPanel")
    hideElement(id="ImplementManagement")
  }else if(identical(input$chooseAssessment,"UN")){
    ##User wants to upload new assessment show upload UI
    showElement(id="UploadInterface")
    hideElement(id="QuotaPanel")
    hideElement(id="ImplementManagement")
  }else{
    ##User wants to run an existing assessment build assessment specific UI
    withProgress(message="Assessment selected: Building interface now",value=0.1,{
      hideElement(id="chooseAssessment")
      showElement(id="trigA")
      ##First remove any existing UI from a previous assessment
      removeUI("#currSizeLimits>*",multiple=TRUE,immediate=TRUE)
      removeUI("#currSizeLimits",multiple=TRUE,immediate=TRUE)
      removeUI("#QuotaPanel>*",multiple=TRUE,immediate=TRUE)
      removeUI("#QuotaPanel",multiple=TRUE,immediate=TRUE)
      removeUI("#currOutput>*",multiple=TRUE,immediate=TRUE)
      removeUI("#currOutput",multiple=TRUE,immediate=TRUE)

      newDiv("ImplementManagement","currSizeLimits")
      incProgress(0.1, detail = "Select new assessment")
      runSelectAssessment(input, output, session)

      incProgress(0.1, detail = "Reading management data")
      readManagementData(input,output,session)

      incProgress(0.1, detail = "Building Forcast interface")
      buildForecastChoices(input, output, session)

      incProgress(0.1, detail = "Building Management interface")
      observerPool[[1]]<<-observe({
        #print("running oberserver 24")

        if(!is.null(input$RebuildTargetYear)){
          withProgress(message="Building Management Tab Panels",value=0.5,{
            buildManagementTabPanels(input, output, session)
            #print("observer 24 ran")
            observerPool[[1]]$destroy()
            observerPool[[2]]<<-observe({
              #print("running oberserver 25")

              if(!is.null(input$ManagementTabset)){
                withProgress(message="Building Management Tab Elements",value=0.1,{
                  #print("observer 25 ran")
                  buildTabElements(input, output, session)
                  observerPool[[2]]$destroy()
                }
                )

                updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
                #observeEvent(input$mainNavBar,{
                if(length(tabValues)>1){
                  for(i in length(tabValues):1){
                    updateTabsetPanel(session=session, inputId = "ManagementTabset",selected = tabValues[i])
                  }}
                updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")

                observerPool[[20]]<<-observe({


                  if(isolate({showInitTab==TRUE})){
                    isolate({showElement(id="trig1")
                      click(id="trig1")
                      hideElement(id="trigB")
                      observerPool[[20]]<<-NULL
                    })}else{
                      isolate({
                        showInitTab<<-TRUE
                        showElement(id="trigB")
                        hideElement(id="trigA")
                      })
                      invalidateLater(5,session=session)
                    }


                })

                updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
                updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
                #})
              }
            })
          })
        }
      })
    })
    hideElement(id="UploadInterface")
    showElement(id="QuotaPanel")
    showElement(id="ImplementManagement")
  }
})

observeEvent(input$updateAssessment,{
  #print("running oberserver 27")
  runUpdateAssessment2(input, output, session)

  updateNavbarPage(session=session, inputId = "mainNavBar",selected="Forecast Results")

  buildObservers(input, output, session,1)

  buildObservers(input, output, session,2)

  buildObservers(input, output, session,3)

  buildObservers(input, output, session,4)

  buildObservers(input, output, session,5)

})

observeEvent(input$createNewAssess,{
  #print("running oberserver 28")
  if(!is.null(input$readStarter) & !is.null(input$readData) & !is.null(input$readControl)){
    starter.new<<-rd_starter(input$readStarter$datapath)
    data.new<<-rd_data(file=input$readData$datapath)
    control.new<<-rd_ctl(file=input$readControl$datapath,data.new)

    if(!is.null(input$readForecast)){
      forecast.new<<-rd_forecast(file=input$readForecast$datapath,Nfleets=data.new$Nfleet,Nareas = data.new$N_areas,Nseas = data.new$nseas)
      forecast.new$Nforecastyrs <<- 100
      for(j in 1:6){
        if(forecast.new$Bmark_years[j]<=0){
          forecast.new$Bmark_years[j]<<-data.new$endyr+forecast.new$Bmark_years[j]
        }
      }
      for(j in 1:4){
        if(forecast.new$Fcast_years[j]<=0){
          forecast.new$Fcast_years[j]<<-data.new$endyr+forecast.new$Fcast_years[j]
        }
      }
    }else{
      forecast.new<<-list()
      forecast.new$sourcefile <<- "forecast.ss"
      forecast.new$type <<- "Stock_Synthesis_forecast_file"
      forecast.new$SSversion <<- "SSv3.21_or_later"
      forecast.new$benchmarks <<- 1
      forecast.new$MSY <<- 3
      forecast.new$SPRtarget <<- 0.3
      forecast.new$Btarget <<- 0.3
      forecast.new$Bmark_years <<- rep(data.new$endyr,6)
      forecast.new$Bmark_relF_Basis <<- 2
      forecast.new$Forecast <<- 3
      forecast.new$Nforecastyrs <<- 100
      forecast.new$F_scalar <<- 1
      forecast.new$Fcast_years <<- rep(data.new$endyr,4)
      forecast.new$ControlRuleMethod <<- 2
      forecast.new$BforconstantF <<- 0.01
      forecast.new$BfornoF <<- 0.001
      forecast.new$Flimitfraction <<- 1
      forecast.new$N_forecast_loops <<- 3
      forecast.new$First_forecast_loop_with_stochastic_recruitment <<- 3
      forecast.new$Forecast_loop_control_3 <<- 0
      forecast.new$Forecast_loop_control_4 <<- 0
      forecast.new$Forecast_loop_control_5 <<- 0
      forecast.new$FirstYear_for_caps_and_allocations <<- (as.integer(format(Sys.Date(), "%Y"))+1)
      forecast.new$stddev_of_log_catch_ratio <<- 0
      forecast.new$Do_West_Coast_gfish_rebuilder_output <<- 0
      forecast.new$Ydecl <<- -1
      forecast.new$Yinit <<- -1
      forecast.new$fleet_relative_F <<- 1
      forecast.new$basis_for_fcast_catch_tuning <<- 3
      forecast.new$max_totalcatch_by_fleet <<- rep(-1,data.new$Nfleet)
      forecast.new$max_totalcatch_by_area <<- rep(-1,data.new$N_areas)
      forecast.new$fleet_assignment_to_allocation_group <<- rep(0,data.new$Nfleet)
      forecast.new$N_allocation_groups <<- 0
      forecast.new$Ncatch <<- 0
      forecast.new$InputBasis <<- -1
    }

    if(!is.null(input$readParameter)){
      pars.new<<-rd_par(file=input$readParameter$datapath)
      starter.new$init_values_src<<-1
      starter.new$last_estimation_phase<<-0

      rowsRecDev<-grep("# recdev1",pars.new$Labels)
      rowsFirstFinit<-grep("# init_F",pars.new$Labels)[1]
    }else{
      pars.new<<-NULL
      starter.new$init_values_src<<-0
      starter.new$last_estimation_phase<<-10
    }

    if(!is.null(input$readDisplay)){
      disp.Desc<<-rd_display(file=input$readDisplay$datapath,forecast.new,data.new)
    }else{
      disp.Desc<<-list()
      disp.Desc$type<<-"DST_Display_Descriptions"
      disp.Desc$AssesDesc<<-"This is a automated default display file modify this description"
      disp.Desc$Title<<-"Automated Stock add a name here"

      disp.Desc$GroupNames<<-matrix(NA,nrow=length(unique(forecast.new$fleet_assignment_to_allocation_group)),ncol=3)
      for(i in 1:length(unique(forecast.new$fleet_assignment_to_allocation_group)))
      {
        disp.Desc$GroupNames[i,1]<<-as.numeric(unique(forecast.new$fleet_assignment_to_allocation_group)[i])
        disp.Desc$GroupNames[i,2]<<-paste0("Group ",unique(forecast.new$fleet_assignment_to_allocation_group)[i])
        disp.Desc$GroupNames[i,3]<<-paste0("Automated description for group ",unique(forecast.new$fleet_assignment_to_allocation_group)[i])
      }

      disp.Desc$FleetNames<<-matrix(NA,nrow=length(forecast.new$fleet_assignment_to_allocation_group),ncol=3)
      for(i in 1:length(forecast.new$fleet_assignment_to_allocation_group))
      {
        disp.Desc$FleetNames[i,1]<<-as.numeric(i)
        disp.Desc$FleetNames[i,2]<<-data.new$fleetnames[i]
        disp.Desc$FleetNames[i,3]<<-paste0("Automated description for ",data.new$fleetnames[i])
      }

      disp.Desc$SeasonNames<<-matrix(NA,nrow=data.new$nseas,ncol=3)
      for(i in 1:data.new$nseas)
      {
        disp.Desc$SeasonNames[i,1]<<-i
        disp.Desc$SeasonNames[i,2]<<-paste0(month.abb[(sum(data.new$months_per_seas[-c(i:length(data.new$months_per_seas))])+1)],"-",month.abb[(sum(data.new$months_per_seas[-c((i+1):length(data.new$months_per_seas))]))])
        disp.Desc$SeasonNames[i,3]<<-paste0("Automated description for season ",i," spaning the months ",month.abb[(sum(data.new$months_per_seas[-c(i:length(data.new$months_per_seas))])+1)],"-",month.abb[(sum(data.new$months_per_seas[-c((i+1):length(data.new$months_per_seas))]))])
      }

      disp.Desc$AreaNames<<-matrix(NA,nrow=data.new$N_areas,ncol=3)
      for(i in 1:data.new$N_areas)
      {
        disp.Desc$AreaNames[i,1]<<-as.numeric(i)
        disp.Desc$AreaNames[i,2]<<-paste0("Area ",i)
        disp.Desc$AreaNames[i,3]<<-paste0("Automated description of area ",i)
      }

      disp.Desc$TargetYears<<-vector(length=2)
      disp.Desc$TargetYears[1]<<-(data.new$endyr+90)
      disp.Desc$TargetYears[2]<<-(data.new$endyr+100)
      disp.Desc$ABCFrac<<-1
      disp.Desc$FleetRel<<-1
      disp.Desc$ImplRebuild<<-2
      disp.Desc$RebuildYears<<-5
      disp.Desc$RebuildFrac<<-1
      disp.Desc$ConstCatch<<-10000
      disp.Desc$Units<<-vector(length=2)
      disp.Desc$Units[1]<<-1
      disp.Desc$Units[2]<<-1
    }
    withProgress(message="Creating Base Assessment",value=0.2,{

      dir.base<<-paste0(getwd(),"/Assessments/")
      nameIter<-1
      iterName<-paste0(input$newAssessName)
      if(!dir.exists(paste0(dir.base,input$newAssessName))){
        dir.base<<-paste0(dir.base,input$newAssessName)
        dir.create(dir.base)
      }else{
        iterName<-paste0(input$newAssessName,nameIter)
        while(dir.exists(paste0(dir.base,iterName))){
          nameIter<-nameIter+1
          iterName<-paste0(input$newAssessName,nameIter)
        }
        dir.base<<-paste0(dir.base,iterName)
        dir.create(dir.base)
      }
      dir.orig<<-paste0(dir.base,"/original")
      dir.create(dir.orig)
      incProgress(0.1, detail = "Writing files")
      wrt_starter(starter.new,dir.orig,overwrite = TRUE,warn=FALSE)
      wrt_data(data.new,outfile=paste0(dir.orig,"/",starter.new$datfile),overwrite = TRUE)
      wrt_ctl(file=paste0(dir.orig,"/",starter.new$ctlfile),data.new,control.new)
      if(!is.null(pars.new)){wrt_par(file=paste(dir.orig,"/ss3.par",sep=""),pars.new)}
      wrt_forecast(forecast.new,dir=dir.orig,overwrite = TRUE)
      wrt_display(disp.Desc,file=paste0(dir.orig,"/Display_Descriptions.txt"))
      file.copy(paste0(getwd(),"/www/ss3.exe"),dir.orig)
      incProgress(0.1, detail = "Running SS3")
      if(starter.new$init_values_src==1){
        shell(paste("cd /d ",dir.orig," && ss3 -nohess",sep=""))
      }else{
        shell(paste("cd /d ",dir.orig," && ss3",sep=""))
      }

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
      pars.new<-rd_par(paste0(dir.orig,"/ss3.par"))

    })
    withProgress(message="Preparing assessment files, building fleet selectivity options and confirming targets attained ",value=0.2,{
      prepareAssessment(input,output,session,output.orig,starter.new,data.new,control.new,forecast.new,pars.new,disp.Desc,dir.orig)
    })
    initAssessments(input, output, session)
    updateSelectInput(session,"chooseAssessment",selected = dir.base)


  }else{
    modalUIstring1<-modalUIstring2<-modalUIstring3<-modalUIstring<-""
    if(is.null(input$readStarter)){
      modalUIstring1<-"Starter"
    }
    if(is.null(input$readData) & is.null(input$readControl)){
      modalUIstring2<-"Data"
    }
    if(is.null(input$readControl)){
      modalUIstring3<-"Control"
    }
    modalUIstring<-paste0("Don't press the Build New Assessment button until you have uploaded at least starter, data, and control files you are still missing [",modalUIstring1,",",modalUIstring2,",",modalUIstring3,"]")
    showModal(modalDialog(modalUIstring,title="Not all required input files have been loaded"))
    #toggleModal(session=session,modalId = "missingFiles",toggle="open")
  }
})

observeEvent(input$trig1,ignoreInit = TRUE,{
  hideElement(id="trig1")
  withProgress(message="Initializing Interface",value=0.1,{
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    if(length(tabValues)>1){
      for(i in length(tabValues):1){
        updateTabsetPanel(session=session, inputId = "ManagementTabset",selected = tabValues[i])
      }
    }
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,15)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,16)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,17)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,8)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,9)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,11)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,12)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,13)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,14)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    buildObservers(input, output, session,18)
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Select Assessment")
    updateNavbarPage(session=session, inputId = "mainNavBar",selected="Management Action")
    incProgress(0.1, detail = "Building base results interface")
    buildBaseQuota(input,output,session)
    hideElement(id="trig1")
    showElement(id="chooseAssessment")
  })
})

observeEvent(input$resetVals,{
  #print("running oberserver 29")
  resetForeVals(input, output, session)
  resetTabElements(input, output, session)
})

}
