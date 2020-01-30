#' Reset forecast parameter values
#'
#' Resets the forecast values of the shiny interface
#' to their default values
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
resetForeVals<-function(input,output,session){
  targetNames<-c("SPR","Spawning Biomass Ratio","Maximize Catch","Use previous estimate")
  targetNames2<-c("SPR Target Value","Ignore ","Spawning Biomass Ratio")
  targetNames3<-c("Implement Optimal","Fixed Fraction Fmsy","Constant Catch")
  targetValue<-c(forecast.orig$SPRtarget,1,forecast.orig$Btarget)

  updateSliderInput(session,"ManagementYearInput",value=(as.integer(format(Sys.Date(), "%Y"))+1))
  updateRadioButtons(session,"ForecastTarget",selected=forecast.orig$MSY)
  updateSliderInput(session,"TargetValue",value=targetValue[forecast.orig$MSY])
  updateSliderInput(session,"TargetYears",value=disp.Desc$TargetYears)
  updateRadioButtons(session,"ForecastApplied",selected=1)
  updateRadioButtons(session,"Rebuild",selected=disp.Desc$ImplRebuild)
  updateSliderInput(session,"ABCfrac",value=disp.Desc$ABCFrac)
  updateTextInput(session,"ConstCatchInput",value=disp.Desc$ConstCatch)
  updateSliderInput(session,"ConstCatchfrac",value=disp.Desc$ABCFrac)
  updateTextInput(session,"ConstCatchRebuild",value="")
  updateSliderInput(session,"RebuildTargetFraction",value=disp.Desc$RebuildFrac)
  updateSliderInput(session,"RebuildTargetYear",value=(as.integer(format(Sys.Date(), "%Y"))+disp.Desc$RebuildYears))
}

#' Reset tab values
#'
#' Resets the tab element values of the shiny interface
#' to their default values
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
resetTabElements<-function(input,output,session){

  updateRadioButtons(session,"GroupAllocUnits",selected=forecast.orig$basis_for_fcast_catch_tuning)
  updateRadioButtons(session,"displaySeason",selected=1)
  updateSliderInput(session,"RelAllocYears",value=c(forecast.orig$Fcast_years[3],forecast.orig$Fcast_years[4]))
  withProgress(message="Building management tab content",value=0,{
    for(j in 1:length(groups))
    {
      if(groups[j]!=0)
      {
        temp.max.group[j]<<-1
        temp.val.group[j]<<-forecast.orig$allocation_among_groups[j]
        updateSliderInput(session,paste0("Group",j,"AllocationSlide"),value=temp.val.group[j])
        updateCheckboxInput(session,paste0("Group",j,"AllocationFixed"),value=FALSE)
      }
      updateRadioButtons(session,paste0("Group",j,"FleetAllocUnits"),selected=2)

      prevGroupAlloc[j]<<-2
      groupfleets<-which(forecast.orig$fleet_assignment_to_allocation_group==groups[j])
      for(i in groupfleets)
      {
        currfleet<-which(groupfleets==i)
        fleets<-groupfleets[-c(currfleet:length(groupfleets))]
        temp.max.fleet[i]<<-1
        if(sum(rel_F_orig[,groupfleets])==0){
          temp.val.fleet[i]<<-0
        }else{
          temp.val.fleet[i]<<-sum(rel_F_orig[,i])/sum(rel_F_orig[,groupfleets])
        }
        updateSliderInput(session,paste0("Fleet",i,"AllocationSlide"),value=temp.val.fleet[i])
        updateCheckboxInput(session,paste0("Fleet",i,"AllocationFixed"),value=FALSE)
        for(k in 1:data.orig$nseas)
        {
          temp.max.fleet.seas[k,i]<<-1
          temp.val.fleet.seas[k,i]<<-rel_F_orig[k,i]/sum(rel_F_orig[,groupfleets])
          updateSliderInput(session,paste0("Fleet",i,"Seas",k,"AllocationSlide"),value=temp.val.fleet.seas[k,i])
          updateCheckboxInput(session,paste0("Fleet",i,"Seas",k,"AllocationFixed"),value=FALSE)
        }

        fleetData<-forecast.orig$ForeCatch[forecast.orig$ForeCatch[,3]==i,,drop=FALSE]
        CatchCol<-5
        if(length(fleetData[,1])==0)
        {
          if(data.orig$units_of_catch[i]==1)
          {
            CatchCol<-5
          }else if (data.orig$units_of_catch[i]==2){
            CatchCol<-7
          }
        }else{
          if(fleetData[1,5]==2)
          {
            fleetData[,4]<-fleetData[,4]*weightScale[disp.Desc$Units[1]]#2.20462
            CatchCol<-5:6
          }else if (fleetData[1,5]==5){
            CatchCol<-7:8

          }else if(fleetData[1,5]==3)
          {
            fleetData[,4]<-fleetData[,4]*weightScale[disp.Desc$Units[1]]#2.20462
            CatchCol<-5
          }else if (fleetData[1,5]==6){
            CatchCol<-7
          }else if(fleetData[1,5]==99)
          {
            CatchCol<-9
          }
        }

        for(hh in 1:((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs)){
          newRows<-seq(1,((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs),3)
          currYr<-(data.orig$endyr+hh)
          if(hh==((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs))
          {
            CatchHistYr<-fleetData[fleetData[,1]==currYr,,drop=FALSE]
            if(length(CatchHistYr[,1])==data.orig$nseas)
            {
              updateTextInput(session,paste0("fleet",i,"recentCatchQuantAll"),value=paste0(sum(CatchHistYr[,4])))
            }else{
              updateTextInput(session,paste0("fleet",i,"recentCatchQuantAll"),value=paste0(""))
            }
          }else{
            CatchHistYr<-fleetData[fleetData[,1]==currYr,,drop=FALSE]
            if(length(CatchHistYr[,1])==data.orig$nseas)
            {
              updateTextInput(session,paste0("fleet",i,"recentCatchQuant",hh),value=paste0(sum(CatchHistYr[,4])))
            }else{
              if(currYr<(as.integer(format(Sys.Date(), "%Y"))+1)){
                updateTextInput(session,paste0("fleet",i,"recentCatchQuant",hh),value=paste0(sum(projCatch[projCatch$year==currYr & projCatch$fleet==i,CatchCol])))
              }else{
                updateTextInput(session,paste0("fleet",i,"recentCatchQuant",hh),value=paste0(""))
              }
            }
          }
        }

        for(k in 1:data.orig$nseas){
          for(hh in 1:((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs)){
            newRows<-seq(1,((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs),3)
            currYr<-(data.orig$endyr+hh)
            if(hh==((as.integer(format(Sys.Date(), "%Y"))+1)-data.orig$endyr+numFutCatchYrs))
            {
              CatchHistYrSeas<-fleetData[fleetData[,1]==currYr & fleetData[,2]==k,,drop=FALSE]
              if(length(CatchHistYrSeas[,1])==1)
              {
                updateTextInput(session,paste0("fleet",i,"seas",k,"recentCatchQuantAll"),value=paste0(sum(CatchHistYrSeas[,4])))
              }else{
                updateTextInput(session,paste0("fleet",i,"seas",k,"recentCatchQuantAll"),value=paste0(""))
              }
            }else{
              CatchHistYrSeas<-fleetData[fleetData[,1]==currYr & fleetData[,2]==k,,drop=FALSE]
              if(length(CatchHistYrSeas[,1])==1)
              {
                updateTextInput(session,paste0("fleet",i,"seas",k,"recentCatchQuant",hh),value=paste0(sum(CatchHistYrSeas[,4])))
              }else{
                if(currYr<(as.integer(format(Sys.Date(), "%Y"))+1)){
                  updateTextInput(session,paste0("fleet",i,"seas",k,"recentCatchQuant",hh),value=paste0(sum(projCatch[projCatch$year==currYr & projCatch$fleet==i & projCatch$season==k,CatchCol])))
                }else{
                  updateTextInput(session,paste0("fleet",i,"seas",k,"recentCatchQuant",hh),value=paste0(""))
                }
              }
            }
          }
        }

        lcRow.temp<-grep(paste0("Size Retention  ",1,"  fleet/Survey  ",i),row.names(control.orig$Select_Params))[1]
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
          if(control.orig$BlockPatterns[[control.orig$Select_Params[lcRow.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcRow.temp,13]]]<data.orig$endyr)
          {
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
            if(control.orig$Select_Params[lcRow.temp,14]==0)
            {
              Lc.Val.Temp<-control.orig$Select_Params[lcRow.temp,13]*exp(rowsTVLc[length(rowsTVLc[,1]),3])
            }else if(control.orig$Select_Params[lcRow.temp,14]==1)
            {
              Lc.Val.Temp<-control.orig$Select_Params[lcRow.temp,13]+rowsTVLc[length(rowsTVLc[,1]),3]
            }else if(control.orig$Select_Params[lcRow.temp,14]==2)
            {
              Lc.Val.Temp<-rowsTVLc[length(rowsTVLc[,1]),3]
            }else if(control.orig$Select_Params[lcRow.temp,14]==3)
            {
              Lc.Val.Temp<-control.orig$Select_Params[lcRow.temp,13]+sum(rowsTVLc[,3])
            }
          }
        }
        maxSize<-100
        if(data.orig$lbin_method==1)
        {
          maxSize<-max(data.orig$lbin_vector)
        }else if(data.orig$lbin_method==2)
        {
          maxSize<-data.orig$maximum_size
        }else if(data.orig$lbin_method==3)
        {
          maxSize<-max(data.orig$lbin_vector_pop)
        }

        updateSliderInput(session,paste0("fleet",i,"Lc"),value=Lc.Val.Temp*lengthScale[disp.Desc$Units[2]])

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
          if(control.orig$BlockPatterns[[control.orig$Select_Params[lcSlope.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcSlope.temp,13]]]<data.orig$endyr)
          {
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
            if(control.orig$Select_Params[lcSlope.temp,14]==0)
            {
              Slope.Val.Temp<-control.orig$Select_Params[lcSlope.temp,13]*exp(rowsTVSlope[length(rowsTVSlope[,1]),3])
            }else if(control.orig$Select_Params[lcSlope.temp,14]==1)
            {
              Slope.Val.Temp<-control.orig$Select_Params[lcSlope.temp,13]+rowsTVSlope[length(rowsTVSlope[,1]),3]
            }else if(control.orig$Select_Params[lcSlope.temp,14]==2)
            {
              Slope.Val.Temp<-rowsTVSlope[length(rowsTVSlope[,1]),3]
            }else if(control.orig$Select_Params[lcSlope.temp,14]==3)
            {
              Slope.Val.Temp<-control.orig$Select_Params[lcSlope.temp,13]+sum(rowsTVSlope[,3])
            }
          }
        }
        if(Slope.Val.Temp>=0)
        {
          vals.temp<-unique(c(sort(c(Slope.Val.Temp,0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20,50,100,200,500,1000,2000)),Inf,-2000,-1000,-500,-200,-100,-50,-20,-10,-5,-2,-1,-0.5,-0.2,-0.1,-0.05,-0.02,-0.01))
        }else{
          vals.temp<-unique(c(0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20,50,100,200,500,1000,2000,Inf,sort(c(Slope.Val.Temp,-2000,-1000,-500,-200,-100,-50,-20,-10,-5,-2,-1,-0.5,-0.2,-0.1,-0.05,-0.02,-0.01))))
        }
        updateSliderInput(session,paste0("fleet",i,"Lcslope"),value=Slope.Val.Temp)

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
          if(control.orig$BlockPatterns[[control.orig$Select_Params[lcMaxRet.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcMaxRet.temp,13]]]<data.orig$endyr)
          {
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
            if(control.orig$Select_Params[lcMaxRet.temp,14]==0)
            {
              MaxRet.Val.Temp<-control.orig$Select_Params[lcMaxRet.temp,13]*exp(rowsTVMaxRet[length(rowsTVMaxRet[,1]),3])
            }else if(control.orig$Select_Params[lcMaxRet.temp,14]==1)
            {
              MaxRet.Val.Temp<-control.orig$Select_Params[lcMaxRet.temp,13]+rowsTVMaxRet[length(rowsTVMaxRet[,1]),3]
            }else if(control.orig$Select_Params[lcMaxRet.temp,14]==2)
            {
              MaxRet.Val.Temp<-rowsTVMaxRet[length(rowsTVMaxRet[,1]),3]
            }else if(control.orig$Select_Params[lcMaxRet.temp,14]==3)
            {
              MaxRet.Val.Temp<-control.orig$Select_Params[lcMaxRet.temp,13]+sum(rowsTVMaxRet[,3])
            }
          }
        }
        updateSliderInput(session,paste0("fleet",i,"retentionMaxSlider"),value=MaxRet.Val.Temp)

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
          if(control.orig$BlockPatterns[[control.orig$Select_Params[lcMaxDisc.temp,13]]][2*control.orig$BlocksPerPattern[control.orig$Select_Params[lcMaxDisc.temp,13]]]<data.orig$endyr)
          {
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
            if(control.orig$Select_Params[lcMaxDisc.temp,14]==0)
            {
              MaxDisc.Val.Temp<-control.orig$Select_Params[lcMaxDisc.temp,13]*exp(rowsTVMaxDisc[length(rowsTVMaxDisc[,1]),3])
            }else if(control.orig$Select_Params[lcMaxDisc.temp,14]==1)
            {
              MaxDisc.Val.Temp<-control.orig$Select_Params[lcMaxDisc.temp,13]+rowsTVMaxDisc[length(rowsTVMaxDisc[,1]),3]
            }else if(control.orig$Select_Params[lcMaxDisc.temp,14]==2)
            {
              MaxDisc.Val.Temp<-rowsTVMaxDisc[length(rowsTVMaxDisc[,1]),3]
            }else if(control.orig$Select_Params[lcMaxDisc.temp,14]==3)
            {
              MaxDisc.Val.Temp<-control.orig$Select_Params[lcMaxDisc.temp,13]+sum(rowsTVMaxDisc[,3])
            }
          }
        }
        updateSliderInput(session,paste0("fleet",i,"discMortSlider"),value=MaxDisc.Val.Temp)

      }
    }
  })
}

#' Build observers
#'
#' All of the observers needed by the shiny interface
#' each can be independently initiated
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @param observers specifies which observer to initiate
#' @author Nathan Vaughan
#' @keywords interface management
buildObservers<-function(input, output, session, observers=0){
  ##This observer makes sure that only the correct result comparison options are available for plotting (i.e. compare new to base results)
  if(is.element(1,observers)){
    observerPool[[4]]<<-observe(priority=1,{
      withProgress(message="Updating results radio options 1",value=0.5,{
        #print("running observer 1")
        if(
          !is.null(input$displayRadioButtonOut1)&
          !is.null(input$displayRadioButtonOut7) &
          !is.null(input$displayRadioButtonOut8)){
          if(input$displayRadioButtonOut1=="Base"){
            updateRadioButtons(session = session, inputId = "displayRadioButtonOut7",selected="Raw",inline=TRUE)
            hideElement("displayRadioButtonOut7")
            hideElement("displayRadioButtonOut8")
          }else if(input$displayRadioButtonOut1=="Target"){
            if(input$displayRadioButtonOut7=="Raw"){
              hideElement("displayRadioButtonOut8")
              updateRadioButtons(session = session, inputId = "displayRadioButtonOut7",choices=c("Raw","Base"),selected="Raw",inline=TRUE)
            }else if(input$displayRadioButtonOut7=="Base"){
              showElement("displayRadioButtonOut8")
              updateRadioButtons(session = session, inputId = "displayRadioButtonOut7",choices=c("Raw","Base"),selected="Base",inline=TRUE)
              showElement("displayRadioButtonOut7")
            }else if(input$displayRadioButtonOut7=="Target"){
              showElement("displayRadioButtonOut8")
              updateRadioButtons(session = session, inputId = "displayRadioButtonOut7",choices=c("Raw","Base"),selected="Base",inline=TRUE)
              showElement("displayRadioButtonOut7")
            }
            showElement("displayRadioButtonOut7")
          }else if(input$displayRadioButtonOut1=="Implemented"){
            if(input$displayRadioButtonOut7=="Raw"){
              updateRadioButtons(session = session, inputId = "displayRadioButtonOut7",choices=c("Raw","Base","Target"),selected="Raw",inline=TRUE)
              hideElement("displayRadioButtonOut8")
            }else if(input$displayRadioButtonOut7=="Base"){
              updateRadioButtons(session = session, inputId = "displayRadioButtonOut7",choices=c("Raw","Base","Target"),selected="Base",inline=TRUE)
              showElement("displayRadioButtonOut8")
            }else if(input$displayRadioButtonOut7=="Target"){
              updateRadioButtons(session = session, inputId = "displayRadioButtonOut7",choices=c("Raw","Base","Target"),selected="Target",inline=TRUE)
              showElement("displayRadioButtonOut8")
            }
            showElement("displayRadioButtonOut7")
          }
        }
      })
    })
  }

  ##This hides options for weight if relative comparison is chosen for results plotting
  if(is.element(2,observers)){
    observerPool[[5]]<<-observe(priority=1,{
      withProgress(message="Updating results radio options 2",value=0.5,{
        #print("running observer 2")
        if(
          !is.null(input$displayRadioButtonOut5)&
          !is.null(input$displayRadioButtonOut7) &
          !is.null(input$displayRadioButtonOut8) &
          !is.null(input$displayRadioButtonOut2)){
          if((input$displayRadioButtonOut7!="Raw" & (input$displayRadioButtonOut8=="Percentage" | input$displayRadioButtonOut8=="Proportion")) | (input$displayRadioButtonOut5!="Landings" & input$displayRadioButtonOut5!="Dead Discards")){
            updateRadioButtons(session = session, inputId = "displayRadioButtonOut2",selected="Weight",inline=TRUE)
            hideElement("displayRadioButtonOut2")
          }else{
            showElement("displayRadioButtonOut2")
          }
        }
      })
    })
  }

  ##This Also hides irrelavent choice radios depending on choice of output for results
  if(is.element(3,observers)){
    observerPool[[6]]<<-observe(priority=1,{
      withProgress(message="Updating results radio options 3",value=0.5,{
        #print("running observer 3")
        if(!is.null(input$displayRadioButtonOut5) &
           !is.null(input$displayRadioButtonOut)){
          if(input$displayRadioButtonOut5=="Kobe"){
            hideElement("TimeSeriesInputs")
            showElement("KobePlotInputs")
          }else if(input$displayRadioButtonOut5=="Landings" | input$displayRadioButtonOut5=="Dead Discards"){
            showElement("TimeSeriesInputs")
            hideElement("KobePlotInputs")
            showElement("displayRadioButtonOut6")
            showElement("displayRadioButtonOut")
          }else if(input$displayRadioButtonOut5=="SPB" | input$displayRadioButtonOut5=="Harvest Rate"){
            if(data.orig$nseas == 1 & data.orig$N_areas ==1){
            }else if(data.orig$nseas == 1){
              updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonOut6",selected=c("Area"))
            }else if(data.orig$N_areas == 1){
              updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonOut6",selected=c("Season"))
            }else{
              updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonOut6",selected=c("Area","Season"))
            }
            updateRadioButtons(session = session, inputId = "displayRadioButtonOut",selected="Total",inline=TRUE)
            showElement("TimeSeriesInputs")
            hideElement("KobePlotInputs")
            hideElement("displayRadioButtonOut6")
            hideElement("displayRadioButtonOut")
          }else if(input$displayRadioButtonOut5=="Population Biomass"){
            showElement("TimeSeriesInputs")
            hideElement("KobePlotInputs")
            #updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonOut6",choices=c("Area"),selected=c("Area"))
            updateRadioButtons(session = session, inputId = "displayRadioButtonOut",selected="Total",inline=TRUE)
            showElement("displayRadioButtonOut6")
            hideElement("displayRadioButtonOut")
          }else if(input$displayRadioButtonOut5=="Economics"){
            showElement("TimeSeriesInputs")
            hideElement("KobePlotInputs")
            #updateRadioButtons(session = session, inputId = "displayRadioButtonOut",selected="Total",inline=TRUE)
            showElement("displayRadioButtonOut6")
            showElement("displayRadioButtonOut")
          }else{
            showElement("TimeSeriesInputs")
            hideElement("KobePlotInputs")
            #updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonOut6",choices==c("Area","Season"))
            updateRadioButtons(session = session, inputId = "displayRadioButtonOut",selected="Total",inline=TRUE)
            showElement("displayRadioButtonOut6")
            hideElement("displayRadioButtonOut")
          }
        }
      })
    })
  }

  ##This hides group and fleet viewer checkbox groups depending on previous choice of results display complexity
  if(is.element(4,observers)){
    observerPool[[7]]<<-observe(priority=1,{
      withProgress(message="Updating results radio options 4",value=0.5,{
        #print("running observer 4")
        if(!is.null(input$displayRadioButtonOut3) &
           !is.null(input$displayRadioButtonOut4) &
           !is.null(input$displayRadioButtonOut)){
          if(input$displayRadioButtonOut=="Total"){
            hideElement("displayRadioButtonOut3")
            hideElement("displayRadioButtonOut4")
          }else if(input$displayRadioButtonOut=="Groups"){
            hideElement("displayRadioButtonOut3")
            showElement("displayRadioButtonOut4")
          }else if(input$displayRadioButtonOut=="Fleets"){
            showElement("displayRadioButtonOut3")
            hideElement("displayRadioButtonOut4")
          }
        }
      })
    })
  }

  ##This saves the current displayed results to an image and cvs output folder
  if(is.element(5,observers)){
    observerPool[[8]]<<-observeEvent(input$saveOutput,priority=1,{
      withProgress(message="Saving results",value=0.5,{
        #print("running observer 5")
        saveCurrResults(input,output,session,input$saveName)
      })
    })
  }

  ##This modifies input sliders depending on choice of optimum target value
  if(is.element(8,observers)){
    observerPool[[9]]<<-observe(priority = 1,{
      #print("running observer 8")
      withProgress(message="Updating Target Values",value=0.5,{
        trigger<-input$ForecastTarget
        if(isolate({!is.null(input$ForecastTarget) & exists("targetValue")})){
          isolate({
            updateSliderInput(session=session,inputId="TargetValue",label=targetNames2[as.numeric(input$ForecastTarget)],min=0,max=1,value=targetValue[as.numeric(input$ForecastTarget)],step=0.05)
          })
          if(as.numeric(input$ForecastTarget)==2){
            isolate({
              hideElement(id="CostBenefits")
              hideElement(id="TargetValue")
              showElement(id="TargetYears")
              showElement(id="ForecastApplied")
              })
          }else if(as.numeric(input$ForecastTarget)==4){
            isolate({
              hideElement(id="CostBenefits")
              hideElement(id="TargetValue")
              hideElement(id="TargetYears")
              showElement(id="ForecastApplied")})
          }else if(as.numeric(input$ForecastTarget)==6){
            isolate({
              showElement(id="CostBenefits")
              hideElement(id="TargetValue")
              showElement(id="TargetYears")
              hideElement(id="ForecastApplied")})
          }else if(as.numeric(input$ForecastTarget)==5){
            isolate({
              hideElement(id="CostBenefits")
              hideElement(id="TargetValue")
              hideElement(id="TargetYears")
              hideElement(id="ForecastApplied")
              })
          }else{
            isolate({
              hideElement(id="CostBenefits")
              showElement(id="TargetValue")
              showElement(id="TargetYears")
              showElement(id="ForecastApplied")})
          }
        }
      })
    })
  }

  ##This modifies input options based on choice of applied fishing strategy
  if(is.element(9,observers)){
    observerPool[[10]]<<-observe(priority = 1,{
      #print("running observer 9")
      withProgress(message="Updating Forecast Values",value=0.5,{
        trigger<-input$ForecastApplied
        trigger<-input$Rebuild
        if(isolate({!is.null(input$ForecastApplied) & !is.null(input$Rebuild)})){
          if(as.numeric(input$ForecastApplied)==1){
            isolate({hideElement(id="ConstCatchInput")
              hideElement(id="ConstCatchfrac")
              hideElement(id="ABCfrac")
              hideElement(id="ConstCatchRebuild")
              hideElement(id="Rebuild")
              hideElement(id="CompAssessments")
              hideElement(id="KobeMinCatch")
              hideElement(id="KobeMaxCatch")
              hideElement(id="KobeNumCatch")
              hideElement(id="KobeCatches")})
          }else if(as.numeric(input$ForecastApplied)==2){
            isolate({hideElement(id="ConstCatchInput")
              hideElement(id="ConstCatchfrac")
              showElement(id="ABCfrac")
              hideElement(id="ConstCatchRebuild")
              hideElement(id="Rebuild")
              hideElement(id="CompAssessments")
              hideElement(id="KobeMinCatch")
              hideElement(id="KobeMaxCatch")
              hideElement(id="KobeNumCatch")
              hideElement(id="KobeCatches")})
          }else if(as.numeric(input$ForecastApplied)==3){
            isolate({hideElement(id="ABCfrac")
              showElement(id="ConstCatchInput")
              hideElement(id="Rebuild")
              hideElement(id="CompAssessments")
              hideElement(id="KobeMinCatch")
              hideElement(id="KobeMaxCatch")
              hideElement(id="KobeNumCatch")
              hideElement(id="KobeCatches")})
            if(isolate({!is.null(input$ConstCatchInput)})){
              if(input$ConstCatchInput==""){
                isolate({showElement(id="ConstCatchfrac")})
              }else{
                isolate({hideElement(id="ConstCatchfrac")})
              }
            }else{
              isolate({hideElement(id="ConstCatchfrac")})
            }
            if(as.numeric(input$Rebuild)==1){
              isolate({hideElement(id="ConstCatchRebuild")})
            }else{
              isolate({hideElement(id="ConstCatchRebuild")})}
          }else if(as.numeric(input$ForecastApplied)==4){
            isolate({hideElement(id="ConstCatchInput")
              hideElement(id="ConstCatchfrac")
              hideElement(id="ABCfrac")
              hideElement(id="ConstCatchRebuild")
              hideElement(id="Rebuild")
              showElement(id="CompAssessments")
              showElement(id="KobeMinCatch")
              showElement(id="KobeMaxCatch")
              showElement(id="KobeNumCatch")
              showElement(id="KobeCatches")})
          }
          if(as.numeric(input$Rebuild)==1){
            isolate({hideElement(id="RebuildTargetFraction")
              hideElement(id="RebuildTargetYear")})
          }else{
            isolate({hideElement(id="RebuildTargetFraction")
              hideElement(id="RebuildTargetYear")})
          }
        }})
    })
  }

  ##This switches between displaying seasonal or annual allocation sliders based on user input
  if(is.element(11,observers)){
    observerPool[[11]]<<-observe(priority = 1,{
      #print("running observer 11")
      withProgress(message="Update fleet and season sliders",value=0.5,{
        trigger<-eval(parse(text=paste0("input$displaySeason")))
        # trigger<-eval(parse(text=paste0("input$Fleet",data.orig$Nfleet,"AllocationSlide")))
        # trigger<-eval(parse(text=paste0("input$Fleet",data.orig$Nfleet,"Seas",data.orig$nseas,"AllocationSlide")))

        if(isolate({!is.null(eval(parse(text=paste0("input$displaySeason")))) &
            !is.null(eval(parse(text=paste0("input$Fleet",data.orig$Nfleet,"AllocationSlide")))) &
            !is.null(eval(parse(text=paste0("input$Fleet",data.orig$Nfleet,"Seas",data.orig$nseas,"AllocationSlide"))))})
        ){
          displaySeas<-eval(parse(text=paste0("input$displaySeason")))
          if(displaySeas==1){
            isolate({
              for(i in 1:data.orig$Nfleet){
                if(length(which(forecast.orig$fleet_assignment_to_allocation_group==forecast.orig$fleet_assignment_to_allocation_group[i]))==1){
                  hideElement(id=paste0("Fleet",i,"AllocationSlide"))
                  hideElement(id=paste0("Fleet",i,"AllocationFixed"))
                }else{
                  showElement(id=paste0("Fleet",i,"AllocationSlide"))
                  showElement(id=paste0("Fleet",i,"AllocationFixed"))
                }
                showElement(id=paste0("Fleet",i,"catchHist"))

                for(j in 1:data.orig$nseas){
                  hideElement(id=paste0("Fleet",i,"Seas",j,"catchHist"))
                  hideElement(id=paste0("Fleet",i,"Seas",j,"AllocationSlide"))
                  hideElement(id=paste0("Fleet",i,"Seas",j,"AllocationFixed"))
                }
              }
            })
          }else if(displaySeas==2){
            isolate({
              for(i in 1:data.orig$Nfleet){
                hideElement(id=paste0("Fleet",i,"AllocationSlide"))
                hideElement(id=paste0("Fleet",i,"AllocationFixed"))
                hideElement(id=paste0("Fleet",i,"catchHist"))
                if(data.orig$nseas==1 && length(which(forecast.orig$fleet_assignment_to_allocation_group==forecast.orig$fleet_assignment_to_allocation_group[i]))==1){
                  hideElement(id=paste0("Fleet",i,"Seas",1,"AllocationSlide"))
                  hideElement(id=paste0("Fleet",i,"Seas",1,"AllocationFixed"))
                  showElement(id=paste0("Fleet",i,"Seas",1,"catchHist"))
                }else{
                  for(j in 1:data.orig$nseas){
                    showElement(id=paste0("Fleet",i,"Seas",j,"AllocationSlide"))
                    showElement(id=paste0("Fleet",i,"Seas",j,"AllocationFixed"))
                    showElement(id=paste0("Fleet",i,"Seas",j,"catchHist"))
                  }
                }
              }
            })
          }
        }else{
          #invalidateLater(100)
        }
      })
    })
  }

  ##This updates hidden seasonal and annual allocation slider values based on user inputs to the visible sliders
  if(is.element(12,observers)){

    observerPool[[12]]<<-observe({
      withProgress(message="Update annual fleet allocation sliders",value=0.5,{
        ScaleAllocations(input,output,session)
      })
    })
  }

  ##Hides catch input cells for future years if fixed catch proportions selected as SS overides these values anyway
  if(is.element(13,observers)){

    observerPool[[13]]<<-observe(priority = 1,{
      trigger<-input$displaySeason
      #print("running observer 13")
      CatchUnits<-rep(0,data.orig$Nfleet)
      withProgress(message="resetting fleet proportion values",value=0.5,{
        for(i_in in 1:data.orig$Nfleet){
          #local({
            i<-i_in
            if(!is.null(eval(parse(text=paste0("input$displaySeason")))) &
               !is.null(eval(parse(text=paste0("input$Group",forecast.orig$fleet_assignment_to_allocation_group[i],"FleetAllocUnits")))) &
               !is.null(eval(parse(text=paste0("input$fleet",i,"recentCatchQuantAll")))) &
               !is.null(eval(parse(text=paste0("input$Fleet",i,"Seas",data.orig$nseas,"AllocationSlide"))))
            ){
              AllocGroup<-which(groups==forecast.orig$fleet_assignment_to_allocation_group[i])
              fleetData<-forecast.orig$ForeCatch[forecast.orig$ForeCatch[,3]==i,,drop=FALSE]
              if(length(fleetData[,1])==0){
                if(data.orig$units_of_catch[i]==1)
                  {CatchUnits[i]<-3}else if (data.orig$units_of_catch[i]==2){CatchUnits[i]<-6}
              }else{
                CatchUnits[i]<-fleetData[1,5]
                # if(fleetData[1,5]==2){
                #   if(data.orig$units_of_catch[i]==1){
                #     CatchUnits[i]<-5
                #   }else if (data.orig$units_of_catch[i]==2){CatchUnits[i]<-6}
                # }else if(fleetData[1,5]==3){if(data.orig$units_of_catch[i]==1){CatchUnits[i]<-2}else if (data.orig$units_of_catch[i]==2){CatchUnits[i]<-3}
                # }else if(fleetData[1,5]==99){CatchUnits[i]<-99}
              }
              if(CatchUnits[i]==2){tempData<-rel_DB_orig}
              if(CatchUnits[i]==3){tempData<-rel_RB_orig}
              if(CatchUnits[i]==5){tempData<-rel_DN_orig}
              if(CatchUnits[i]==6){tempData<-rel_RN_orig}
              if(CatchUnits[i]==99){tempData<-rel_F_orig}
              withProgress(message="Update Catch history values and inputs",value=0.5,{
                if(input$displaySeason==1){
                  for(j in 1:data.orig$nseas){
                    if((forecast.orig$fleet_assignment_to_allocation_group[i]==0 & eval(parse(text=paste0("input$Group",AllocGroup,"FleetAllocUnits")))==2)){
                      showElement(id=paste0("fleet",i,"recentCatchQuantAll"))
                    }else{
                      hideElement(id=paste0("fleet",i,"recentCatchQuantAll"))
                    }
                    showElement(id=paste0("fleet",i,"seas",j,"recentCatchQuantAll"))
                    updateTextInput(session=session,inputId=paste0("fleet",i,"seas",j,"recentCatchQuantAll"),value = as.numeric(eval(parse(text=paste0("input$fleet",i,"recentCatchQuantAll"))))*(tempData[j,i]/sum(tempData[,i])))
                    hideElement(id=paste0("fleet",i,"seas",j,"recentCatchQuantAll"))

                    for(k in 1:(as.integer(format(Sys.Date(), "%Y"))-data.orig$endyr+numFutCatchYrs)){
                      if((forecast.orig$fleet_assignment_to_allocation_group[i]==0 & eval(parse(text=paste0("input$Group",AllocGroup,"FleetAllocUnits")))==2) | k<=(as.integer(format(Sys.Date(), "%Y"))-data.orig$endyr)){
                        showElement(id=paste0("fleet",i,"recentCatchQuant",k))
                      }else{
                        hideElement(id=paste0("fleet",i,"recentCatchQuant",k))
                      }
                      hideElement(id=paste0("fleet",i,"seas",j,"recentCatchQuant",k))
                      if(!is.na(as.numeric(eval(parse(text=paste0("input$fleet",i,"recentCatchQuant",k)))))){
                        updateTextInput(session=session,inputId=paste0("fleet",i,"seas",j,"recentCatchQuant",k),value = as.numeric(eval(parse(text=paste0("input$fleet",i,"recentCatchQuant",k))))*(tempData[j,i]/sum(tempData[,i])))
                      }
                    }
                  }
                }else if(input$displaySeason==2){
                  sumCatch<-rep(NA,(as.integer(format(Sys.Date(), "%Y"))-data.orig$endyr+numFutCatchYrs+1))
                  for(j in 1:data.orig$nseas){
                    if((forecast.orig$fleet_assignment_to_allocation_group[i]==0 & eval(parse(text=paste0("input$Group",AllocGroup,"FleetAllocUnits")))==2)){
                      showElement(id=paste0("fleet",i,"seas",j,"recentCatchQuantAll"))
                    }else{
                      hideElement(id=paste0("fleet",i,"seas",j,"recentCatchQuantAll"))
                    }
                    if(!is.na(as.numeric(eval(parse(text=paste0("input$fleet",i,"seas",j,"recentCatchQuantAll")))))){
                      if(is.na(sumCatch[length(sumCatch)])){sumCatch[length(sumCatch)]<-0}
                      sumCatch[length(sumCatch)]<-sumCatch[length(sumCatch)]+as.numeric(eval(parse(text=paste0("input$fleet",i,"seas",j,"recentCatchQuantAll"))))
                    }
                    for(k in 1:(as.integer(format(Sys.Date(), "%Y"))-data.orig$endyr+numFutCatchYrs)){
                      if((forecast.orig$fleet_assignment_to_allocation_group[i]==0 & eval(parse(text=paste0("input$Group",AllocGroup,"FleetAllocUnits")))==2) | k<=(as.integer(format(Sys.Date(), "%Y"))-data.orig$endyr)){
                        showElement(id=paste0("fleet",i,"seas",j,"recentCatchQuant",k))
                      }else{
                        hideElement(id=paste0("fleet",i,"seas",j,"recentCatchQuant",k))
                      }
                      if(!is.na(as.numeric(eval(parse(text=paste0("input$fleet",i,"seas",j,"recentCatchQuant",k)))))){
                        if(is.na(sumCatch[k])){sumCatch[k]<-0}
                        sumCatch[k]<-sumCatch[k]+as.numeric(eval(parse(text=paste0("input$fleet",i,"seas",j,"recentCatchQuant",k))))
                      }
                    }
                  }
                  if(!is.na(sumCatch[length(sumCatch)])){
                    updateTextInput(session=session,inputId=paste0("fleet",i,"recentCatchQuantAll"),value = sumCatch[length(sumCatch)])
                    hideElement(id=paste0("fleet",i,"recentCatchQuantAll"))
                  }
                  for(k in 1:(as.integer(format(Sys.Date(), "%Y"))-data.orig$endyr+numFutCatchYrs)){
                    if(!is.na(sumCatch[k])){
                      updateTextInput(session=session,inputId=paste0("fleet",i,"recentCatchQuant",k),value = sumCatch[k])
                      hideElement(id=paste0("fleet",i,"recentCatchQuant",k))
                    }
                  }
                }
              })
            }
          #})
        }
      })
    })
  }

  ##This updates fleet allocation slider values when allocation units are changed
  if(is.element(14,observers)){
    observerPool[[14]]<<-observe(priority = 1,{
      #print("running observer 14")
      withProgress(message="Updating allocations change in relative intensity",value=0.5,{
        totalAlloc<-eval(parse(text=paste0("input$GroupAllocUnits")))
        displaySeas<-input$displaySeason
        if(isolate({!is.null(input$displaySeason) &
            !is.null(eval(parse(text=paste0("input$Fleet",data.orig$Nfleet,"AllocationSlide")))) &
            !is.null(eval(parse(text=paste0("input$Group",max(forecast.orig$fleet_assignment_to_allocation_group,1),"FleetAllocUnits")))) &
            !is.null(eval(parse(text=paste0("input$GroupAllocUnits"))))})){
          if(input$displaySeason==1){
            for(my_j in 1:data.orig$Nfleet){
              isolate({group<-forecast.orig$fleet_assignment_to_allocation_group[my_j]
              currGroup<-which(groups==group)})
              if(isolate({!is.null(eval(parse(text=paste0("input$Fleet",my_j,"AllocationSlide")))) &
                  !is.null(eval(parse(text=paste0("input$Group",currGroup,"FleetAllocUnits")))) &
                  !is.null(eval(parse(text=paste0("input$GroupAllocUnits"))))})){
                local({
                  j<-my_j
                  GroupAlloc<-eval(parse(text=paste0("input$Group",currGroup,"FleetAllocUnits")))
                  isolate({groupfleets<-which(forecast.orig$fleet_assignment_to_allocation_group==group)
                  currfleet<-which(groupfleets==j)
                  fleets<-groupfleets[-(currfleet:length(groupfleets))]})
                  if(prevGroupAlloc[currGroup]==GroupAlloc & prevTotalAlloc==totalAlloc){
                    temp.max.fleet[j]<<-1#round((1-sum(temp.val.fleet[fleets])),sliderResolution)
                    temp.val.fleet[j]<<-eval(parse(text=paste0("input$Fleet",j,"AllocationSlide")))
                  }else{
                    temp.max.fleet[j]<<-1#round((1-sum(temp.val.fleet[fleets])),sliderResolution)
                    if(eval(parse(text=paste0("input$Group",currGroup,"FleetAllocUnits")))==1){
                      if(eval(parse(text=paste0("input$GroupAllocUnits")))==2){
                        if(sum(rel_DB_orig[,groupfleets])==0){
                          temp.val.fleet[j]<<-0
                        }else{
                          temp.val.fleet[j]<<-sum(rel_DB_orig[,j])/sum(rel_DB_orig[,groupfleets])
                        }
                      }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==3){
                        if(sum(rel_RB_orig[,groupfleets])==0){
                          temp.val.fleet[j]<<-0
                        }else{
                          temp.val.fleet[j]<<-sum(rel_RB_orig[,j])/sum(rel_RB_orig[,groupfleets])
                        }
                      }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==5){
                        if(sum(rel_DN_orig[,groupfleets])==0){
                          temp.val.fleet[j]<<-0
                        }else{
                          temp.val.fleet[j]<<-sum(rel_DN_orig[,j])/sum(rel_DN_orig[,groupfleets])
                        }
                      }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==6){
                        if(sum(rel_RN_orig[,groupfleets])==0){
                          temp.val.fleet[j]<<-0
                        }else{
                          temp.val.fleet[j]<<-sum(rel_RN_orig[,j])/sum(rel_RN_orig[,groupfleets])
                        }
                      }
                    }else{
                      if(sum(rel_F_orig[,groupfleets])==0){
                        temp.val.fleet[j]<<-0
                      }else{
                        temp.val.fleet[j]<<-sum(rel_F_orig[,j])/sum(rel_F_orig[,groupfleets])
                      }
                    }
                    isolate({updateSliderInput(session=session,inputId=paste0("Fleet",j,"AllocationSlide"), max=temp.max.fleet[j], value=temp.val.fleet[j], step=(10^-sliderResolution))})
                  }
                  #updateSliderInput(session=session,inputId=paste0("Fleet",j,"AllocationSlide"), max=temp.max.fleet[j], value=temp.val.fleet[j], step=(10^-sliderResolution))
                  #showElement(id=paste0("Fleet",j,"AllocationSlide"))
                  for(i in 1:data.orig$nseas){
                    if(!is.null(eval(parse(text=paste0("input$Fleet",j,"Seas",i,"AllocationSlide"))))){
                      temp.max.fleet.seas[i,j]<<-1#round((1-sum(temp.val.fleet.seas[,fleets])-sum(temp.val.fleet.seas[-(i:data.orig$nseas),j])),sliderResolution)
                      if(eval(parse(text=paste0("input$Group",currGroup,"FleetAllocUnits")))==1){
                        if(eval(parse(text=paste0("input$GroupAllocUnits")))==2){
                          if(sum(rel_DB_orig[,j])==0){
                            temp.val.fleet.seas[i,j]<<-0
                          }else{
                            temp.val.fleet.seas[i,j]<<-(rel_DB_orig[i,j]/sum(rel_DB_orig[,j]))*temp.val.fleet[j]}
                        }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==3){
                          if(sum(rel_RB_orig[,j])==0){
                            temp.val.fleet.seas[i,j]<<-0
                          }else{temp.val.fleet.seas[i,j]<<-(rel_RB_orig[i,j]/sum(rel_RB_orig[,j]))*temp.val.fleet[j]}
                        }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==5){
                          if(sum(rel_DN_orig[,j])==0){
                            temp.val.fleet.seas[i,j]<<-0
                          }else{temp.val.fleet.seas[i,j]<<-(rel_DN_orig[i,j]/sum(rel_DN_orig[,j]))*temp.val.fleet[j]}
                        }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==6){
                          if(sum(rel_RN_orig[,j])==0){
                            temp.val.fleet.seas[i,j]<<-0
                          }else{temp.val.fleet.seas[i,j]<<-(rel_RN_orig[i,j]/sum(rel_RN_orig[,j]))*temp.val.fleet[j]}
                        }
                      }else{
                        if(sum(rel_F_orig[,j])==0){
                          temp.val.fleet.seas[i,j]<<-0
                        }else{temp.val.fleet.seas[i,j]<<-(rel_F_orig[i,j]/sum(rel_F_orig[,j]))*temp.val.fleet[j]}
                      }
                      isolate({updateSliderInput(session=session,inputId=paste0("Fleet",j,"Seas",i,"AllocationSlide"), max=temp.max.fleet.seas[i,j], value=temp.val.fleet.seas[i,j], step=(10^-sliderResolution))})
                      #hideElement(id=paste0("Fleet",j,"Seas",i,"AllocationSlide"))
                    }
                  }
                })
              }else{break}
            }
            for(i in 1:length(groups)){
              if(!is.null(eval(parse(text=paste0("input$Group",i,"FleetAllocUnits"))))){
                prevGroupAlloc[i]<<-eval(parse(text=paste0("input$Group",i,"FleetAllocUnits")))
              }
            }
            prevTotalAlloc<<-as.integer(eval(parse(text=paste0("input$GroupAllocUnits"))))
          }else{
            for(my_j in 1:data.orig$Nfleet){
              group<-forecast.orig$fleet_assignment_to_allocation_group[my_j]
              currGroup<-which(groups==group)
              if(!is.null(eval(parse(text=paste0("input$Fleet",my_j,"AllocationSlide")))) &
                 !is.null(eval(parse(text=paste0("input$Group",currGroup,"FleetAllocUnits")))) &
                 !is.null(eval(parse(text=paste0("input$GroupAllocUnits"))))){
                local({
                  j<-my_j
                  temp.max.fleet[j]<<-0
                  temp.val.fleet[j]<<-0
                  GroupAlloc<-eval(parse(text=paste0("input$Group",currGroup,"FleetAllocUnits")))
                  groupfleets<-which(forecast.orig$fleet_assignment_to_allocation_group==group)
                  currfleet<-which(groupfleets==j)
                  fleets<-groupfleets[-(currfleet:length(groupfleets))]
                  for(i in 1:data.orig$nseas){
                    currVal<-eval(parse(text=paste0("input$Fleet",j,"Seas",i,"AllocationSlide")))
                    if(prevGroupAlloc[currGroup]==GroupAlloc & prevTotalAlloc==totalAlloc){
                      temp.max.fleet.seas[i,j]<<-1
                      temp.val.fleet.seas[i,j]<<-eval(parse(text=paste0("input$Fleet",j,"Seas",i,"AllocationSlide")))
                    }else{
                      temp.max.fleet.seas[i,j]<<-1
                      if(eval(parse(text=paste0("input$Group",currGroup,"FleetAllocUnits")))==1){
                        if(eval(parse(text=paste0("input$GroupAllocUnits")))==2){
                          if(sum(rel_DB_orig[,groupfleets])==0){
                            temp.val.fleet.seas[i,j]<<-0
                          }else{
                            temp.val.fleet.seas[i,j]<<-rel_DB_orig[i,j]/sum(rel_DB_orig[,groupfleets])
                          }
                        }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==3){
                          if(sum(rel_RB_orig[,groupfleets])==0){
                            temp.val.fleet.seas[i,j]<<-0
                          }else{
                            temp.val.fleet.seas[i,j]<<-sum(rel_RB_orig[i,j])/sum(rel_RB_orig[,groupfleets])
                          }
                        }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==5){
                          if(sum(rel_DN_orig[,groupfleets])==0){
                            temp.val.fleet.seas[i,j]<<-0
                          }else{
                            temp.val.fleet.seas[i,j]<<-sum(rel_DN_orig[i,j])/sum(rel_DN_orig[,groupfleets])
                          }
                        }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==6){
                          if(sum(rel_RN_orig[,groupfleets])==0){
                            temp.val.fleet.seas[i,j]<<-0
                          }else{
                            temp.val.fleet.seas[i,j]<<-sum(rel_RN_orig[i,j])/sum(rel_RN_orig[,groupfleets])
                          }
                        }
                      }else{
                        if(sum(rel_F_orig[,groupfleets])==0){
                          temp.val.fleet.seas[i,j]<<-0
                        }else{
                          temp.val.fleet.seas[i,j]<<-sum(rel_F_orig[i,j])/sum(rel_F_orig[,groupfleets])
                        }
                      }
                      isolate({updateSliderInput(session=session,inputId=paste0("Fleet",j,"Seas",i,"AllocationSlide"), max=temp.max.fleet.seas[i,j], value=temp.val.fleet.seas[i,j], step=(10^-sliderResolution))})
                    }
                    temp.max.fleet[j]<<-1
                    temp.val.fleet[j]<<-temp.val.fleet[j]+temp.val.fleet.seas[i,j]
                  }
                  isolate({updateSliderInput(session=session,inputId=paste0("Fleet",j,"AllocationSlide"), max=temp.max.fleet[j], value=temp.val.fleet[j], step=(10^-sliderResolution))})
                })
              }else{break}
            }
            for(i in 1:length(groups)){
              if(!is.null(eval(parse(text=paste0("input$Group",i,"FleetAllocUnits"))))){
                prevGroupAlloc[i]<<-eval(parse(text=paste0("input$Group",i,"FleetAllocUnits")))
              }
            }
            prevTotalAlloc<<-as.integer(eval(parse(text=paste0("input$GroupAllocUnits"))))
          }
        }
      })
    })
  }

  ##This hides options for weight if relative comparison is chosen for results plotting
  if(is.element(15,observers)){
    observerPool[[16]]<<-observe(priority=1,{
      withProgress(message="Updating Base radio options 2",value=0.5,{
        #print("running oberserver 16")
        if(
          !is.null(input$displayRadioButtonBase5)&
          !is.null(input$displayRadioButtonBase2)){
          if((input$displayRadioButtonBase5!="Landings" & input$displayRadioButtonBase5!="Dead Discards")){
            updateRadioButtons(session = session, inputId = "displayRadioButtonBase2",selected="Weight",inline=TRUE)
            hideElement("displayRadioButtonBase2")
          }else{
            showElement("displayRadioButtonBase2")
          }
        }
      })
    })
  }

  ##This Also hides irrelavent choice radios depending on choice of output for results
  if(is.element(16,observers)){
    observerPool[[17]]<<-observe(priority=1,{
      withProgress(message="Updating Base radio options 3",value=0.5,{
        #print("running observer 17")
        if(!is.null(input$displayRadioButtonBase5) &
           !is.null(input$displayRadioButtonBase)){
          if(input$displayRadioButtonBase5=="Landings" | input$displayRadioButtonBase5=="Dead Discards" ){
            showElement("TimeSeriesInputsBase")
            showElement("displayRadioButtonBase6")
            showElement("displayRadioButtonBase")
          }else if(input$displayRadioButtonBase5=="SPB"| input$displayRadioButtonBase5=="Harvest Rate"){
            if(data.orig$nseas == 1 & data.orig$N_areas ==1){
            }else if(data.orig$nseas == 1){
              updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonBase6",selected=c("Area"))
            }else if(data.orig$N_areas == 1){
              updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonBase6",selected=c("Season"))
            }else{
              updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonBase6",selected=c("Area","Season"))
            }
            updateRadioButtons(session = session, inputId = "displayRadioButtonBase",selected="Total",inline=TRUE)
            showElement("TimeSeriesInputsBase")
            hideElement("displayRadioButtonBase6")
            hideElement("displayRadioButtonBase")
          }else if(input$displayRadioButtonBase5=="Population Biomass"){
            showElement("TimeSeriesInputsBase")
            #updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonOut6",choices=c("Area"),selected=c("Area"))
            updateRadioButtons(session = session, inputId = "displayRadioButtonBase",selected="Total",inline=TRUE)
            showElement("displayRadioButtonBase6")
            hideElement("displayRadioButtonBase")
          }else{
            showElement("TimeSeriesInputsBase")
            #updateCheckboxGroupInput(session = session, inputId = "displayRadioButtonOut6",choices==c("Area","Season"))
            updateRadioButtons(session = session, inputId = "displayRadioButtonBase",selected="Total",inline=TRUE)
            showElement("displayRadioButtonBase6")
            hideElement("displayRadioButtonBase")
          }
        }
      })
    })
  }

  ##This hides group and fleet viewer checkbox groups depending on previous choice of results display complexity
  if(is.element(17,observers)){
    observerPool[[18]]<<-observe(priority=1,{
      withProgress(message="Updating Base radio options 4",value=0.5,{
        if(!is.null(input$displayRadioButtonBase3) &
           !is.null(input$displayRadioButtonBase4) &
           !is.null(input$displayRadioButtonBase)){
          if(input$displayRadioButtonBase=="Total"){
            hideElement("displayRadioButtonBase3")
            hideElement("displayRadioButtonBase4")
          }else if(input$displayRadioButtonBase=="Groups"){
            hideElement("displayRadioButtonBase3")
            showElement("displayRadioButtonBase4")
          }else if(input$displayRadioButtonBase=="Fleets"){
            showElement("displayRadioButtonBase3")
            hideElement("displayRadioButtonBase4")
          }
        }
      })
    })
  }

  ##This updates the allocation fractions to match the selected year range any time you adjust the slider
  if(is.element(18,observers)){
    observerPool[[19]]<<-observeEvent(input$RelAllocYears,ignoreInit=TRUE,{
      withProgress(message="Updating Allocation to match historic values",value=0.5,{
        rel_F_temp <- matrix(NA,nrow=data.orig$nseas,ncol=data.orig$Nfleet,byrow=TRUE)
        names(rel_F_temp) <- paste0("Fleet ",1:data.orig$Nfleet)
        rel_C_temp <- rel_F_temp
        for(iseas in 1:data.orig$nseas){
          rel_F_temp[iseas,]<-as.numeric(apply(aggreg.timeSeries.F[(aggreg.timeSeries.F$Yr>=input$RelAllocYears[1] & aggreg.timeSeries.F$Yr<=input$RelAllocYears[2]),(11+8*(1:data.orig$Nfleet))],2,sum)/(input$RelAllocYears[2]-input$RelAllocYears[1]+1))
          if(input$GroupAllocUnits==2){
            rel_C_temp[iseas,]<-as.numeric(apply(aggreg.timeSeries.DB[(aggreg.timeSeries.DB$Yr>=input$RelAllocYears[1] & aggreg.timeSeries.DB$Yr<=input$RelAllocYears[2]),(13+8*(0:(data.orig$Nfleet-1)))],2,sum)/(input$RelAllocYears[2]-input$RelAllocYears[1]+1))*weightScale[disp.Desc$Units[1]]
          }else if(input$GroupAllocUnits==3){
            rel_C_temp[iseas,]<-as.numeric(apply(aggreg.timeSeries.RB[(aggreg.timeSeries.RB$Yr>=input$RelAllocYears[1] & aggreg.timeSeries.RB$Yr<=input$RelAllocYears[2]),(14+8*(0:(data.orig$Nfleet-1)))],2,sum)/(input$RelAllocYears[2]-input$RelAllocYears[1]+1))*weightScale[disp.Desc$Units[1]]
          }else if(input$GroupAllocUnits==5){
            rel_C_temp[iseas,]<-as.numeric(apply(aggreg.timeSeries.DN[(aggreg.timeSeries.DN$Yr>=input$RelAllocYears[1] & aggreg.timeSeries.DN$Yr<=input$RelAllocYears[2]),(16+8*(0:(data.orig$Nfleet-1)))],2,sum)/(input$RelAllocYears[2]-input$RelAllocYears[1]+1))
          }else if(input$GroupAllocUnits==6){
            rel_C_temp[iseas,]<-as.numeric(apply(aggreg.timeSeries.RN[(aggreg.timeSeries.RN$Yr>=foreinput$RelAllocYears[1] & aggreg.timeSeries.RN$Yr<=input$RelAllocYears[2]),(17+8*(0:(data.orig$Nfleet-1)))],2,sum)/(input$RelAllocYears[2]-input$RelAllocYears[1]+1))
          }
        }

        for(j in 1:length(groups)){
          groupfleets<-which(forecast.orig$fleet_assignment_to_allocation_group==groups[j])
          if(groups[j]!=0){
            if(sum(rel_C_temp)==0){
              temp.val.group[j]<<-0
            }else{
              temp.val.group[j]<<-sum(rel_C_temp[,groupfleets])/sum(rel_C_temp)
            }
            updateSliderInput(session,paste0("Group",j,"AllocationSlide"),value=temp.val.group[j])
          }
          for(i in groupfleets){
            if(!is.null(eval(parse(text=paste0("input$Group",j,"FleetAllocUnits")))) & !is.null(eval(parse(text=paste0("input$displaySeason")))) & !is.null(eval(parse(text=paste0("input$Fleet",i,"AllocationSlide"))))){
              if(eval(parse(text=paste0("input$Group",j,"FleetAllocUnits")))==1){
                if(eval(parse(text=paste0("input$displaySeason")))==1){
                  if(sum(rel_C_temp[,groupfleets])==0){
                    temp.val.fleet[i]<<-0
                  }else{
                    temp.val.fleet[i]<<-sum(rel_C_temp[,i])/sum(rel_C_temp[,groupfleets])
                  }
                  updateSliderInput(session,paste0("Fleet",i,"AllocationSlide"),value=temp.val.fleet[i])
                }else{
                  for(k in 1:data.orig$nseas){
                    if(sum(rel_C_temp[,groupfleets])==0){
                      temp.val.fleet.seas[k,i]<<-0
                    }else{
                      temp.val.fleet.seas[k,i]<<-sum(rel_C_temp[k,i])/sum(rel_C_temp[,groupfleets])
                    }
                    updateSliderInput(session,paste0("Fleet",i,"Seas",k,"AllocationSlide"),value=temp.val.fleet.seas[k,i])
                  }
                }
              }else if(eval(parse(text=paste0("input$Group",j,"FleetAllocUnits")))==2){
                if(eval(parse(text=paste0("input$displaySeason")))==1){
                  if(sum(rel_F_temp[,groupfleets])==0){
                    temp.val.fleet[i]<<-0
                  }else{
                    temp.val.fleet[i]<<-sum(rel_F_temp[,i])/sum(rel_F_temp[,groupfleets])
                  }
                  updateSliderInput(session,paste0("Fleet",i,"AllocationSlide"),value=temp.val.fleet[i])
                }else{
                  for(k in 1:data.orig$nseas){
                    if(sum(rel_F_temp[,groupfleets])==0){
                      temp.val.fleet.seas[k,i]<<-0
                    }else{
                      temp.val.fleet.seas[k,i]<<-sum(rel_F_temp[k,i])/sum(rel_F_temp[,groupfleets])
                    }
                    updateSliderInput(session,paste0("Fleet",i,"Seas",k,"AllocationSlide"),value=temp.val.fleet.seas[k,i])
                  }
                }
              }
            }
          }
        }
      })
    })
  }
}

#' Scale Allocations
#'
#' Scales all allocation elements so that
#' appropriate values sum to one.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
ScaleAllocations<-function(input,output,session){
  incProgress(0.1, detail = "Scaling allocation fractions to 1") #0.1 total

  if(!is.null(eval(parse(text=paste0("input$Group",1,"AllocationSlide")))) | max(forecast.orig$fleet_assignment_to_allocation_group)==0){
    groups<-sort(unique(forecast.orig$fleet_assignment_to_allocation_group))
    if(length(groups[groups==0])>0){groups<-c(groups[-1],0)}
    sumGroupAllocFree<-0
    sumGroupAllocFixed<-0
    for(i in 1:length(groups)){
      fleets<-which(forecast.orig$fleet_assignment_to_allocation_group==groups[i])
      if(!is.null(eval(parse(text=paste0("input$Group",i,"AllocationSlide")))) & !is.null(eval(parse(text=paste0("input$Group",i,"AllocationFixed")))))
      {
        if((eval(parse(text=paste0("input$Group",i,"AllocationFixed"))))==FALSE){
          sumGroupAllocFree<-sumGroupAllocFree+as.numeric(eval(parse(text=paste0("input$Group",i,"AllocationSlide"))))
        }else{
          sumGroupAllocFixed<-sumGroupAllocFixed+as.numeric(eval(parse(text=paste0("input$Group",i,"AllocationSlide"))))
        }
      }else if(max(forecast.orig$fleet_assignment_to_allocation_group)==0){
        sumGroupAllocFree<-0
        sumGroupAllocFixed<-1
      }
      sumFleetAllocFree<-0
      sumFleetAllocFixed<-0
      for(j in fleets)
      {
        if(!is.null(eval(parse(text=paste0("input$displaySeason")))))
        {
          if(eval(parse(text=paste0("input$displaySeason")))==1){
            if(!is.null(eval(parse(text=paste0("input$Fleet",j,"AllocationSlide")))) & !is.null(eval(parse(text=paste0("input$Fleet",j,"AllocationFixed")))))
            {
              if((eval(parse(text=paste0("input$Fleet",j,"AllocationFixed"))))==FALSE){
                sumFleetAllocFree<-sumFleetAllocFree+as.numeric(eval(parse(text=paste0("input$Fleet",j,"AllocationSlide"))))
              }else{
                sumFleetAllocFixed<-sumFleetAllocFixed+as.numeric(eval(parse(text=paste0("input$Fleet",j,"AllocationSlide"))))
              }
            }
          }else{
            for(k in 1:data.orig$nseas)
            {
              if(!is.null(eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationSlide")))) & !is.null(eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationFixed")))))
              {
                if((eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationFixed"))))==FALSE){
                  sumFleetAllocFree<-sumFleetAllocFree+as.numeric(eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationSlide"))))
                }else{
                  sumFleetAllocFixed<-sumFleetAllocFixed+as.numeric(eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationSlide"))))
                }
              }
            }
          }
        }
      }
      if(((sumFleetAllocFixed+sumFleetAllocFree)>1.00001 | (sumFleetAllocFixed+sumFleetAllocFree)<0.99999) & (sumFleetAllocFixed+sumFleetAllocFree)!=0)
      {
        for(j in fleets)
        {
          if(!is.null(eval(parse(text=paste0("input$displaySeason")))))
          {
            if(eval(parse(text=paste0("input$displaySeason")))==1)
            {
              if(!is.null(eval(parse(text=paste0("input$Fleet",j,"AllocationSlide")))) & !is.null(eval(parse(text=paste0("input$Fleet",j,"AllocationFixed")))))
              {
                if(sumFleetAllocFixed>=1)
                {
                  if((eval(parse(text=paste0("input$Fleet",j,"AllocationFixed"))))==FALSE){
                    updateSliderInput(session=session,inputId=paste0("Fleet",j,"AllocationSlide"), value=0)
                  }else{
                    newVal<-eval(parse(text=paste0("input$Fleet",j,"AllocationSlide")))/sumFleetAllocFixed
                    updateSliderInput(session=session,inputId=paste0("Fleet",j,"AllocationSlide"), value=newVal)
                  }
                }else{
                  if((eval(parse(text=paste0("input$Fleet",j,"AllocationFixed"))))==FALSE){
                    newVal<-eval(parse(text=paste0("input$Fleet",j,"AllocationSlide")))/(sumFleetAllocFree/(1-sumFleetAllocFixed))
                    updateSliderInput(session=session,inputId=paste0("Fleet",j,"AllocationSlide"), value=newVal)
                  }
                }
              }
            }

            if(eval(parse(text=paste0("input$displaySeason")))==2){
              for(k in 1:data.orig$nseas)
              {
                if(!is.null(eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationSlide")))) & !is.null(eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationFixed")))))
                {
                  if(sumFleetAllocFixed>=1)
                  {
                    if((eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationFixed"))))==FALSE){
                      updateSliderInput(session=session,inputId=paste0("Fleet",j,"Seas",k,"AllocationSlide"), value=0)
                    }else{
                      newVal<-eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationSlide")))/sumFleetAllocFixed
                      updateSliderInput(session=session,inputId=paste0("Fleet",j,"Seas",k,"AllocationSlide"), value=newVal)
                    }
                  }else{
                    if((eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationFixed"))))==FALSE){
                      newVal<-eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationSlide")))/(sumFleetAllocFree/(1-sumFleetAllocFixed))
                      updateSliderInput(session=session,inputId=paste0("Fleet",j,"Seas",k,"AllocationSlide"), value=newVal)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    if((sumGroupAllocFixed+sumGroupAllocFree)>1.00001 | (sumGroupAllocFixed+sumGroupAllocFree)<0.99999)
    {
      for(i in 1:length(groups)){
        if(!is.null(eval(parse(text=paste0("input$Group",i,"AllocationSlide")))) & !is.null(eval(parse(text=paste0("input$Group",i,"AllocationFixed")))))
        {
          if(sumGroupAllocFixed>=1)
          {
            if((eval(parse(text=paste0("input$Group",i,"AllocationFixed"))))==FALSE){
              updateSliderInput(session=session,inputId=paste0("Group",i,"AllocationSlide"), value=0)
            }else{
              newVal<-eval(parse(text=paste0("input$Group",i,"AllocationSlide")))/sumGroupAllocFixed
              updateSliderInput(session=session,inputId=paste0("Group",i,"AllocationSlide"), value=newVal)
            }
          }else{
            if((eval(parse(text=paste0("input$Group",i,"AllocationFixed"))))==FALSE){
              newVal<-eval(parse(text=paste0("input$Group",i,"AllocationSlide")))/(sumGroupAllocFree/(1-sumGroupAllocFixed))
              updateSliderInput(session=session,inputId=paste0("Group",i,"AllocationSlide"), value=newVal)
            }
          }
        }
      }
    }
  }
}
