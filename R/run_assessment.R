#' Update assessment
#'
#' Runs an updated assessment using all the
#' user specified input adjustments to projection
#' parameters.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
runUpdateAssessment2<-function(input, output, session){
  withProgress(message="Running new assessment forecast",value=0,{

    if(input$ForecastApplied!=4){
      duplicateFleets(input,output,session)
      RunTargetForecast(input,output,session)
      RunAppliedForecast(input,output,session)

      if(input$runFolder!=""){
        dir.save<-paste0(dir.base,"/",input$runFolder)
        dir.save.run<-paste0(dir.base,"/",input$runFolder,"/appliedFishing")
        dir.save.msy<-paste0(dir.base,"/",input$runFolder,"/optimalFishing")
        if(!dir.exists(dir.save))
        {
          incProgress(0.0, detail = paste0("Building Save directory"))
          dir.create(dir.save)
          dir.create(dir.save.run)
          dir.create(dir.save.msy)
          incProgress(0.0, detail = paste0("Saving results"))
          files.run<-list.files(dir.run)
          files.msy<-list.files(dir.msy)
          file.copy(paste0(dir.run,"/",files.run),dir.save.run)
          file.copy(paste0(dir.msy,"/",files.msy),dir.save.msy)
        }else{
          incProgress(0.0, detail = paste0("Saving results"))
          files.save.msy<-list.files(dir.save.msy)
          unlink(paste0(dir.save.msy,"/",files.save.msy))
          files.msy<-list.files(dir.msy)
          file.copy(paste0(dir.msy,"/",files.msy),dir.save.msy)
          files.save.run<-list.files(dir.save.run)
          unlink(paste0(dir.save.run,"/",files.save.run))
          files.run<-list.files(dir.run)
          file.copy(paste0(dir.run,"/",files.run),dir.save.run)
        }
      }
    }else{
      true.dir.base<-dir.base
      print(true.dir.base)
      true.dir.orig<-dir.orig
      print(true.dir.orig)
      true.dir.run<-dir.run
      print(true.dir.run)
      dir.base<<-paste0(dir.base,"/kobe")
      print(dir.base)
      dir.run<<-paste0(dir.base,"/Run")
      print(dir.run)
      unlink(dir.base,recursive = TRUE, force=TRUE)
      dir.create(dir.base)
      unlink(dir.run,recursive = TRUE, force=TRUE)
      dir.create(dir.run)
      files.orig<-list.files(dir.orig)
      print(files.orig)
      file.copy(paste0(dir.orig,"/",files.orig),dir.run)
      unlink(paste0(dir.base,"/Kobe_Matrix.csv"))
      unlink(paste0(dir.base,"/OptHarv_Matrix.csv"))

      outVectColNames<-c("Yieldequil","SSBequi","Bequil","Fequil","Yield1yr","SSB1yr","B1yr","F1yr","Yield5yr","SSB5yr","B5yr","F5yr","Yield10yr","SSB10yr","B10yr","F10yr","Yield20yr","SSB20yr","B20yr","F20yr","Yieldequil/MSY","SSBequi/Bmsy","BequilBmsy","Fequil/Fmsy","Yield1yr/MSY","SSB1yr/SSBmsy","B1yr/Bmsy","F1yr/Fmsy","Yield5yr/MSY","SSB5yr/SSBmsy","B5yr/Bmsy","F5yr/Fmsy","Yield10yr/MSY","SSB10yr/SSBmsy","B10yr/Bmsy","F10yr/Fmsy","Yield20yr/MSY","SSB20yr/SSBmsy","B20yr/Bmsy","F20yr/Fmsy")
      outMatrixColNames<-as.data.frame(matrix(c(outVectColNames),nrow=1,ncol=40,byrow = TRUE))
      write.table(outMatrixColNames,file=paste0(dir.base,"/Kobe_Matrix.csv"),row.names = FALSE, col.names = FALSE, append=FALSE, sep=",", dec=".")

      for(i in 1:length(input$CompAssessments))
      {
        dir.temp<-input$CompAssessments[i]
        dir.temp<-paste0(dir.temp,"/original")
        print(dir.temp)
        unlink(dir.run,recursive = TRUE, force=TRUE)
        dir.create(dir.run)
        files.temp<-list.files(dir.temp)
        print(files.temp)
        file.copy(paste0(dir.temp,"/",files.temp),dir.run)
        incProgress(0, detail = "Reading starter file")
        starter.run<<-starter.orig<<-rd_starter(paste0(dir.temp,"/starter.ss"))
        incProgress(0, detail = "Reading data file")
        data.run<<-data.orig<<-rd_data(file=paste0(dir.temp,"/",starter.orig$datfile))
        incProgress(0, detail = "Reading control file")
        control.run<<-control.orig<<-rd_ctl(file=paste(dir.temp,"/",starter.orig$ctlfile,sep=""),data.orig)
        incProgress(0, detail = "Reading forecast file")
        forecast.run<<-forecast.orig<<-rd_forecast(file=paste0(dir.temp,"/forecast.ss"),Nfleets=data.orig$Nfleet,Nareas = data.orig$N_areas,Nseas = data.orig$nseas)
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
        pars.run<<-pars.orig<<-rd_par(file=paste(dir.temp,"/ss3.par",sep=""))
        disp.Desc<<-rd_display(file=paste(dir.temp,"/Display_Descriptions.txt",sep=""),forecast.orig,data.orig)
        numCols<<-200
        output.read<-FALSE
        output.temp<-NULL
        incProgress(0.1, detail = "Reading output file")
        while(output.read==FALSE){
          try({output.temp<-rd_output(dir.temp, covar=F, ncol=numCols)})
          if(is.null(output.temp)){
            output.read<-FALSE
            numCols<<-numCols+100
          }else{
            output.read<-TRUE
            output.run<<-output.orig<<-output.temp
          }
        }

        incProgress(0.1, detail="reading base catch")
        baseCatch<<-ReadCatch(output.orig, (data.orig$styr-1):(data.orig$endyr+forecast.orig$Nforecastyrs))
        projCatch<<-aggregate(baseCatch,list(year=baseCatch[,1],fleet=baseCatch[,4],season=baseCatch[,2]),sum)
        projCatch$Fleet<<-forecast.orig$fleet_assignment_to_allocation_group[projCatch$fleet]
        projCatch<<-projCatch[,c(1,2,3,7,8,9,10,11,13)]
        projCatch[,4:5]<<-projCatch[,4:5]*weightScale[disp.Desc$Units[1]]#2.20462

        duplicateFleets(input,output,session)

        RunTargetForecast(input,output,session)
        file.copy(from=paste0(dir.run,"/report.sso"),to=paste0(dir.base,"/",i,"TargReport.sso"),overwrite=TRUE)


        RunAppliedForecast(input,output,session,kobeComp=i)

      }
    }
  })
}

#' Read management input
#'
#' Reads the user input management values from
#' the shiny GUI for modification of the
#' assessment projection assumptions.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords assessment projection
getMangementLimits<-function(input, output, session){
  incProgress(0.0, detail = "Processing data") #0.1 total
  CatchManage<-baseCatch
  CatchManage<-aggregate(CatchManage,list(year=CatchManage[,1],fleet=CatchManage[,4],seas=CatchManage[,2]),sum)
  CatchManage$Fleet<-forecast.orig$fleet_assignment_to_allocation_group[CatchManage$fleet]
  CatchManage<-CatchManage[,c(1,2,3,8,9,10,11,13,7)]
  newNumForecastYrs<-forecast.orig$Nforecastyrs
  managementLimits<-list()
  managementLimits$sizeLimits<-matrix(NA,nrow=data.orig$Nfleet,ncol=4)
  managementLimits$pastCatch<-matrix(NA,nrow=2*data.orig$Nfleet*newNumForecastYrs*data.orig$nseas,ncol=5,dimnames = list(NULL,c(names(forecast.orig$ForeCatch))))
  keep<-vector()
  incProgress(0.1, detail = "Retrieving management limits") #0.2 total
  incProgress(0.01, detail = "Retrieving management limits") #0.2 total
  for(ii in 1:data.orig$Nfleet){
    if(!is.null(eval(parse(text=paste0("input$fleet",ii,"Lc"))))){
      managementLimits$sizeLimits[ii,1]<-as.numeric(eval(parse(text=paste0("input$fleet",ii,"Lc"))))/lengthScale[disp.Desc$Units[2]]#*2.54
    }
    if(!is.null(eval(parse(text=paste0("input$fleet",ii,"discMortSlider"))))){
      managementLimits$sizeLimits[ii,2]<-as.numeric(eval(parse(text=paste0("input$fleet",ii,"discMortSlider"))))
    }
    if(!is.null(eval(parse(text=paste0("input$fleet",ii,"retentionMaxSlider"))))){
      managementLimits$sizeLimits[ii,3]<-as.numeric(eval(parse(text=paste0("input$fleet",ii,"retentionMaxSlider"))))
    }
    if(!is.null(eval(parse(text=paste0("input$fleet",ii,"Lcslope"))))){
      managementLimits$sizeLimits[ii,4]<-as.numeric(eval(parse(text=paste0("input$fleet",ii,"Lcslope"))))
    }
    if(forecast.orig$fleet_assignment_to_allocation_group[ii]!=9999){
      keep<-c(keep,ii)
    }
    if(newNumForecastYrs>0){
      for(iii in 1:data.orig$nseas){
        for(iiii in 1:newNumForecastYrs){
          currRowOrig<-(iiii-1)*data.orig$nseas*data.orig$Nfleet*2+(iii-1)*data.orig$Nfleet*2+ii
          currRowDummy<-(iiii-1)*data.orig$nseas*data.orig$Nfleet*2+(iii-1)*data.orig$Nfleet*2+data.orig$Nfleet+ii

          managementLimits$pastCatch[currRowOrig,]<-c((iiii+data.orig$endyr),iii,(ii),NA,NA)
          managementLimits$pastCatch[currRowDummy,]<-c((iiii+data.orig$endyr),iii,(ii+data.orig$Nfleet),NA,NA)

          if(iiii<(as.integer(input$ManagementYearInput)-data.orig$endyr)){
            managementLimits$pastCatch[currRowDummy,4:5]<-c(0,99)
            if(!is.null(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuant",iiii))))){
              if(length(as.numeric(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuant",iiii)))))!=0){
                managementLimits$pastCatch[currRowOrig,4]<-as.numeric(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuant",iiii))))
                if(iiii<3){}
              }
              if(is.na(managementLimits$pastCatch[currRowOrig,4])){
                if(data.orig$units_of_catch[ii]==1){
                  managementLimits$pastCatch[currRowOrig,4:5]<-c(CatchManage[(CatchManage[,1]==(data.orig$endyr+iiii) & CatchManage[,2]==ii & CatchManage[,3]==iii),4],3)
                }else if(data.orig$units_of_catch[ii]==2){
                  managementLimits$pastCatch[currRowOrig,4:5]<-c(CatchManage[(CatchManage[,1]==(data.orig$endyr+iiii) & CatchManage[,2]==ii & CatchManage[,3]==iii),6],6)
                }
              }else{
                checkInput<-forecast.orig$ForeCatch[forecast.orig$ForeCatch[,3]==ii,,drop=FALSE]
                if(data.orig$units_of_catch[ii]==1){
                  managementLimits$pastCatch[currRowOrig,4]<- managementLimits$pastCatch[currRowOrig,4]/weightScale[disp.Desc$Units[1]]
                }
                if(length(checkInput[,1])==0){
                  if(data.orig$units_of_catch[ii]==1){
                    managementLimits$pastCatch[currRowOrig,5]<-3
                  }else if(data.orig$units_of_catch[ii]==2){
                    managementLimits$pastCatch[currRowOrig,5]<-6
                  }
                }else{
                  managementLimits$pastCatch[currRowOrig,5]<-checkInput[1,5]
                }
              }
            }else{
              checkInput<-forecast.orig$ForeCatch[forecast.orig$ForeCatch[,3]==ii,,drop=FALSE]
              checkInput2<-checkInput[checkInput[,1]==(data.orig$endyr+iiii) & checkInput[,2]==iii,,drop=FALSE]
              if(length(checkInput2[,1])==0){
                if(data.orig$units_of_catch[ii]==1){
                  managementLimits$pastCatch[currRowOrig,4:5]<-c(CatchManage[(CatchManage[,1]==(data.orig$endyr+iiii) & CatchManage[,2]==ii & CatchManage[,3]==iii),4],3)
                }else if(data.orig$units_of_catch[ii]==2){
                  managementLimits$pastCatch[currRowOrig,4:5]<-c(CatchManage[(CatchManage[,1]==(data.orig$endyr+iiii) & CatchManage[,2]==ii & CatchManage[,3]==iii),6],6)
                }
              }else{
                managementLimits$pastCatch[currRowOrig,4:5]<-c(checkInput2[1,4],checkInput2[1,5])
              }
            }
            if(iiii<3){}
          }else{
            managementLimits$pastCatch[currRowOrig,4:5]<-c(0,99)
            if(!is.null(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuant",iiii))))){
              checkInput<-forecast.orig$ForeCatch[forecast.orig$ForeCatch[,3]==ii,,drop=FALSE]
              managementLimits$pastCatch[currRowDummy,4]<-as.numeric(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuant",iiii))))

              if(length(checkInput[,1])==0){
                if(data.orig$units_of_catch[ii]==1){
                  managementLimits$pastCatch[currRowDummy,4]<-managementLimits$pastCatch[currRowDummy,4]/weightScale[disp.Desc$Units[1]]
                  managementLimits$pastCatch[currRowDummy,5]<-3
                }else if(data.orig$units_of_catch[ii]==2){
                  managementLimits$pastCatch[currRowDummy,5]<-6
                }
              }else{
                managementLimits$pastCatch[currRowDummy,5]<-checkInput[1,5]
                if(checkInput[1,5]==2 | checkInput[1,5]==3){
                  managementLimits$pastCatch[currRowDummy,4]<-managementLimits$pastCatch[currRowDummy,4]/weightScale[disp.Desc$Units[1]]
                }
              }
            }else{
              checkInput<-forecast.orig$ForeCatch[forecast.orig$ForeCatch[,3]==ii,,drop=FALSE]
              checkInput2<-checkInput[checkInput[,1]==(data.orig$endyr+iiii) & checkInput[,2]==iii,,drop=FALSE]
              if(length(checkInput2[,1])==0){
                managementLimits$pastCatch[currRowDummy,4]<-NA
                if(is.na(managementLimits$pastCatch[currRowDummy,4])){
                  if(!is.null(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuantAll"))))){
                    if(!is.na(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuantAll"))))){
                      if(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuantAll")))!=""){
                        managementLimits$pastCatch[currRowDummy,4]<-as.numeric(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuantAll"))))
                      }
                    }
                  }
                }
                if(length(checkInput[,1])==0){
                  if(data.orig$units_of_catch[ii]==1){
                    managementLimits$pastCatch[currRowDummy,4]<-managementLimits$pastCatch[currRowDummy,4]/weightScale[disp.Desc$Units[1]]
                    managementLimits$pastCatch[currRowDummy,5]<-3
                  }else if(data.orig$units_of_catch[ii]==2){
                    managementLimits$pastCatch[currRowDummy,5]<-6
                  }
                }else{
                  managementLimits$pastCatch[currRowDummy,5]<-checkInput[1,5]
                  if(checkInput[1,5]==2 |checkInput[1,5]==3){
                    managementLimits$pastCatch[currRowDummy,4]<-managementLimits$pastCatch[currRowDummy,4]/weightScale[disp.Desc$Units[1]]
                  }
                }
              }else{
                managementLimits$pastCatch[currRowDummy,4]<-checkInput2[1,4]

                if(!is.null(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuantAll"))))){
                  if(!is.na(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuantAll"))))){
                    if(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuantAll")))!=""){
                      managementLimits$pastCatch[currRowDummy,4]<-as.numeric(eval(parse(text=paste0("input$fleet",ii,"seas",iii,"recentCatchQuantAll"))))
                      if(checkInput2[1,5]==2 |checkInput2[1,5]==3){
                        managementLimits$pastCatch[currRowDummy,4]<-managementLimits$pastCatch[currRowDummy,4]/weightScale[disp.Desc$Units[1]]
                      }
                    }
                  }
                }
                managementLimits$pastCatch[currRowDummy,5]<-checkInput2[1,5]
              }
            }
          }
        }
      }
    }
  }
  managementLimits$fullCatch<-managementLimits$pastCatch
  managementLimits$pastCatch<-managementLimits$pastCatch[!is.na(managementLimits$pastCatch[,4]),,drop=FALSE]
  managementLimits$sizeLimits<-managementLimits$sizeLimits[keep,,drop=FALSE]
  return(managementLimits)
}

#' Duplicate fleets
#'
#' This function allows projection of fishery
#' scenarios where fishing allocation and selectivity
#' do not match the historical record. It achieves
#' this goal by replicating all fishing fleets with
#' "dummy" fleets and setting their historic catch to
#' E10-7 times the true fleets catch. Allocation is
#' then shifted to these fleets and their selectivity
#' patterns can be freely modified.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords assessment projection
duplicateFleets<-function(input, output, session){
  #print("Getting started")
  data.run<<-data.orig
  control.run<<-control.orig
  pars.run<<-pars.orig
  forecast.run<<-forecast.orig
  output.run<<-output.orig
  managementLimits<<-getMangementLimits(input, output, session)
  sizeLimits<-managementLimits$sizeLimits

  sizeFleet<-NULL
  sizeSurvey<-NULL
  ageFleet<-NULL
  ageSurvey<-NULL
  selectParmsValCurr<-0

  incProgress(0.1, detail = "Building dummy selection fleets") #0.3 total
  pattern<-matrix(nrow=34,ncol=2)
  pattern[,1]<-c(2,8,6,0,2,2,8,8,6,0,2,2,8,(data.orig$Nages+1),0,2,(data.orig$Nages+1),8,6,6,0,4,6,6,3,3,3,0,0,0,0,0,0,0)
  pattern[,2]<-c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0)

  rowsCopyCtrl<-NULL
  #RowsTVCtrl<-NULL
  row<-0
  Copy.Row<-NULL
  Edit.Row.TV.Env<-NULL
  Edit.Row.TV.Dev<-NULL
  Edit.Row.TV.Blk<-NULL
  Edit.Row.Lc<-NULL
  Edit.Row.Slope<-NULL
  Edit.Row.Discard<-NULL
  Edit.Row.Retain<-NULL
  Add.Row<-NULL
  rowFleetLength<-NULL
  rowFleetAge<-NULL
  rowSurveyLength<-NULL
  rowSurveyAge<-NULL
  Fleets<-1:data.orig$Nfleet

  marker<-0
  #print(paste("reached point ",marker))
  marker<-marker+1

  for(i in 1:length(control.orig$Size_Select[,1]))
  {
    if(control.orig$Size_Select[i,1]!=0)
    {
      if(pattern[control.orig$Size_Select[i,1],1]!=0)
      {
        nParams<-pattern[control.orig$Size_Select[i,1],1]+pattern[control.orig$Size_Select[i,1],2]*control.orig$Size_Select[i,4]
        for(j in 1:nParams)
        {
          row<-row+1
          if(is.element(i,Fleets))
          {
            rowFleetLength<-c(rowFleetLength,TRUE)
            rowSurveyLength<-c(rowSurveyLength,FALSE)
            rowsCopyCtrl<-c(rowsCopyCtrl,row)
            Copy.Row<-c(Copy.Row,TRUE)
          }else{
            Copy.Row<-c(Copy.Row,FALSE)
            rowFleetLength<-c(rowFleetLength,FALSE)
            rowSurveyLength<-c(rowSurveyLength,TRUE)
          }
          rowFleetAge<-c(rowFleetAge,FALSE)
          rowSurveyAge<-c(rowSurveyAge,FALSE)
          Edit.Row.Lc<-c(Edit.Row.Lc,FALSE)
          Edit.Row.Slope<-c(Edit.Row.Slope,FALSE)
          Edit.Row.Discard<-c(Edit.Row.Discard,FALSE)
          Edit.Row.Retain<-c(Edit.Row.Retain,FALSE)
          if(control.orig$Select_Params[row,8]!=0){
            Edit.Row.TV.Env<-c(Edit.Row.TV.Env,TRUE)
          }else{
            Edit.Row.TV.Env<-c(Edit.Row.TV.Env,FALSE)
          }
          if(control.orig$Select_Params[row,9]!=0){
            Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,TRUE)
          }else{
            Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,FALSE)
          }
          if(control.orig$Select_Params[row,13]!=0){
            Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,TRUE)
          }else{
            Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,FALSE)
          }
        }
      }
    }

    if(is.element(i,Fleets))
    {
      if(control.orig$Size_Select[i,2]==1 | control.orig$Size_Select[i,2]==2)
      {
        for(j in 1:4)
        {
          row<-row+1
          rowsCopyCtrl<-c(rowsCopyCtrl,row)
          Copy.Row<-c(Copy.Row,TRUE)
          rowFleetLength<-c(rowFleetLength,TRUE)
          rowFleetAge<-c(rowFleetAge,FALSE)
          rowSurveyLength<-c(rowSurveyLength,FALSE)
          rowSurveyAge<-c(rowSurveyAge,FALSE)
          Edit.Row.Discard<-c(Edit.Row.Discard,FALSE)
          if(j==1)
          {
            Edit.Row.Lc<-c(Edit.Row.Lc,TRUE)
            Edit.Row.Retain<-c(Edit.Row.Retain,FALSE)
            Edit.Row.Slope<-c(Edit.Row.Slope,FALSE)
          }else if(j==2){
            Edit.Row.Slope<-c(Edit.Row.Slope,TRUE)
            Edit.Row.Lc<-c(Edit.Row.Lc,FALSE)
            Edit.Row.Retain<-c(Edit.Row.Retain,FALSE)
          }else if(j==3){
            Edit.Row.Lc<-c(Edit.Row.Lc,FALSE)
            Edit.Row.Retain<-c(Edit.Row.Retain,TRUE)
            Edit.Row.Slope<-c(Edit.Row.Slope,FALSE)
          }else{
            Edit.Row.Lc<-c(Edit.Row.Lc,FALSE)
            Edit.Row.Retain<-c(Edit.Row.Retain,FALSE)
            Edit.Row.Slope<-c(Edit.Row.Slope,FALSE)
          }

          if(control.orig$Select_Params[row,8]!=0){
            Edit.Row.TV.Env<-c(Edit.Row.TV.Env,TRUE)
          }else{
            Edit.Row.TV.Env<-c(Edit.Row.TV.Env,FALSE)
          }
          if(control.orig$Select_Params[row,9]!=0){
            Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,TRUE)
          }else{
            Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,FALSE)
          }
          if(control.orig$Select_Params[row,13]!=0){
            Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,TRUE)
          }else{
            Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,FALSE)
          }
        }
      }

      if(control.orig$Size_Select[i,2]==2)
      {
        for(j in 1:4)
        {
          row<-row+1

          rowsCopyCtrl<-c(rowsCopyCtrl,row)
          Copy.Row<-c(Copy.Row,TRUE)
          rowFleetLength<-c(rowFleetLength,TRUE)
          rowFleetAge<-c(rowFleetAge,FALSE)
          rowSurveyLength<-c(rowSurveyLength,FALSE)
          rowSurveyAge<-c(rowSurveyAge,FALSE)
          Edit.Row.Slope<-c(Edit.Row.Slope,FALSE)
          Edit.Row.Lc<-c(Edit.Row.Lc,FALSE)
          Edit.Row.Retain<-c(Edit.Row.Retain,FALSE)
          if(j==3)
          {
            Edit.Row.Discard<-c(Edit.Row.Discard,TRUE)
          }else{
            Edit.Row.Discard<-c(Edit.Row.Discard,FALSE)
          }

          if(control.orig$Select_Params[row,8]!=0){
            Edit.Row.TV.Env<-c(Edit.Row.TV.Env,TRUE)
          }else{
            Edit.Row.TV.Env<-c(Edit.Row.TV.Env,FALSE)
          }
          if(control.orig$Select_Params[row,9]!=0){
            Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,TRUE)
          }else{
            Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,FALSE)
          }
          if(control.orig$Select_Params[row,13]!=0){
            Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,TRUE)
          }else{
            Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,FALSE)
          }
        }
      }
    }

    if(control.orig$Size_Select[i,3]>0)
    {
      if(control.orig$Size_Select[i,3]<3){nSSpar<-4}else{if(control.orig$Size_Select[i,1]==1){nSSpar<-3}else{nSSpar<-5}}

      for(j in 1:nSSpar)
      {
        row<-row+1
        if(is.element(i,Fleets))
        {
          rowFleetLength<-c(rowFleetLength,TRUE)
          rowSurveyLength<-c(rowSurveyLength,FALSE)
          rowsCopyCtrl<-c(rowsCopyCtrl,row)
          Copy.Row<-c(Copy.Row,TRUE)
        }else{Copy.Row<-c(Copy.Row,FALSE)
        rowFleetLength<-c(rowFleetLength,FALSE)
        rowSurveyLength<-c(rowSurveyLength,TRUE)
        }
        rowFleetAge<-c(rowFleetAge,FALSE)
        rowSurveyAge<-c(rowSurveyAge,FALSE)
        Edit.Row.Slope<-c(Edit.Row.Slope,FALSE)
        Edit.Row.Lc<-c(Edit.Row.Lc,FALSE)
        Edit.Row.Discard<-c(Edit.Row.Discard,FALSE)
        Edit.Row.Retain<-c(Edit.Row.Retain,FALSE)

        if(control.orig$Select_Params[row,8]!=0){
          Edit.Row.TV.Env<-c(Edit.Row.TV.Env,TRUE)
        }else{
          Edit.Row.TV.Env<-c(Edit.Row.TV.Env,FALSE)
        }
        if(control.orig$Select_Params[row,9]!=0){
          Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,TRUE)
        }else{
          Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,FALSE)
        }
        if(control.orig$Select_Params[row,13]!=0){
          Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,TRUE)
        }else{
          Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,FALSE)
        }
      }
    }
  }

  for(i in 1:length(control.orig$Age_Select[,1]))
  {
    if(control.orig$Age_Select[i,1]!=0)
    {
      if(pattern[control.orig$Age_Select[i,1],1]!=0)
      {
        nParams<-pattern[control.orig$Age_Select[i,1],1]+pattern[control.orig$Age_Select[i,1],2]*control.orig$Age_Select[i,4]
        for(j in 1:nParams)
        {
          row<-row+1
          if(is.element(i,Fleets))
          {
            rowFleetAge<-c(rowFleetAge,TRUE)
            rowSurveyAge<-c(rowSurveyAge,FALSE)
            rowsCopyCtrl<-c(rowsCopyCtrl,row)
            Copy.Row<-c(Copy.Row,TRUE)
          }else{Copy.Row<-c(Copy.Row,FALSE)
          rowFleetAge<-c(rowFleetAge,FALSE)
          rowSurveyAge<-c(rowSurveyAge,TRUE)}

          rowFleetLength<-c(rowFleetLength,FALSE)
          rowSurveyLength<-c(rowSurveyLength,FALSE)

          Edit.Row.Lc<-c(Edit.Row.Lc,FALSE)
          Edit.Row.Slope<-c(Edit.Row.Slope,FALSE)
          Edit.Row.Discard<-c(Edit.Row.Discard,FALSE)
          Edit.Row.Retain<-c(Edit.Row.Retain,FALSE)

          if(control.orig$Select_Params[row,8]!=0){
            Edit.Row.TV.Env<-c(Edit.Row.TV.Env,TRUE)
          }else{
            Edit.Row.TV.Env<-c(Edit.Row.TV.Env,FALSE)
          }
          if(control.orig$Select_Params[row,9]!=0){
            Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,TRUE)
          }else{
            Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,FALSE)
          }
          if(control.orig$Select_Params[row,13]!=0){
            Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,TRUE)
          }else{
            Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,FALSE)
          }
        }
      }
    }
    if(control.orig$Age_Select[i,3]>0)
    {
      if(control.orig$Age_Select[i,3]<3){nSSpar<-4}else{if(control.orig$Age_Select[i,1]==1){nSSpar<-3}else{nSSpar<-5}}

      for(j in 1:nSSpar)
      {
        row<-row+1
        if(is.element(i,Fleets))
        {
          rowFleetAge<-c(rowFleetAge,TRUE)
          rowSurveyAge<-c(rowSurveyAge,FALSE)
          rowsCopyCtrl<-c(rowsCopyCtrl,row)
          Copy.Row<-c(Copy.Row,TRUE)
        }else{Copy.Row<-c(Copy.Row,FALSE)
        rowFleetAge<-c(rowFleetAge,FALSE)
        rowSurveyAge<-c(rowSurveyAge,TRUE)}

        rowFleetLength<-c(rowFleetLength,FALSE)
        rowSurveyLength<-c(rowSurveyLength,FALSE)
        Edit.Row.Lc<-c(Edit.Row.Lc,FALSE)
        Edit.Row.Slope<-c(Edit.Row.Slope,FALSE)
        Edit.Row.Discard<-c(Edit.Row.Discard,FALSE)
        Edit.Row.Retain<-c(Edit.Row.Retain,FALSE)

        if(control.orig$Select_Params[row,8]!=0){
          Edit.Row.TV.Env<-c(Edit.Row.TV.Env,TRUE)
        }else{
          Edit.Row.TV.Env<-c(Edit.Row.TV.Env,FALSE)
        }
        if(control.orig$Select_Params[row,9]!=0){
          Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,TRUE)
        }else{
          Edit.Row.TV.Dev<-c(Edit.Row.TV.Dev,FALSE)
        }
        if(control.orig$Select_Params[row,13]!=0){
          Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,TRUE)
        }else{
          Edit.Row.TV.Blk<-c(Edit.Row.TV.Blk,FALSE)
        }
      }
    }
  }

  #print(paste("reached point ",marker))
  #marker<-marker+1
  # temp2<-output.orig$SelAgeAdj[output.orig$SelAgeAdj>=forecast.orig$Fcast_years[1] & output.orig$SelAgeAdj<=forecast.orig$Fcast_years[2],]
  # temp2<-aggregate(temp2,list(fleet=temp2[,1]),mean)
  # temp2<-as.matrix(temp2[,-c(1,2)])
  # temp2<-c(t(temp2))
  #Add references to dummy fleets in parameter file
  incProgress(0.0, detail = "Modifying parameter file") #0.3 total

  rowsInitF<-grep("# init_F",pars.run$Labels)
  pars.run$Labels<<-c(pars.run$Labels[1:max(rowsInitF)],pars.run$Labels[rowsInitF[Fleets]],pars.run$Labels[(max(rowsInitF)+1):length(pars.run$Labels)])
  pars.run$Values<<-c(pars.run$Values[1:(max(rowsInitF)-1)],unlist(pars.run$Values[(rowsInitF[Fleets]-1)])*0.0000000001,pars.run$Values[(max(rowsInitF)):length(pars.run$Values)])

  rowsFrate<-grep("# F_rate",pars.run$Labels)
  if(length(rowsFrate)>0)
  {
    fleetRows<-c(t(outer((Fleets-1)*(data.orig$endyr-data.orig$styr+1),1:(data.orig$endyr-data.orig$styr+1),"+")))
    pars.run$Labels<<-c(pars.run$Labels[1:max(rowsFrate)],paste("# F_rate[",(seq((length(rowsFrate)+1),(length(rowsFrate)+length(Fleets)*(data.orig$endyr-data.orig$styr+1)),1)),"]:",sep=""),pars.run$Labels[(max(rowsFrate)+1):length(pars.run$Labels)])
    pars.run$Values<<-c(pars.run$Values[1:(max(rowsFrate)-1)],unlist(pars.run$Values[(rowsFrate[fleetRows]-1)])*0.0000000001,pars.run$Values[(max(rowsFrate)):length(pars.run$Values)])
  }

  rowsSelect<-grep("# selparm",pars.run$Labels)
  rowsSelectTV<-NULL
  if(length(rowsSelect)>row)
  {
    rowsSelectTV<-rowsSelect[(row+1):length(rowsSelect)]
    rowsSelect<-rowsSelect[1:row]
  }

  newLabels<-pars.run$Labels[(rowsSelect)]
  oldLabels<-pars.run$Labels[(rowsSelect)]
  newVals<-pars.run$Values[(rowsSelect-1)]
  oldVals<-pars.run$Values[(rowsSelect-1)]

  newLabelsTV<-pars.run$Labels[(rowsSelectTV)]
  oldLabelsTV<-pars.run$Labels[(rowsSelectTV)]
  newValsTV<-pars.run$Values[(rowsSelectTV-1)]
  oldValsTV<-pars.run$Values[(rowsSelectTV-1)]

  #Edit.Row.TV.Env
  rowsTV.Env<-rowsSelect[Edit.Row.TV.Env]
  rowsTV.Env.Copy<-Copy.Row[Edit.Row.TV.Env]
  numPar.TV.Env<-0
  curr.Par<-0
  TVValsAddENV<-NULL
  if(length(rowsTV.Env)>0)
  {
    numPar.TV.Env<-length(control.orig$Select_Params_TV$ENV_Params[,1])
    if(control.orig$Select_Params_TV$Cust_ENV==1)
    {
      for(tve in 1:numPar.TV.Env)
      {
        curr.Par<-curr.Par+1
        if(rowsTV.Env.Copy[tve])
        {
          TVValsAddENV<-c(TVValsAddENV+tve)
        }
      }
    }else{
      curr.Par<-1
    }
  }

  #Edit.Row.TV.Blk
  rowsTV.Blk<-rowsSelect[Edit.Row.TV.Blk]
  rowsTV.Blk<-rowsTV.Blk-rowsSelect[1]+1
  numPar.TV.Blk<-0
  Select_Params_TV_BLK<-control.run$Select_Params[Edit.Row.TV.Blk,]
  if(length(rowsTV.Blk)>0)
  {
    if(control.orig$Select_Params_TV$Cust_Blk==1)
    {
      for(tvb in rowsTV.Blk)
      {
        if(control.run$Select_Params[tvb,13]==-1)
        {
          numPar.TV.Blk<-numPar.TV.Blk+3
          newVals[tvb]<-as.numeric(newVals[tvb])+exp(as.numeric(newValsTV[(curr.Par+1)]))
          curr.Par<-curr.Par+3
        }else if(control.run$Select_Params[tvb,13]==-2)
        {
          numPar.TV.Blk<-numPar.TV.Blk+3
          newVals[tvb]<-newValsTV[(curr.Par+1)]
          curr.Par<-curr.Par+3
        }else if(control.run$Select_Params[tvb,13]>0)
        {
          numBlocks<-control.orig$BlocksPerPattern[control.run$Select_Params[tvb,13]]
          numPar.TV.Blk<-numPar.TV.Blk+numBlocks

          if(control.orig$BlockPatterns[[control.run$Select_Params[tvb,13]]][2*numBlocks]>=data.orig$endyr)
          {
            if(control.run$Select_Params[tvb,14]==0)
            {
              curr.Par<-curr.Par+numBlocks
              newVals[tvb]<-as.numeric(newVals[tvb])*exp(as.numeric(newValsTV[curr.Par]))
            }
            if(control.run$Select_Params[tvb,14]==1)
            {
              curr.Par<-curr.Par+numBlocks
              newVals[tvb]<-as.numeric(newVals[tvb])+as.numeric(newValsTV[curr.Par])
            }
            if(control.run$Select_Params[tvb,14]==2)
            {
              curr.Par<-curr.Par+numBlocks
              newVals[tvb]<-newValsTV[curr.Par]
            }
            if(control.run$Select_Params[tvb,14]==3)
            {
              for(nBk in 1:numBlocks)
              {
                curr.Par<-curr.Par+1
                newVals[tvb]<-as.numeric(newVals[tvb])+as.numeric(newValsTV[curr.Par])
              }
            }
          }else{
            curr.Par<-curr.Par+numBlocks
          }
        }
      }
    }else{
      numPar.TV.Blk<-1
      curr.Par<-curr.Par+1
    }
  }

  temp<-output.orig$SelSizeAdj[output.orig$SelSizeAdj[,2]>=forecast.orig$Fcast_years[1] & output.orig$SelSizeAdj[,2]<=forecast.orig$Fcast_years[2],]
  temp<-aggregate(temp,list(fleet=temp[,1]),mean)
  temp<-as.matrix(temp[,-c(1,2,3)])
  temp<-c(t(temp))
  temp<-temp[!is.na(temp)]
  newVals[1:length(temp)]<-temp

  #I don't do anything at the moment for Time Varying Parameter Devs because I don't know how the are implemented
  #in the Par file so this is a work in progress I will have to experiment with later when I have time. As such I
  #will shut off time varying devs for copied fleets I don't think they extend forward into the projection phase anyway.
  #Here I just roll in any left over select Params as Dev params not sure if this is correct or not but should work sort of.

  newVals[Edit.Row.Lc]<-ifelse(!is.na(sizeLimits[,1]),sizeLimits[,1],newVals[Edit.Row.Lc])
  newVals[Edit.Row.Slope]<-ifelse(!is.na(sizeLimits[,4]),sizeLimits[,4],newVals[Edit.Row.Slope])
  newVals[Edit.Row.Discard]<-ifelse(!is.na(sizeLimits[,2]),sizeLimits[,2],newVals[Edit.Row.Discard])
  newVals[Edit.Row.Retain]<-ifelse(!is.na(sizeLimits[,3]),sizeLimits[,3],newVals[Edit.Row.Retain])
  if(!is.null(rowsSelectTV))
  {
    if(length(pars.run$Labels)>max(rowsSelectTV))
    {
      pars.run$Labels<<-c(pars.run$Labels[1:(min(rowsSelect)-1)],oldLabels[rowFleetLength],newLabels[rowFleetLength],oldLabels[rowSurveyLength],oldLabels[rowFleetAge],newLabels[rowFleetAge],oldLabels[rowSurveyAge],
                          pars.run$Labels[(max(rowsSelectTV)+1):length(pars.run$Labels)])
      pars.run$Values<<-c(pars.run$Values[1:(min(rowsSelect)-2)],oldVals[rowFleetLength],newVals[rowFleetLength],oldVals[rowSurveyLength],oldVals[rowFleetAge],newVals[rowFleetAge],oldVals[rowSurveyAge],
                          pars.run$Values[(max(rowsSelectTV)):length(pars.run$Values)])
    }else{
      if(numPar.TV.Env>0){
        pars.run$Labels<<-c(pars.run$Labels[1:(min(rowsSelect)-1)],oldLabels[rowFleetLength],newLabels[rowFleetLength],oldLabels[rowSurveyLength],oldLabels[rowFleetAge],newLabels[rowFleetAge],oldLabels[rowSurveyAge],
                            oldLabelsTV[1:numPar.TV.Env],newLabelsTV[TVValsAddENV],oldLabelsTV[-c(1:numPar.TV.Env)])
        pars.run$Values<<-c(pars.run$Values[1:(min(rowsSelect)-2)],oldVals[rowFleetLength],newVals[rowFleetLength],oldVals[rowSurveyLength],oldVals[rowFleetAge],newVals[rowFleetAge],oldVals[rowSurveyAge],
                            oldValsTV[1:numPar.TV.Env],newValsTV[TVValsAddENV],oldValsTV[-c(1:numPar.TV.Env)])
      }else{
        pars.run$Labels<<-c(pars.run$Labels[1:(min(rowsSelect)-1)],oldLabels[rowFleetLength],newLabels[rowFleetLength],oldLabels[rowSurveyLength],oldLabels[rowFleetAge],newLabels[rowFleetAge],oldLabels[rowSurveyAge],
                            oldLabelsTV)
        pars.run$Values<<-c(pars.run$Values[1:(min(rowsSelect)-2)],oldVals[rowFleetLength],newVals[rowFleetLength],oldVals[rowSurveyLength],oldVals[rowFleetAge],newVals[rowFleetAge],oldVals[rowSurveyAge],
                            oldValsTV)
      }
    }
  }else{
    pars.run$Labels<<-c(pars.run$Labels[1:(min(rowsSelect)-1)],oldLabels[rowFleetLength],newLabels[rowFleetLength],oldLabels[rowSurveyLength],oldLabels[rowFleetAge],newLabels[rowFleetAge],oldLabels[rowSurveyAge],
                        oldLabelsTV)
    pars.run$Values<<-c(pars.run$Values[1:(min(rowsSelect)-2)],oldVals[rowFleetLength],newVals[rowFleetLength],oldVals[rowSurveyLength],oldVals[rowFleetAge],newVals[rowFleetAge],oldVals[rowSurveyAge],
                        oldValsTV)
  }


  if(length(c(control.run$Q)[c(control.run$Q)>0])>0)
  {
    oldQrows<-grep("# Q_parm",pars.run$Labels)
    randQsplits<-vector(length=8)
    tempQF<-control.run$Q[1:data.run$Nfleet,1]
    tempQS<-control.run$Q[-c(1:data.run$Nfleet),1]
    randQsplits[1]<-length(tempQF[tempQF>0])
    randQsplits[2]<-length(tempQS[tempQS>0])
    tempQF<-control.run$Q[1:data.run$Nfleet,2]
    tempQS<-control.run$Q[-c(1:data.run$Nfleet),2]
    randQsplits[3]<-length(tempQF[tempQF>0])
    randQsplits[4]<-length(tempQS[tempQS>0])
    tempQF<-control.run$Q[1:data.run$Nfleet,3]
    tempQS<-control.run$Q[-c(1:data.run$Nfleet),3]
    randQsplits[5]<-length(tempQF[tempQF>0])
    randQsplits[6]<-length(tempQS[tempQS>0])
    tempQF<-control.run$Q[1:data.run$Nfleet,4]
    tempQS<-control.run$Q[-c(1:data.run$Nfleet),4]
    randQsplits[7]<-length(tempQF[tempQF>0])
    randQsplits[8]<-length(tempQS[tempQS>0])

    usedSoFar<-0
    newQrows<-vector()
    for(Qi in 1:8)
    {
      if(randQsplits[Qi]>0)
      {
        if(max(Qi==c(1,3,5,7))==1)
        {
          newQrows<-c(newQrows,oldQrows[(usedSoFar+1):(usedSoFar+randQsplits[Qi])],oldQrows[(usedSoFar+1):(usedSoFar+randQsplits[Qi])])
        }else{
          newQrows<-c(newQrows,oldQrows[(usedSoFar+1):(usedSoFar+randQsplits[Qi])])
        }
        usedSoFar<-usedSoFar+randQsplits[Qi]
      }
    }
    if(usedSoFar<length(oldQrows))
    {
      newQrows<-c(newQrows,oldQrows[(usedSoFar+1):length(oldQrows)])
    }
    pars.run$Labels<<-c(pars.run$Labels[1:(min(newQrows)-1)],pars.run$Labels[newQrows],pars.run$Labels[-c(1:(max(newQrows)))])
    pars.run$Values<<-c(pars.run$Values[1:(min(newQrows)-2)],pars.run$Values[(newQrows-1)],pars.run$Values[-c(1:(max(newQrows)-1))])
  }

  #print(paste("reached point ",marker))
  marker<-marker+1
  incProgress(0.0, detail = "Modifying control file") #0.3 total
  #Add references to dummy fleets in control file
  control.run$F_init<<-rbind(control.run$F_init,0.000001*control.run$F_init[Fleets,])
  #control.run$F_init[(length(control.run$F_init[,3])+1-length(Fleets)):length(control.run$F_init[,3]),3]<<-rep(0,length(Fleets))
  control.run$Q<<-rbind(control.run$Q[1:data.orig$Nfleet,],control.run$Q[Fleets,],control.run$Q[-(1:data.orig$Nfleet),])
  if(!is.null(control.run$Qrand)){
    control.run$Qrand<<-control.run$Qrand[(newQrows-min(newQrows)+1),]
  }
  control.run$Size_Select<<-rbind(control.run$Size_Select[1:data.orig$Nfleet,],control.run$Size_Select[Fleets,],control.run$Size_Select[-(1:data.orig$Nfleet),])
  control.run$Age_Select<<-rbind(control.run$Age_Select[1:data.orig$Nfleet,],control.run$Age_Select[Fleets,],control.run$Age_Select[-(1:data.orig$Nfleet),])
  if(!is.null(control.run$Select_Params_TV$Cust_ENV)){
    if(control.run$Select_Params_TV$Cust_ENV==1){
      control.run$Select_Params_TV$ENV_Params<-rbind(control.run$Select_Params_TV$ENV_Params,control.run$Select_Params_TV$ENV_Params[TVValsAddENV,])
    }
  }
  control.run$Lambda_Changes[,2]<<-ifelse(control.run$Lambda_Changes[,2]>data.orig$Nfleet,control.run$Lambda_Changes[,2]+data.orig$Nfleet,control.run$Lambda_Changes[,2])

  newParams<-control.run$Select_Params
  oldParams<-control.run$Select_Params
  newParams[,3]<-unlist(newVals)
  newParams[,2]<-rep(9999,length(newParams[,2]))
  oldParams[,2]<-rep(8888,length(oldParams[,2]))
  newParams[Edit.Row.Lc,3]<-ifelse(!is.na(sizeLimits[,1]),sizeLimits[,1],newParams[Edit.Row.Lc,3])
  newParams[Edit.Row.Slope,3]<-ifelse(!is.na(sizeLimits[,4]),sizeLimits[,4],newParams[Edit.Row.Slope,3])
  newParams[Edit.Row.Discard,3]<-ifelse(!is.na(sizeLimits[,2]),sizeLimits[,2],newParams[Edit.Row.Discard,3])
  newParams[Edit.Row.Retain,3]<-ifelse(!is.na(sizeLimits[,3]),sizeLimits[,3],newParams[Edit.Row.Retain,3])
  newParams[,5]<-rep(-1,length(newParams[,5]))
  newParams[,8]<-rep(0,length(newParams[,8]))
  newParams[,9]<-rep(0,length(newParams[,9]))
  newParams[,13]<-rep(0,length(newParams[,13]))
  newParams[,14]<-rep(0,length(newParams[,14]))
  newParams<-newParams[rowsCopyCtrl,]

  allRows<-1:row
  sizeRows<-grep("Size",row.names(control.run$Select_Params))

  # rowFleetLength<-NULL
  # rowFleetAge<-NULL
  # rowSurveyLength<-NULL
  # rowSurveyAge<-NULL

  if(data.orig$Nsurveys>0){
    sizeFleetRows<-sizeRows
    for(sFR in 1:data.orig$Nsurveys)
    {
      sizeFleetRows<-sizeFleetRows[-grep(paste("fleet/Survey    2",(data.orig$Nfleet+sFR),sep=""),row.names(control.run$Select_Params[sizeFleetRows,]))]
    }
    #sizeFleetRows<-
    #sizeFleetRows<-sizeRows[-grep(paste("fleet/Survey  ",(data.orig$Nfleet+1):(data.orig$Nfleet+data.orig$Nsurveys),sep="",collapse="|"),row.names(control.run$Select_Params[sizeRows,]))]
  }else{
    sizeFleetRows<-sizeRows
  }
  #sizeFleetRows<-sizeRows[grep(paste("fleet/Survey  ",1:data.orig$Nfleet,sep="",collapse="|"),row.names(control.run$Select_Params[sizeRows,]))]
  sizeSurveyRows<-setdiff(sizeRows,sizeFleetRows)
  sizeNew<-grep("Size",row.names(newParams))
  ageRows<-setdiff(allRows,sizeRows)
  if(data.orig$Nsurveys>0){
    ageFleetRows<-ageRows[-grep(paste("fleet/Survey  ",(data.orig$Nfleet+1):(data.orig$Nfleet+data.orig$Nsurveys),sep="",collapse="|"),row.names(control.run$Select_Params[ageRows,]))]
  }else{
    ageFleetRows<-ageRows
  }
  ageSurveyRows<-setdiff(ageRows,ageFleetRows)

  tempSizeSelectParams<-rbind(control.run$Select_Params[rowFleetLength,],
                              newParams[rowFleetLength[rowsCopyCtrl],],
                              control.run$Select_Params[rowSurveyLength,],
                              control.run$Select_Params[rowFleetAge,],
                              newParams[rowFleetAge[rowsCopyCtrl],],
                              control.run$Select_Params[rowSurveyAge,])

  #oldParams[sizeFleetRows,2]<-rep(7777,length(sizeFleetRows))
  # oldParams[sizeSurveyRows,2]<-rep(6666,length(sizeSurveyRows))
  # tempSizeSelectParams<-rbind(oldParams[sizeFleetRows,],
  #                             newParams[sizeNew,],
  #                             oldParams[sizeSurveyRows,],
  #                             oldParams[ageFleetRows,],
  #                             newParams[-sizeNew,],
  #                             oldParams[ageSurveyRows,])

  control.run$Select_Params<-tempSizeSelectParams
  control.run$Select_Params<<-tempSizeSelectParams

  if(control.run$Var_adj==1)
  {
    #dummy.adjust<-matrix(c(rep(0,3*data.orig$Nfleet),rep(1,3*data.orig$Nfleet)),nrow=6,byrow=TRUE)
    #control.run$Var_Adj_Fact<-cbind(control.orig$Var_Adj_Fact[,1:data.orig$Nfleet],dummy.adjust,control.orig$Var_Adj_Fact[,-(1:data.orig$Nfleet)])
    control.run$Var_Adj_Fact<-cbind(control.orig$Var_Adj_Fact[,1:data.orig$Nfleet],control.orig$Var_Adj_Fact[,1:data.orig$Nfleet],control.orig$Var_Adj_Fact[,-(1:data.orig$Nfleet)])
  }

  #print(paste("reached point ",marker))
  marker<-marker+1
  incProgress(0.0, detail = "Modifying data file") #0.3 total
  #Add references to dummy fleets in data file
  data.run$fleetnames<<-c(data.run$fleetnames[1:data.run$Nfleet],paste(data.run$fleetnames[Fleets],"_dummy",sep=""),data.run$fleetnames[-(1:data.run$Nfleet)])
  data.run$surveytiming<<-c(data.run$surveytiming[1:data.run$Nfleet],data.run$surveytiming[Fleets],data.run$surveytiming[-(1:data.run$Nfleet)])
  data.run$areas<<-c(data.run$areas[1:data.run$Nfleet],data.run$areas[Fleets],data.run$areas[-(1:data.run$Nfleet)])
  data.run$units_of_catch<<-c(data.run$units_of_catch[1:data.run$Nfleet],data.run$units_of_catch[Fleets],data.run$units_of_catch[-(1:data.run$Nfleet)])
  data.run$se_log_catch<<-c(data.run$se_log_catch[1:data.run$Nfleet],data.run$se_log_catch[Fleets],data.run$se_log_catch[-(1:data.run$Nfleet)])
  data.run$init_equil<<-c(data.run$init_equil[1:data.run$Nfleet],0.0000000001*data.run$init_equil[Fleets],data.run$init_equil[-(1:data.run$Nfleet)])
  data.run$fleetinfo1<<-cbind(data.run$fleetinfo1[,1:data.run$Nfleet],data.run$fleetinfo1[,Fleets],data.run$fleetinfo1[,-(1:data.run$Nfleet)])
  data.run$fleetinfo2<<-cbind(data.run$fleetinfo2[,1:data.run$Nfleet],data.run$fleetinfo2[,Fleets],data.run$fleetinfo2[,-(1:data.run$Nfleet)])
  data.run$catch<<-cbind(data.run$catch[,1:data.run$Nfleet],matrix(0.0000000001,nrow=length(data.run$catch[,1]),ncol=length(Fleets)),data.run$catch[,-(1:data.run$Nfleet)])
  data.run$CPUEinfo<<-rbind(data.run$CPUEinfo[1:data.run$Nfleet,],data.run$CPUEinfo[Fleets,],data.run$CPUEinfo[-(1:data.run$Nfleet),])
  data.run$CPUEinfo[,1]<<-1:length(data.run$CPUEinfo[,1])
  data.run$CPUE$index<<-ifelse(data.run$CPUE$index>data.orig$Nfleet,(data.run$CPUE$index+data.orig$Nfleet),data.run$CPUE$index)
  temp.CPUEIndex<-data.run$CPUE[data.run$CPUE$index<=data.orig$Nfleet,]
  temp.CPUEIndex$index<-temp.CPUEIndex$index+data.orig$Nfleet
  #data.run$CPUE<<-rbind(data.run$CPUE[data.run$CPUE$index<=data.orig$Nfleet,],temp.CPUEIndex,data.run$CPUE[data.run$CPUE$index>data.orig$Nfleet,])
  #data.run$N_cpue<<-length(data.run$CPUE$year)
  data.run$lencomp$FltSvy<<-ifelse(data.run$lencomp$FltSvy>data.orig$Nfleet,(data.run$lencomp$FltSvy+data.orig$Nfleet),data.run$lencomp$FltSvy)
  data.run$agecomp$FltSvy<<-ifelse(data.run$agecomp$FltSvy>data.orig$Nfleet,(data.run$agecomp$FltSvy+data.orig$Nfleet),data.run$agecomp$FltSvy)
  if(data.run$N_agecomp==0){
    data.run$agecomp<<-NULL
  }
  if(data.run$N_lencomp==0){
    data.run$lencomp<<-NULL
  }
  data.run$Nfleet<<-data.run$Nfleet+length(Fleets)

  #print(paste("reached point ",marker))
  marker<-marker+1
  incProgress(0.0, detail = "Modifying forecast file") #0.3 total
  #Add references to dummy fleets in forcast file
  if(forecast.run$N_allocation_groups==0)
  {
    forecast.run$N_allocation_groups<<-1
    forecast.run$allocation_among_groups<<-1
    forecast.run$fleet_assignment_to_allocation_group<<-rep(1,length(forecast.run$fleet_assignment_to_allocation_group))
  }

  forecast.run$fleet_relative_F<<-2
  rel_F <- rel_F_orig



  nextAllocationGroup<-1
  filledAllocationGroup<-0
  newAllocationGroups<-forecast.run$fleet_assignment_to_allocation_group
  groups<-sort(unique(forecast.orig$fleet_assignment_to_allocation_group))
  if(length(groups[groups==0])>0){groups<-c(groups[-1],0)}
  newAllocations<-0
  if(eval(parse(text=paste0("input$GroupAllocUnits")))==2)
  {
    scalerFArray<-rel_DB_orig
  }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==3)
  {
    scalerFArray<-rel_RB_orig
  }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==5)
  {
    scalerFArray<-rel_DN_orig
  }else if(eval(parse(text=paste0("input$GroupAllocUnits")))==6)
  {
    scalerFArray<-rel_RN_orig
  }
  #print(paste0("groups = ",groups))
  for(i in 1:length(groups))
  {
    #print(paste0("group = ",i))
    #print(paste0("group alloc = ",eval(parse(text=paste0("input$Group",i,"FleetAllocUnits")))))
    groupfleets<-which(forecast.orig$fleet_assignment_to_allocation_group==groups[i])
    if(eval(parse(text=paste0("input$Group",i,"FleetAllocUnits")))==2)
    {
      groupSumF<-sum(rel_F_orig[,groupfleets])
      scalerF<-1
      for(j in groupfleets)
      {

        if(groups[i]==0)
        {
          scalerF<-1
          forecast.run$fleet_assignment_to_allocation_group[j]<<-0
        }else{
          scalerF<-(eval(parse(text=paste0("input$Group",i,"AllocationSlide")))/forecast.orig$allocation_among_groups[i])
          forecast.run$fleet_assignment_to_allocation_group[j]<<-nextAllocationGroup
        }
        for(k in 1:data.orig$nseas)
        {
          rel_F[k,j]<-eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationSlide")))*groupSumF*scalerF
          if(!is.finite(rel_F[k,j])){
            rel_F[k,j]<-0
          }
        }
        #print(paste0("groups slide = ",eval(parse(text=paste0("input$Group",i,"AllocationSlide")))))
        #print(paste0("fleet slide = ",eval(parse(text=paste0("input$Fleet",j,"AllocationSlide")))))
      }
      if(groups[i]!=0){
        #print("tried to add allocation")
        newAllocations<-c(newAllocations,as.numeric(eval(parse(text=paste0("input$Group",i,"AllocationSlide")))))
        #print(paste0("New Allocations are: ",newAllocations))
        }
      nextAllocationGroup<-nextAllocationGroup+1

    }else if(eval(parse(text=paste0("input$Group",i,"FleetAllocUnits")))==1)
    {
      groupSumF<-sum(scalerFArray[,groupfleets])
      for(j in groupfleets)
      {
        #print("tried to add allocation")
        forecast.run$fleet_assignment_to_allocation_group[j]<<-nextAllocationGroup
        if(groups[i]==0){
          newAllocations<-c(newAllocations,(as.numeric(eval(parse(text=paste0("input$Fleet",j,"AllocationSlide"))))))
        }else{
          newAllocations<-c(newAllocations,(as.numeric(eval(parse(text=paste0("input$Group",i,"AllocationSlide"))))*as.numeric(eval(parse(text=paste0("input$Fleet",j,"AllocationSlide"))))))
        }
        #print(paste0("groups slide = ",eval(parse(text=paste0("input$Group",i,"AllocationSlide")))))
        #print(paste0("fleet slide = ",eval(parse(text=paste0("input$Fleet",j,"AllocationSlide")))))
        #print(paste0("New Allocations are: ",newAllocations))
        #forecast.run$allocation_among_groups[nextAllocationGroup]<<-eval(parse(text=paste0("input$Group",i,"AllocationSlide")))*eval(parse(text=paste0("input$Fleet",j,"AllocationSlide")))
        nextAllocationGroup<-nextAllocationGroup+1

        for(k in 1:data.orig$nseas)
        {
          scalerF<-(eval(parse(text=paste0("input$Fleet",j,"Seas",k,"AllocationSlide")))*groupSumF)/scalerFArray[k,j]
          rel_F[k,j]<-rel_F[k,j]*scalerF
          if(!is.finite(rel_F[k,j])){
            rel_F[k,j]<-0
          }
        }
      }
    }
  }
  #print(paste0("New Allocations are: ",newAllocations))

  forecast.run$allocation_among_groups<<-as.data.frame(matrix(newAllocations[-1],nrow=1,ncol=length(newAllocations[-1])))
  forecast.run$N_allocation_groups<<-length(forecast.run$allocation_among_groups)
  forecast.run$fleet_assignment_to_allocation_group<<-c(rep(0,length(forecast.run$fleet_assignment_to_allocation_group)),forecast.run$fleet_assignment_to_allocation_group)
  forecast.run$max_totalcatch_by_fleet<<-c(forecast.run$max_totalcatch_by_fleet,forecast.run$max_totalcatch_by_fleet)

  forecast.run$ForeCatch<<-as.data.frame(managementLimits$pastCatch)

  rel_F<-cbind(0.0000000001*rel_F_orig,rel_F)
  names(rel_F) <- c(paste0("Fleet ",1:data.orig$Nfleet),paste0("Fleet ",1:data.orig$Nfleet," Dummy"))
  forecast.run$rel_F<<-data.frame(rel_F)
  forecast.run$FirstYear_for_caps_and_allocations<<-as.numeric(input$ManagementYearInput)
  forecast.run$Ncatch<<-length(forecast.run$ForeCatch[,1])
  forecast.run$Nforecastyrs<<-forecast.orig$Nforecastyrs

   rowsRecDev<-grep("# recdev1",pars.run$Labels)
   rowsFirstFinit<-grep("# init_F",pars.run$Labels)[1]
  # pars.run$Labels<<-c(pars.run$Labels[1:(rowsRecDev)],pars.run$Labels[1:2],pars.run$Labels[(rowsFirstFinit):length(pars.run$Labels)])
  # pars.run$Labels[[rowsRecDev+1]]<-"# Fcast_recruitments:"
  # pars.run$Labels[[rowsRecDev+2]]<-"# Fcast_impl_error:"
  # pars.run$Values<<-c(pars.run$Values[1:(rowsRecDev-1)],pars.run$Values[1:2],pars.run$Values[(rowsFirstFinit-1):length(pars.run$Values)])
  # pars.run$Values[[rowsRecDev]]<-rep(0,(forecast.run$Nforecastyrs+(data.run$endyr-control.run$SR_EndYr)))
  # pars.run$Values[[rowsRecDev+1]]<-rep(0,(forecast.run$Nforecastyrs))
  #
  forecast.run$benchmarks<<-1
  forecast.run$MSY<<-input$ForecastTarget
  forecast.run$Forecast<<-input$ForecastTarget
  if(input$ForecastTarget==1)
  {
    forecast.run$SPRtarget<<-input$TargetValue
  }else if(input$ForecastTarget==2)
  {
    forecast.run$SPRtarget<<-0.3
  }else if(input$ForecastTarget==3)
  {
    forecast.run$Btarget<<-input$TargetValue
  }
  #print(paste("reached point ",marker))
  marker<-marker+1
  incProgress(0.0, detail = "Writing modified files to run folder") #0.3 total
  wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
  wrt_data(data.run,outfile=paste0(dir.run,"/",starter.run$datfile),overwrite = TRUE)
  wrt_ctl(file=paste0(dir.run,"/",starter.run$ctlfile),data.run,control.run)
  wrt_par(file=paste(dir.run,"/ss3.par",sep=""),pars.run,overwrite=TRUE)
  print("wrote all the files")
}

#' Run target forecast
#'
#' Runs stock synthesis with -nohess and no parameter
#' estimation in order to produce projections. This
#' process is looped until the desiged management
#' targets are obtained (i.e. SPR=0.3 or MSY)
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords assessment projection
RunTargetForecast<-function(input, output, session){
  if(input$ForecastTarget==5){
    incProgress(0.1, detail = paste0("Running fixed catch projection ")) #0.4 total
    shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))

    output.read<-FALSE
    output.temp<-NULL
    while(output.read==FALSE){
      try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
      if(is.null(output.temp)){
        output.read<-FALSE
        numCols<<-numCols+100
      }else{
        output.read<-TRUE
        output.run<<-output.temp
      }
    }

    dir.msy<<-paste0(dir.base,"/msy")
    if(!dir.exists(dir.msy))
    {
      incProgress(0.0, detail = paste0("Building MSY directory"))
      dir.create(dir.msy)
      files.run<-list.files(dir.run)
      file.copy(paste0(dir.run,"/",files.run),dir.msy)
    }else{
      incProgress(0.0, detail = paste0("Saving MSY results"))
      files.msy<-list.files(dir.msy)
      unlink(paste0(dir.msy,"/",files.msy))
      files.run<-list.files(dir.run)
      file.copy(paste0(dir.run,"/",files.run),dir.msy)
    }
    incProgress(0.1, detail = paste0("Processing MSY catch data (this could take a few minutes)"))
    msyCatch<<-ReadCatch(output.run, (data.orig$styr-1):(data.orig$endyr+forecast.orig$Nforecastyrs))
    starter.msy<<-starter.run
    data.msy<<-data.run
    forecast.msy<<-forecast.run
    control.msy<<-control.run
    output.msy<<-output.run
    pars.msy<<-pars.run
  }else if(input$ForecastTarget!=4)
  {
    if(input$ForecastTarget==1)
    {
      incProgress(0.1, detail = paste0("Finding MSY proxy of SPR = ",input$TargetValue)) #0.4 total
      #print("MSY == 1, start search")
      keep.searching<-TRUE
      Search.Iters<-0
      search.bounds<-matrix(c(0,0,1,1),nrow=2,ncol=2)
      while(keep.searching==TRUE)
      {
        incProgress(0.1, detail = paste0("Finding MSY proxy of SPR = ",input$TargetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30)")) #0.4 total
        shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))
        #print("SS ran successfully :)")
        incProgress(0.0, detail = paste0("Finding MSY proxy of SPR = ",input$TargetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Reading output")) #0.4 total

        output.read<-FALSE
        output.temp<-NULL
        while(output.read==FALSE){
          try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
          if(is.null(output.temp)){
            output.read<-FALSE
            numCols<<-numCols+100
          }else{
            output.read<-TRUE
            output.run<<-output.temp
          }
        }
        #print("Output read successfully")
        incProgress(0.0, detail = paste0("Finding MSY proxy of SPR = ",input$TargetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Calculating new target")) #0.4 total

        equilSPR<-mean(output.run$sprseries$SPR[output.run$sprseries$Year>=input$TargetYears[1] & output.run$sprseries$Year<=input$TargetYears[2]])
        if(abs(equilSPR-input$TargetValue)>0.0001*input$TargetValue && Search.Iters<=30)
        {
          if(equilSPR<=input$TargetValue){
            search.bounds[,1]<-c(equilSPR,forecast.run$SPRtarget)
            forecast.run$SPRtarget<<-search.bounds[2,1]+(((input$TargetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))+(search.bounds[1,2]-search.bounds[1,1])*0.2*(1-((input$TargetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))))*(search.bounds[2,2]-search.bounds[2,1])
          }
          if(equilSPR>=input$TargetValue){
            search.bounds[,2]<-c(equilSPR,forecast.run$SPRtarget)
            forecast.run$SPRtarget<<-search.bounds[2,1]+((1-0.2*(search.bounds[1,2]-search.bounds[1,1]))*((input$TargetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1])))*(search.bounds[2,2]-search.bounds[2,1])
          }
          #forecast.run$SPRtarget<<-min(c(max(c(0.01,2*forecast.run$SPRtarget)),max(c(0.01,0.5*forecast.run$SPRtarget,(forecast.run$SPRtarget+(input$TargetValue-equilSPR))))))
          incProgress(0.0, detail = paste0("Finding MSY proxy of SPR = ",input$TargetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Writing new forecast file")) #0.4 total
          wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
          Search.Iters<-Search.Iters+1
        }else{
          keep.searching<-FALSE
        }
      }
    }else if(input$ForecastTarget==6){
      
      get_Cost_Benefit<-function(Val_series=NULL,Quant_Series=NULL,Point_value=NULL){
        #browser()
        lower_1<-Quant_Series[Quant_Series<=Point_value]
        lower_1<-lower_1[length(lower_1)]
        upper_1<-Quant_Series[Quant_Series>=Point_value]
        upper_1<-upper_1[1]
        lower_2<-Val_series[Quant_Series<=Point_value]
        lower_2<-lower_2[length(lower_2)]
        upper_2<-Val_series[Quant_Series>=Point_value]
        upper_2<-upper_2[1]
        
        if(upper_1==lower_1){
          Val<-Point_value*((upper_2+lower_2)/2)
        }else{
          Val<-Point_value*(lower_2+((Point_value-lower_1)/(upper_1-lower_1))*(upper_2-lower_2))
        }
        return(Val)
      }
      #browser()
      temp_str<-as.numeric(strsplit(input$CostBenefits,split="[,\n \t]+")[[1]])
      temp_str<-temp_str[!is.na(temp_str)]
      temp_str<-temp_str[temp_str!=""]
      ProfitMatrix_temp<-matrix(temp_str,ncol=4,byrow=TRUE)
      #ProfitMatrix_temp<-matrix(as.numeric(strsplit(input$CostBenefits,split="[,\n ]+")[[1]]),ncol=4,byrow=TRUE)
      ProfitMatrix_temp<-ProfitMatrix_temp[order(ProfitMatrix_temp[,1],ProfitMatrix_temp[,2],ProfitMatrix_temp[,3]),]
      nCatches<-length(unique(ProfitMatrix_temp[ProfitMatrix_temp[,2]==1,3]))
      CatchSeq<-c(0,sort(unique(ProfitMatrix_temp[ProfitMatrix_temp[,2]==1,3])),Inf)
      nFs<-length(unique(ProfitMatrix_temp[ProfitMatrix_temp[,2]==2,3]))
      FSeq<-c(0,sort(unique(ProfitMatrix_temp[ProfitMatrix_temp[,2]==2,3])),Inf)
      nFls<-length(unique(ProfitMatrix_temp[,1]))
      FleetSeq<-sort(unique(ProfitMatrix_temp[,1]))
      CostMatrix<-matrix(NA,nrow=nFls,ncol=(nFs+2))
      BenefitMatrix<-matrix(NA,nrow=nFls,ncol=(nCatches+2))
      for(i in 1:nFls){
        CostMatrix[i,2:(nFs+1)]<-ProfitMatrix_temp[ProfitMatrix_temp[,1]==i & ProfitMatrix_temp[,2]==2,4]
        BenefitMatrix[i,2:(nCatches+1)]<-ProfitMatrix_temp[ProfitMatrix_temp[,1]==i & ProfitMatrix_temp[,2]==1,4]
        CostMatrix[i,1]<-CostMatrix[i,2]
        CostMatrix[i,(nFs+2)]<-CostMatrix[i,(nFs+1)]
        BenefitMatrix[i,1]<-BenefitMatrix[i,2]
        BenefitMatrix[i,(nCatches+2)]<-BenefitMatrix[i,(nCatches+1)]
      }
      incProgress(0.1, detail = paste0("Finding MEY")) #0.4 total
      #print("MSY == 2, start search")
      statusMatrix<-matrix(NA,nrow=11,ncol=30,dimnames = list(c("target SPR","input SPR","achieved SPR","achieved Catch","achieved F","achieved cost","achieved benefit","achieved profit","last Catch","min Catch","max Catch")))
      
      statusMatrix[,1]<-c(0,0,0,0,0,0,0,0,0,0,0)
      statusMatrix[,2]<-c(1,1,1,0,0,0,0,0,0,0,0)
      TargetVals<-c(0.2,0.3,0.5)
      TargetVal<-1
      step<-3
      statusMatrix[1,step]<-TargetVals[TargetVal]
      statusMatrix[2,step]<-TargetVals[TargetVal]
      forecast.run$MSY<<-1
      forecast.run$Forecast<<-1
      forecast.run$SPRtarget<<-as.numeric(statusMatrix[2,step])
      wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
      incProgress(0.05, detail = paste0("Finding MEY - search loop ",step," of probably 10-15 (max 30)- finding Target ",TargetVal," of 3. Running SS3")) #0.4 total
      
      findingTargets<-TRUE
      while(findingTargets){
        
        shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))
        #print("SS ran successfully 1 :)")
        incProgress(0.00, detail = paste0("Finding MEY - search loop ",step," of probably 10-15 (max 30)- finding Target ",TargetVal," of 3. Reading output file")) #0.4 total
        
        output.read<-FALSE
        output.temp<-NULL
        while(output.read==FALSE){
          try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
          if(is.null(output.temp)){
            output.read<-FALSE
            numCols<<-numCols+100
          }else{
            output.read<-TRUE
            output.run<<-output.temp
          }
        }
        
        #print("Output read successfully 1")
        incProgress(0.00, detail = paste0("Finding MEY - search loop ",step," of probably 10-15 (max 30)- finding Target ",TargetVal," of 3. Reading catch results")) #0.4 total
        
        
        
        statusMatrix[3,step]<-mean(output.run$sprseries$SPR[output.run$sprseries$Year>=input$TargetYears[1] & output.run$sprseries$Year<=input$TargetYears[2]])
        Catch<-ReadCatch(output.run,input$TargetYears[1]:input$TargetYears[2])
        Catch<-Catch[Catch[,1]>=input$TargetYears[1] & Catch[,1]<=input$TargetYears[2],]
        statusMatrix[4,step]<-sum(Catch[,5])/length(unique(Catch[,1]))
        
        temp_Cost_sum<-0
        temp_Benefit_sum<-0
        for(i in 1:nFls){
          temp_Fleet_Catch<-Catch[Catch[,4]==(i+nFls),,drop=FALSE]
          if(output.run$catch_units[i]==1){
            catch_Col_temp<-5
          }else{
            catch_Col_temp<-7
          }
          for(j in 1:length(temp_Fleet_Catch[,1])){
            temp_Cost_sum<-temp_Cost_sum+get_Cost_Benefit(CostMatrix[i,],FSeq,temp_Fleet_Catch[j,10])
            
            temp_Benefit_sum<-temp_Benefit_sum+get_Cost_Benefit(BenefitMatrix[i,],CatchSeq,temp_Fleet_Catch[j,catch_Col_temp])
          }
        }
        
        statusMatrix[5,step]<-sum(Catch[,10])/length(unique(Catch[,1]))
        
        statusMatrix[6,step]<-temp_Cost_sum/length(unique(Catch[,1]))
        statusMatrix[7,step]<-temp_Benefit_sum/length(unique(Catch[,1]))
        statusMatrix[8,step]<-(temp_Benefit_sum-temp_Cost_sum)/length(unique(Catch[,1]))
        
        if(statusMatrix[3,step]<=0.01){statusMatrix[3,step]<=0.01}
        #print(paste("management year = ",(input$ManagementYearInput+1)))
        #print(paste("target year = ",(input$TargetYears[1]-1)))
        CatchAll<-ReadCatch(output.run,(input$ManagementYearInput+1):(input$TargetYears[1]-1))
        #print("Catch All = ")
        #print(CatchAll)
        
        CatchAllAnnual<-aggregate(CatchAll[,5:6],list(CatchAll[,1]),sum,na.rm=TRUE)
        #print("Catch All Annual= ")
        #print(CatchAllAnnual)
        statusMatrix[9,step]<-CatchAllAnnual[1,2]
        print("Status matrix = ")
        print(statusMatrix)
        if(length(min(CatchAllAnnual[,2],na.rm=TRUE))==0){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[10,step]<-0
          statusMatrix[4,step]<-0
        }else if(min(CatchAllAnnual[,2],na.rm=TRUE)==Inf){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[10,step]<-0
          statusMatrix[4,step]<-0
        }else if(min(CatchAllAnnual[,2],na.rm=TRUE)<=0){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[10,step]<-0
          statusMatrix[4,step]<-0
        }else{
          statusMatrix[10,step]<-min(CatchAllAnnual[,2])
        }
        
        if(length(max(CatchAllAnnual[,2],na.rm=TRUE))==0){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[11,step]<-0
          statusMatrix[4,step]<-0
        }else if(max(CatchAllAnnual[,2],na.rm=TRUE)==Inf){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[11,step]<-0
          statusMatrix[4,step]<-0
        }else if(max(CatchAllAnnual[,2],na.rm=TRUE)<=0){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[11,step]<-0
          statusMatrix[4,step]<-0
        }else{
          statusMatrix[11,step]<-max(CatchAllAnnual[,2])
        }
        
        if(statusMatrix[3,step]<=0.01)
        {
          statusMatrix[3,step]<-0
          statusMatrix[4,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[10,step]<-0
          statusMatrix[11,step]<-0
        }
        
        # if(statusMatrix[6,step]<0.9*min(c(statusMatrix[4,step],statusMatrix[5,step]),na.rm=TRUE)){
        #
        #   statusMatrix[4,step]<-0
        # }
        # if(statusMatrix[7,step]>1.1*max(c(statusMatrix[4,step],statusMatrix[5,step]),na.rm=TRUE)){
        #   statusMatrix[4,step]<-0
        # }
        
        if(abs(statusMatrix[1,step]-statusMatrix[3,step])>0.2 && step<=(10+5*TargetVal))
        {
          statusMatrix[1,(step+1)]<-statusMatrix[1,step]
          if(statusMatrix[1,step]>statusMatrix[3,step]){
            statusMatrix[2,(step+1)]<-statusMatrix[2,step]+min((statusMatrix[1,step]-statusMatrix[3,step]),(0.75*(1-statusMatrix[2,step])*(statusMatrix[1,step]-statusMatrix[3,step])/statusMatrix[2,step]))
          }else{
            statusMatrix[2,(step+1)]<-statusMatrix[2,step]+max((statusMatrix[1,step]-statusMatrix[3,step]),-0.75*(statusMatrix[2,step]))
          }
          step<-step+1
          print("Status matrix = ")
          print(statusMatrix)
          forecast.run$SPRtarget<<-as.numeric(statusMatrix[2,step])
          wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
        }else{
          if(TargetVal<3){
            TargetVal<-TargetVal+1
            statusMatrix[1,(step+1)]<-TargetVals[TargetVal]
            statusMatrix[2,(step+1)]<-statusMatrix[2,step]+(TargetVals[TargetVal]-TargetVals[(TargetVal-1)])*(1-statusMatrix[2,step])
            step<-step+1
            print("Status matrix = ")
            print(statusMatrix)
            forecast.run$SPRtarget<<-as.numeric(statusMatrix[2,step])
            wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
          }else{
            step<-step+1
            findingTargets<-FALSE
          }
        }
      }
      
      findingMSY<-TRUE
      stepSize<-1
      newGuess1<-matrix(NA,nrow=0,ncol=5)
      newGuess2<-matrix(NA,nrow=0,ncol=5)
      lowMSY<-matrix(c(0,0,0,0),nrow=4,ncol=1)
      highMSY<-matrix(c(0,0,0,0),nrow=4,ncol=1)
      while(findingMSY==TRUE)
      {
        incProgress(0.00, detail = paste0("Finding MEY - search loop ",step," of probably 10-15 (max 30)- finding MEY. Calculating next search estimate")) #0.4 total
        
        BestGuesses<-statusMatrix[,order(statusMatrix[8,],statusMatrix[3,],statusMatrix[2,],decreasing = TRUE)]
        
        bestMSY<-BestGuesses[,1,drop=FALSE]
        
        lowMSY<-BestGuesses[,BestGuesses[2,]<bestMSY[2,1],drop=FALSE]
        lowMSY<-lowMSY[,lowMSY[3,]<=bestMSY[3,1],drop=FALSE]
        lowMSY<-lowMSY[,!is.na(lowMSY[3,]),drop=FALSE]
        lowMSY<-lowMSY[,lowMSY[3,]==max(lowMSY[3,]),drop=FALSE]
        lowMSY<-lowMSY[,1,drop=FALSE]
        
        highMSY<-BestGuesses[,BestGuesses[2,]>bestMSY[2,],drop=FALSE]
        highMSY<-highMSY[,highMSY[3,]>=bestMSY[3,],drop=FALSE]
        highMSY<-highMSY[,!is.na(highMSY[3,]),drop=FALSE]
        highMSY<-highMSY[,highMSY[3,]==min(highMSY[3,]),drop=FALSE]
        highMSY<-highMSY[,1,drop=FALSE]
        
        newGuess1<-rbind(FitParabola(c(bestMSY[2,1],lowMSY[2,1],highMSY[2,1]),c(bestMSY[8,1],lowMSY[8,1],highMSY[8,1])),newGuess1)
        newGuess2<-rbind(FitParabola(c(bestMSY[3,1],lowMSY[3,1],highMSY[3,1]),c(bestMSY[8,1],lowMSY[8,1],highMSY[8,1])),newGuess2)
        
        statusMatrix[1,step]<-0.8*newGuess2[1,4]+0.1*lowMSY[3,1]+0.1*highMSY[3,1]
        statusMatrix[2,step]<-0.8*newGuess1[1,4]+0.1*lowMSY[2,1]+0.1*highMSY[2,1]
        
        print(paste("best mey = ",bestMSY))
        print(paste("low mey = ",lowMSY))
        print(paste("high mey = ",highMSY))
        print(paste("new guess 1 = ",newGuess1))
        print(paste("new guess 2 = ",newGuess2))
        
        print("Status matrix = ")
        print(statusMatrix)
        #statusMatrix[1,(6+Search.Iters)]<-(bestMSY[3,1]*(bestMSY[4,1])+highMSY[3,1]*(highMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize)+lowMSY[3,1]*(lowMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize))/(bestMSY[4,1]+(highMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize)+(lowMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize))
        #statusMatrix[2,(6+Search.Iters)]<-(bestMSY[2,1]*(bestMSY[4,1])+highMSY[2,1]*(highMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize)+lowMSY[2,1]*(lowMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize))/(bestMSY[4,1]+(highMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize)+(lowMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize))
        
        
        #if((highMSY[3,1]-bestMSY[3,1])>(bestMSY[3,1]-lowMSY[3,1])){
        # }else{
        #   statusMatrix[1,(6+Search.Iters)]<-(bestMSY[3,1]*(bestMSY[4,1]+stepSize*lowMSY[4,1])+lowMSY[3,1]*(lowMSY[4,1]+stepSize*bestMSY[4,1]))/((bestMSY[4,1]+stepSize*lowMSY[4,1])+(lowMSY[4,1]+stepSize*bestMSY[4,1]))
        #   statusMatrix[2,(6+Search.Iters)]<-(bestMSY[2,1]*(bestMSY[4,1]+stepSize*lowMSY[4,1])+lowMSY[2,1]*(lowMSY[4,1]+stepSize*bestMSY[4,1]))/((bestMSY[4,1]+stepSize*lowMSY[4,1])+(lowMSY[4,1]+stepSize*bestMSY[4,1]))
        # }
        #statusMatrix[1,(4+Search.Iters)]<-BestGuesses[3,1]+stepSize*((BestGuesses[4,1]-BestGuesses[4,2])*(BestGuesses[3,1]-BestGuesses[3,2])+(BestGuesses[4,1]-BestGuesses[4,3])*(BestGuesses[3,1]-BestGuesses[3,3]))/((BestGuesses[4,1]-BestGuesses[4,2])+(BestGuesses[4,1]-BestGuesses[4,3]))
        #statusMatrix[2,(4+Search.Iters)]<-BestGuesses[2,1]+stepSize*((BestGuesses[4,1]-BestGuesses[4,2])*(BestGuesses[2,1]-BestGuesses[2,2])+(BestGuesses[4,1]-BestGuesses[4,3])*(BestGuesses[2,1]-BestGuesses[2,3]))/((BestGuesses[4,1]-BestGuesses[4,2])+(BestGuesses[4,1]-BestGuesses[4,3]))
        
        forecast.run$SPRtarget<<-as.numeric(statusMatrix[2,step])
        wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
        
        #print(statusMatrix)
        
        incProgress(0.05, detail = paste0("Finding MEY - search loop ",step," of probably 10-15 (max 30)- finding MEY. Running SS3. Best estim so far MEY = ",bestMSY[8,1]," (",lowMSY[8,1],",",highMSY[8,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total
        
        shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))#
        #print(paste0("SS ran successfully :) ",Search.Iters))
        incProgress(0.00, detail = paste0("Finding MEY - search loop ",step," of probably 10-15 (max 30)- finding MEY. Reading output file. Best estim so far MEY = ",bestMSY[8,1]," (",lowMSY[8,1],",",highMSY[8,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total
        
        output.read<-FALSE
        output.temp<-NULL
        while(output.read==FALSE){
          try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
          if(is.null(output.temp)){
            output.read<-FALSE
            numCols<<-numCols+100
          }else{
            output.read<-TRUE
            output.run<<-output.temp
          }
        }
        incProgress(0.00, detail = paste0("Finding MEY - search loop ",step," of probably 10-15 (max 30)- finding MEY. Reading catch data. Best estim so far MEY = ",bestMSY[8,1]," (",lowMSY[8,1],",",highMSY[8,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total
        
        #print(paste0("Output read successfully ",Search.Iters))
        statusMatrix[3,step]<-sum(output.run$sprseries$SPR[output.run$sprseries$Year>=input$TargetYears[1] & output.run$sprseries$Year<=input$TargetYears[2]])/length(output.run$sprseries$SPR[output.run$sprseries$Year>=input$TargetYears[1] & output.run$sprseries$Year<=input$TargetYears[2]])
        Catch<-ReadCatch(output.run,input$TargetYears[1]:input$TargetYears[2])
        Catch<-Catch[Catch[,1]>=input$TargetYears[1] & Catch[,1]<=input$TargetYears[2],]
        statusMatrix[4,step]<-sum(Catch[,5])/length(unique(Catch[,1]))
        
        temp_Cost_sum<-0
        temp_Benefit_sum<-0
        for(i in 1:nFls){
          temp_Fleet_Catch<-Catch[Catch[,4]==(i+nFls),,drop=FALSE]
          if(output.run$catch_units[i]==1){
            catch_Col_temp<-5
          }else{
            catch_Col_temp<-7
          }
          for(j in 1:length(temp_Fleet_Catch[,1])){
            temp_Cost_sum<-temp_Cost_sum+get_Cost_Benefit(CostMatrix[i,],FSeq,temp_Fleet_Catch[j,10])
            temp_Benefit_sum<-temp_Benefit_sum+get_Cost_Benefit(BenefitMatrix[i,],CatchSeq,temp_Fleet_Catch[j,catch_Col_temp])
          }
        }
        
        statusMatrix[5,step]<-sum(Catch[,10])/length(unique(Catch[,1]))
        
        statusMatrix[6,step]<-temp_Cost_sum/length(unique(Catch[,1]))
        statusMatrix[7,step]<-temp_Benefit_sum/length(unique(Catch[,1]))
        statusMatrix[8,step]<-(temp_Benefit_sum-temp_Cost_sum)/length(unique(Catch[,1]))
        
        CatchAll<-ReadCatch(output.run,(input$ManagementYearInput+1):(input$TargetYears[1]-1))
        CatchAllAnnual<-aggregate(CatchAll[,5:6],list(CatchAll[,1]),sum,na.rm=TRUE)
        
        statusMatrix[9,step]<-CatchAllAnnual[1,2]
        print("Status matrix = ")
        print(statusMatrix)
        if(length(min(CatchAllAnnual[,2],na.rm=TRUE))==0){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[10,step]<-0
          statusMatrix[4,step]<-0
        }else if(min(CatchAllAnnual[,2],na.rm=TRUE)==Inf){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[10,step]<-0
          statusMatrix[4,step]<-0
        }else if(min(CatchAllAnnual[,2],na.rm=TRUE)<=0){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[10,step]<-0
          statusMatrix[4,step]<-0
        }else{
          statusMatrix[10,step]<-min(CatchAllAnnual[,2])
        }
        
        if(length(max(CatchAllAnnual[,2],na.rm=TRUE))==0){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[11,step]<-0
          statusMatrix[4,step]<-0
        }else if(max(CatchAllAnnual[,2],na.rm=TRUE)==Inf){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[11,step]<-0
          statusMatrix[4,step]<-0
        }else if(max(CatchAllAnnual[,2],na.rm=TRUE)<=0){
          statusMatrix[3,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[11,step]<-0
          statusMatrix[4,step]<-0
        }else{
          statusMatrix[11,step]<-max(CatchAllAnnual[,2])
        }
        
        if(statusMatrix[3,step]<=0.01)
        {
          statusMatrix[3,step]<-0
          statusMatrix[4,step]<-0
          statusMatrix[5,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[7,step]<-0
          statusMatrix[8,step]<-0
          statusMatrix[9,step]<-0
          statusMatrix[10,step]<-0
          statusMatrix[11,step]<-0
        }
        
        
        # if(statusMatrix[6,step]<0.9*min(c(statusMatrix[4,step],statusMatrix[5,step]),na.rm=TRUE)){
        #   statusMatrix[4,step]<-0
        # }
        # if(statusMatrix[7,step]>1.1*max(c(statusMatrix[4,step],statusMatrix[5,step]),na.rm=TRUE)){
        #   statusMatrix[4,step]<-0
        # }
        
        if(statusMatrix[8,step]>BestGuesses[8,1]){
          
        }else{
          stepSize<-0.5*stepSize
        }
        step<-step+1
        print("Status matrix = ")
        print(statusMatrix)
        if(((highMSY[3,1]-lowMSY[3,1])<0.001)||(stepSize<0.001)||(step>30)){
          findingMSY<-FALSE
          outputName1<-"MEYSearch"
          outputName2<-"ParabolaGuesses1"
          outputName3<-"ParabolaGuesses2"
          dir.outputs<<-paste0(dir.base,"/outputs")
          if(!dir.exists(dir.outputs))
          {
            dir.create(dir.outputs)
          }else{
            files.outputs<-list.files(dir.outputs)
            if(length(grep(paste0("MEYSearch"),files.outputs))!=0){
              repNum<-1
              outputName1<-paste0("MEYSearch(",repNum,")")
              while(length(grep(paste0(outputName1),files.outputs,fixed=TRUE))!=0){
                repNum<-repNum+1
                outputName1<-paste0("MEYSearch(",repNum,")")
              }
            }
            
            if(length(grep(paste0("ParabolaGuesses1"),files.outputs))!=0){
              repNum<-1
              outputName2<-paste0("ParabolaGuesses1(",repNum,")")
              while(length(grep(paste0(outputName2),files.outputs,fixed=TRUE))!=0){
                repNum<-repNum+1
                outputName2<-paste0("ParabolaGuesses1(",repNum,")")
              }
            }
            
            if(length(grep(paste0("ParabolaGuesses2"),files.outputs))!=0){
              repNum<-1
              outputName3<-paste0("ParabolaGuesses2(",repNum,")")
              while(length(grep(paste0(outputName3),files.outputs,fixed=TRUE))!=0){
                repNum<-repNum+1
                outputName3<-paste0("ParabolaGuesses2(",repNum,")")
              }
            }
          }
          write.csv(statusMatrix,file=paste0(dir.outputs,"/",outputName1,".csv"),row.names = FALSE)
          write.csv(newGuess1,file=paste0(dir.outputs,"/",outputName2,".csv"),row.names = FALSE)
          write.csv(newGuess2,file=paste0(dir.outputs,"/",outputName3,".csv"),row.names = FALSE)
        }
      }
    }else if(input$ForecastTarget==2){

      incProgress(0.1, detail = paste0("Finding MSY")) #0.4 total
      #print("MSY == 2, start search")
      statusMatrix<-matrix(NA,nrow=7,ncol=30,dimnames = list(c("target SPR","input SPR","achieved SPR","achieved Catch","last Catch","min Catch","max Catch")))

      statusMatrix[,1]<-c(0,0,0,0,0,0,0)
      statusMatrix[,2]<-c(1,1,1,0,0,0,0)
      TargetVals<-c(0.2,0.3,0.5)
      TargetVal<-1
      step<-3
      statusMatrix[1,step]<-TargetVals[TargetVal]
      statusMatrix[2,step]<-TargetVals[TargetVal]
      forecast.run$MSY<<-1
      forecast.run$Forecast<<-1
      forecast.run$SPRtarget<<-as.numeric(statusMatrix[2,step])
      wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
      incProgress(0.05, detail = paste0("Finding MSY - search loop ",step," of probably 10-15 (max 30)- finding Target ",TargetVal," of 3. Running SS3")) #0.4 total

      findingTargets<-TRUE
      while(findingTargets){

      shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))
      #print("SS ran successfully 1 :)")
      incProgress(0.00, detail = paste0("Finding MSY - search loop ",step," of probably 10-15 (max 30)- finding Target ",TargetVal," of 3. Reading output file")) #0.4 total

      output.read<-FALSE
      output.temp<-NULL
      while(output.read==FALSE){
        try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
        if(is.null(output.temp)){
          output.read<-FALSE
          numCols<<-numCols+100
        }else{
          output.read<-TRUE
          output.run<<-output.temp
        }
      }

      #print("Output read successfully 1")
      incProgress(0.00, detail = paste0("Finding MSY - search loop ",step," of probably 10-15 (max 30)- finding Target ",TargetVal," of 3. Reading catch results")) #0.4 total

      statusMatrix[3,step]<-mean(output.run$sprseries$SPR[output.run$sprseries$Year>=input$TargetYears[1] & output.run$sprseries$Year<=input$TargetYears[2]])
      Catch<-ReadCatch(output.run,input$TargetYears[1]:input$TargetYears[2])
      Catch<-Catch[Catch[,1]>=input$TargetYears[1] & Catch[,1]<=input$TargetYears[2],]
      statusMatrix[4,step]<-sum(Catch[,5])/length(unique(Catch[,1]))

      if(statusMatrix[3,step]<=0.01){statusMatrix[3,step]<=0.01}
      #print(paste("management year = ",(input$ManagementYearInput+1)))
      #print(paste("target year = ",(input$TargetYears[1]-1)))
      CatchAll<-ReadCatch(output.run,(input$ManagementYearInput+1):(input$TargetYears[1]-1))
      #print("Catch All = ")
      #print(CatchAll)

      CatchAllAnnual<-aggregate(CatchAll[,5:6],list(CatchAll[,1]),sum,na.rm=TRUE)
      #print("Catch All Annual= ")
      #print(CatchAllAnnual)
      statusMatrix[5,step]<-CatchAllAnnual[1,2]
      print("Status matrix = ")
      print(statusMatrix)
      if(length(min(CatchAllAnnual[,2],na.rm=TRUE))==0){
        statusMatrix[3,step]<-0
        statusMatrix[5,step]<-0
        statusMatrix[6,step]<-0
        statusMatrix[4,step]<-0
      }else if(min(CatchAllAnnual[,2],na.rm=TRUE)==Inf){
        statusMatrix[3,step]<-0
        statusMatrix[5,step]<-0
        statusMatrix[6,step]<-0
        statusMatrix[4,step]<-0
      }else if(min(CatchAllAnnual[,2],na.rm=TRUE)<=0){
        statusMatrix[3,step]<-0
        statusMatrix[5,step]<-0
        statusMatrix[6,step]<-0
        statusMatrix[4,step]<-0
      }else{
        statusMatrix[6,step]<-min(CatchAllAnnual[,2])
      }

      if(length(max(CatchAllAnnual[,2],na.rm=TRUE))==0){
        statusMatrix[3,step]<-0
        statusMatrix[5,step]<-0
        statusMatrix[7,step]<-0
        statusMatrix[4,step]<-0
      }else if(max(CatchAllAnnual[,2],na.rm=TRUE)==Inf){
        statusMatrix[3,step]<-0
        statusMatrix[5,step]<-0
        statusMatrix[7,step]<-0
        statusMatrix[4,step]<-0
      }else if(max(CatchAllAnnual[,2],na.rm=TRUE)<=0){
        statusMatrix[3,step]<-0
        statusMatrix[5,step]<-0
        statusMatrix[7,step]<-0
        statusMatrix[4,step]<-0
      }else{
        statusMatrix[7,step]<-max(CatchAllAnnual[,2])
      }

      if(statusMatrix[3,step]<=0.01)
      {
        statusMatrix[3,step]<-0
        statusMatrix[4,step]<-0
        statusMatrix[5,step]<-0
        statusMatrix[6,step]<-0
        statusMatrix[7,step]<-0
      }

      # if(statusMatrix[6,step]<0.9*min(c(statusMatrix[4,step],statusMatrix[5,step]),na.rm=TRUE)){
      #
      #   statusMatrix[4,step]<-0
      # }
      # if(statusMatrix[7,step]>1.1*max(c(statusMatrix[4,step],statusMatrix[5,step]),na.rm=TRUE)){
      #   statusMatrix[4,step]<-0
      # }

      if(abs(statusMatrix[1,step]-statusMatrix[3,step])>0.2 && step<=(10+5*TargetVal))
      {
        statusMatrix[1,(step+1)]<-statusMatrix[1,step]
        if(statusMatrix[1,step]>statusMatrix[3,step]){
          statusMatrix[2,(step+1)]<-statusMatrix[2,step]+min((statusMatrix[1,step]-statusMatrix[3,step]),(0.75*(1-statusMatrix[2,step])*(statusMatrix[1,step]-statusMatrix[3,step])/statusMatrix[2,step]))
        }else{
          statusMatrix[2,(step+1)]<-statusMatrix[2,step]+max((statusMatrix[1,step]-statusMatrix[3,step]),-0.75*(statusMatrix[2,step]))
        }
        step<-step+1
        print("Status matrix = ")
        print(statusMatrix)
        forecast.run$SPRtarget<<-as.numeric(statusMatrix[2,step])
        wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
      }else{
        if(TargetVal<3){
          TargetVal<-TargetVal+1
          statusMatrix[1,(step+1)]<-TargetVals[TargetVal]
          statusMatrix[2,(step+1)]<-statusMatrix[2,step]+(TargetVals[TargetVal]-TargetVals[(TargetVal-1)])*(1-statusMatrix[2,step])
          step<-step+1
          print("Status matrix = ")
          print(statusMatrix)
          forecast.run$SPRtarget<<-as.numeric(statusMatrix[2,step])
          wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
        }else{
          step<-step+1
          findingTargets<-FALSE
        }
      }
      }

      findingMSY<-TRUE
      stepSize<-1
      newGuess1<-matrix(NA,nrow=0,ncol=5)
      newGuess2<-matrix(NA,nrow=0,ncol=5)
      lowMSY<-matrix(c(0,0,0,0),nrow=4,ncol=1)
      highMSY<-matrix(c(0,0,0,0),nrow=4,ncol=1)
      while(findingMSY==TRUE)
      {
        incProgress(0.00, detail = paste0("Finding MSY - search loop ",step," of probably 10-15 (max 30)- finding MSY. Calculating next search estimate")) #0.4 total

        BestGuesses<-statusMatrix[,order(statusMatrix[4,],statusMatrix[3,],statusMatrix[2,],decreasing = TRUE)]

        bestMSY<-BestGuesses[,1,drop=FALSE]

        lowMSY<-BestGuesses[,BestGuesses[2,]<bestMSY[2,1],drop=FALSE]
        lowMSY<-lowMSY[,lowMSY[3,]<=bestMSY[3,1],drop=FALSE]
        lowMSY<-lowMSY[,!is.na(lowMSY[3,]),drop=FALSE]
        lowMSY<-lowMSY[,lowMSY[3,]==max(lowMSY[3,]),drop=FALSE]
        lowMSY<-lowMSY[,1,drop=FALSE]

        highMSY<-BestGuesses[,BestGuesses[2,]>bestMSY[2,],drop=FALSE]
        highMSY<-highMSY[,highMSY[3,]>=bestMSY[3,],drop=FALSE]
        highMSY<-highMSY[,!is.na(highMSY[3,]),drop=FALSE]
        highMSY<-highMSY[,highMSY[3,]==min(highMSY[3,]),drop=FALSE]
        highMSY<-highMSY[,1,drop=FALSE]

        newGuess1<-rbind(FitParabola(c(bestMSY[2,1],lowMSY[2,1],highMSY[2,1]),c(bestMSY[4,1],lowMSY[4,1],highMSY[4,1])),newGuess1)
        newGuess2<-rbind(FitParabola(c(bestMSY[3,1],lowMSY[3,1],highMSY[3,1]),c(bestMSY[4,1],lowMSY[4,1],highMSY[4,1])),newGuess2)

        statusMatrix[1,step]<-0.8*newGuess2[1,4]+0.1*lowMSY[3,1]+0.1*highMSY[3,1]
        statusMatrix[2,step]<-0.8*newGuess1[1,4]+0.1*lowMSY[2,1]+0.1*highMSY[2,1]

        print(paste("best msy = ",bestMSY))
        print(paste("low msy = ",lowMSY))
        print(paste("high msy = ",highMSY))
        print(paste("new guess 1 = ",newGuess1))
        print(paste("new guess 2 = ",newGuess2))

        print("Status matrix = ")
        print(statusMatrix)
        #statusMatrix[1,(6+Search.Iters)]<-(bestMSY[3,1]*(bestMSY[4,1])+highMSY[3,1]*(highMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize)+lowMSY[3,1]*(lowMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize))/(bestMSY[4,1]+(highMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize)+(lowMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize))
        #statusMatrix[2,(6+Search.Iters)]<-(bestMSY[2,1]*(bestMSY[4,1])+highMSY[2,1]*(highMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize)+lowMSY[2,1]*(lowMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize))/(bestMSY[4,1]+(highMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize)+(lowMSY[4,1]+stepSize*bestMSY[4,1])/(1+stepSize))


        #if((highMSY[3,1]-bestMSY[3,1])>(bestMSY[3,1]-lowMSY[3,1])){
        # }else{
        #   statusMatrix[1,(6+Search.Iters)]<-(bestMSY[3,1]*(bestMSY[4,1]+stepSize*lowMSY[4,1])+lowMSY[3,1]*(lowMSY[4,1]+stepSize*bestMSY[4,1]))/((bestMSY[4,1]+stepSize*lowMSY[4,1])+(lowMSY[4,1]+stepSize*bestMSY[4,1]))
        #   statusMatrix[2,(6+Search.Iters)]<-(bestMSY[2,1]*(bestMSY[4,1]+stepSize*lowMSY[4,1])+lowMSY[2,1]*(lowMSY[4,1]+stepSize*bestMSY[4,1]))/((bestMSY[4,1]+stepSize*lowMSY[4,1])+(lowMSY[4,1]+stepSize*bestMSY[4,1]))
        # }
        #statusMatrix[1,(4+Search.Iters)]<-BestGuesses[3,1]+stepSize*((BestGuesses[4,1]-BestGuesses[4,2])*(BestGuesses[3,1]-BestGuesses[3,2])+(BestGuesses[4,1]-BestGuesses[4,3])*(BestGuesses[3,1]-BestGuesses[3,3]))/((BestGuesses[4,1]-BestGuesses[4,2])+(BestGuesses[4,1]-BestGuesses[4,3]))
        #statusMatrix[2,(4+Search.Iters)]<-BestGuesses[2,1]+stepSize*((BestGuesses[4,1]-BestGuesses[4,2])*(BestGuesses[2,1]-BestGuesses[2,2])+(BestGuesses[4,1]-BestGuesses[4,3])*(BestGuesses[2,1]-BestGuesses[2,3]))/((BestGuesses[4,1]-BestGuesses[4,2])+(BestGuesses[4,1]-BestGuesses[4,3]))

        forecast.run$SPRtarget<<-as.numeric(statusMatrix[2,step])
        wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)

        #print(statusMatrix)

        incProgress(0.05, detail = paste0("Finding MSY - search loop ",step," of probably 10-15 (max 30)- finding MSY. Running SS3. Best estim so far MSY = ",bestMSY[4,1]," (",lowMSY[4,1],",",highMSY[4,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total

        shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))#
        #print(paste0("SS ran successfully :) ",Search.Iters))
        incProgress(0.00, detail = paste0("Finding MSY - search loop ",step," of probably 10-15 (max 30)- finding MSY. Reading output file. Best estim so far MSY = ",bestMSY[4,1]," (",lowMSY[4,1],",",highMSY[4,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total

        output.read<-FALSE
        output.temp<-NULL
        while(output.read==FALSE){
          try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
          if(is.null(output.temp)){
            output.read<-FALSE
            numCols<<-numCols+100
          }else{
            output.read<-TRUE
            output.run<<-output.temp
          }
        }
        incProgress(0.00, detail = paste0("Finding MSY - search loop ",step," of probably 10-15 (max 30)- finding MSY. Reading catch data. Best estim so far MSY = ",bestMSY[4,1]," (",lowMSY[4,1],",",highMSY[4,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total

        #print(paste0("Output read successfully ",Search.Iters))
        statusMatrix[3,step]<-sum(output.run$sprseries$SPR[output.run$sprseries$Year>=input$TargetYears[1] & output.run$sprseries$Year<=input$TargetYears[2]])/length(output.run$sprseries$SPR[output.run$sprseries$Year>=input$TargetYears[1] & output.run$sprseries$Year<=input$TargetYears[2]])
        Catch<-ReadCatch(output.run,input$TargetYears[1]:input$TargetYears[2])
        Catch<-Catch[Catch[,1]>=input$TargetYears[1] & Catch[,1]<=input$TargetYears[2],]
        statusMatrix[4,step]<-sum(Catch[,5])/length(unique(Catch[,1]))

        CatchAll<-ReadCatch(output.run,(input$ManagementYearInput+1):(input$TargetYears[1]-1))
        CatchAllAnnual<-aggregate(CatchAll[,5:6],list(CatchAll[,1]),sum,na.rm=TRUE)
        statusMatrix[5,step]<-CatchAllAnnual[1,2]
        if(length(min(CatchAllAnnual[,2],na.rm=TRUE))==0){
          statusMatrix[6,step]<-0
          statusMatrix[4,step]<-0
        }else if(min(CatchAllAnnual[,2],na.rm=TRUE)==Inf){
          statusMatrix[6,step]<-0
          statusMatrix[4,step]<-0
        }else if(min(CatchAllAnnual[,2],na.rm=TRUE)<=0){
          statusMatrix[3,step]<-0
          statusMatrix[6,step]<-0
          statusMatrix[4,step]<-0
        }else{
          statusMatrix[6,step]<-min(CatchAllAnnual[,2])
        }

        if(length(max(CatchAllAnnual[,2],na.rm=TRUE))==0){
          statusMatrix[7,step]<-0
          statusMatrix[4,step]<-0
        }else if(max(CatchAllAnnual[,2],na.rm=TRUE)==Inf){
          statusMatrix[7,step]<-0
          statusMatrix[4,step]<-0
        }else if(max(CatchAllAnnual[,2],na.rm=TRUE)<=0){
          statusMatrix[7,step]<-0
          statusMatrix[4,step]<-0
        }else{
          statusMatrix[7,step]<-max(CatchAllAnnual[,2])
        }

        # if(statusMatrix[6,step]<0.9*min(c(statusMatrix[4,step],statusMatrix[5,step]),na.rm=TRUE)){
        #   statusMatrix[4,step]<-0
        # }
        # if(statusMatrix[7,step]>1.1*max(c(statusMatrix[4,step],statusMatrix[5,step]),na.rm=TRUE)){
        #   statusMatrix[4,step]<-0
        # }

        if(statusMatrix[4,step]>BestGuesses[4,1]){

        }else{
          stepSize<-0.5*stepSize
        }
        step<-step+1
        print("Status matrix = ")
        print(statusMatrix)
        if(((highMSY[3,1]-lowMSY[3,1])<0.001)||(stepSize<0.001)||(step>30)){
          findingMSY<-FALSE
          outputName1<-"MSYSearch"
          outputName2<-"ParabolaGuesses1"
          outputName3<-"ParabolaGuesses2"
          dir.outputs<<-paste0(dir.base,"/outputs")
          if(!dir.exists(dir.outputs))
          {
            dir.create(dir.outputs)
          }else{
            files.outputs<-list.files(dir.outputs)
            if(length(grep(paste0("MSYSearch"),files.outputs))!=0){
              repNum<-1
              outputName1<-paste0("MSYSearch(",repNum,")")
              while(length(grep(paste0(outputName1),files.outputs,fixed=TRUE))!=0){
                repNum<-repNum+1
                outputName1<-paste0("MSYSearch(",repNum,")")
              }
            }

            if(length(grep(paste0("ParabolaGuesses1"),files.outputs))!=0){
              repNum<-1
              outputName2<-paste0("ParabolaGuesses1(",repNum,")")
              while(length(grep(paste0(outputName2),files.outputs,fixed=TRUE))!=0){
                repNum<-repNum+1
                outputName2<-paste0("ParabolaGuesses1(",repNum,")")
              }
            }

            if(length(grep(paste0("ParabolaGuesses2"),files.outputs))!=0){
              repNum<-1
              outputName3<-paste0("ParabolaGuesses2(",repNum,")")
              while(length(grep(paste0(outputName3),files.outputs,fixed=TRUE))!=0){
                repNum<-repNum+1
                outputName3<-paste0("ParabolaGuesses2(",repNum,")")
              }
            }
          }
          write.csv(statusMatrix,file=paste0(dir.outputs,"/",outputName1,".csv"),row.names = FALSE)
          write.csv(newGuess1,file=paste0(dir.outputs,"/",outputName2,".csv"),row.names = FALSE)
          write.csv(newGuess2,file=paste0(dir.outputs,"/",outputName3,".csv"),row.names = FALSE)
        }
      }
    }else if(input$ForecastTarget==3){
      #print("MSY == 3, start search")
      incProgress(0.1, detail = paste0("Finding MSY proxy of Spawning biomass ratio = ",input$TargetValue)) #0.4 total
      keep.searching<-TRUE
      Search.Iters<-0
      search.bounds<-matrix(c(0,0,1,1),nrow=2,ncol=2)
      while(keep.searching==TRUE)
      {
        incProgress(0.1, detail = paste0("Finding MSY proxy of Spawning biomass ratio = ",input$TargetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30)")) #0.4 total

        shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))#
        #print(paste0("SS ran successfully :) ",Search.Iters))
        incProgress(0.0, detail = paste0("Finding MSY proxy of Spawning biomass ratio = ",input$TargetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Reading output file")) #0.4 total

        output.read<-FALSE
        output.temp<-NULL
        while(output.read==FALSE){
          try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
          if(is.null(output.temp)){
            output.read<-FALSE
            numCols<<-numCols+100
          }else{
            output.read<-TRUE
            output.run<<-output.temp
          }
        }
        #print(paste0("Output read successfully ",Search.Iters))
        incProgress(0.0, detail = paste0("Finding MSY proxy of Spawning biomass ratio = ",input$TargetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Calculating new target")) #0.4 total

        comb.timeseries<-output.run$timeseries[output.run$timeseries$Area==1 & output.run$timeseries$Seas==1,1:8]
        comb.timeseries[,5:8]<-aggregate(output.run$timeseries[,5:8],list(output.run$timeseries[,2]),sum)[,2:5]
        meanSSB<-mean(comb.timeseries$SpawnBio[comb.timeseries$Yr>=input$TargetYears[1] & comb.timeseries$Yr<=input$TargetYears[2]])
        meanSPR<-meanSSB/comb.timeseries$SpawnBio[1]
        if(abs(meanSPR-input$TargetValue)>0.0001*input$TargetValue && Search.Iters<=30)
        {
          if(meanSPR<=input$TargetValue){
            search.bounds[,1]<-c(meanSPR,forecast.run$Btarget)
            forecast.run$Btarget<<-search.bounds[2,1]+(((input$TargetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))+(search.bounds[1,2]-search.bounds[1,1])*0.2*(1-((input$TargetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))))*(search.bounds[2,2]-search.bounds[2,1])
          }
          if(meanSPR>=input$TargetValue){
            search.bounds[,2]<-c(meanSPR,forecast.run$Btarget)
            forecast.run$Btarget<<-search.bounds[2,1]+((1-0.2*(search.bounds[1,2]-search.bounds[1,1]))*((input$TargetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1])))*(search.bounds[2,2]-search.bounds[2,1])
          }
          #forecast.run$Btarget<<-search.bounds[2,1]+(((input$TargetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))+0.2*(1-((input$TargetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))))*(search.bounds[2,2]-search.bounds[2,1])
          #forecast.run$Btarget<<-min(c(max(c(0.01,2*forecast.run$Btarget)),max(c(0.01,0.5*forecast.run$Btarget,(forecast.run$Btarget+0.5*(input$TargetValue-meanSPR))))))
          wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
          Search.Iters<-Search.Iters+1
        }else{
          keep.searching<-FALSE
        }
      }
      #forecast.run$Btarget<<-input$TargetValue
      #wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
    }

    incProgress(0.1, detail = paste0("Completed MSY search"))

    dir.msy<<-paste0(dir.base,"/msy")
    if(!dir.exists(dir.msy))
    {
      incProgress(0.0, detail = paste0("Building MSY directory"))
      dir.create(dir.msy)
      files.run<-list.files(dir.run)
      file.copy(paste0(dir.run,"/",files.run),dir.msy)
    }else{
      incProgress(0.0, detail = paste0("Saving MSY results"))
      files.msy<-list.files(dir.msy)
      unlink(paste0(dir.msy,"/",files.msy))
      files.run<-list.files(dir.run)
      file.copy(paste0(dir.run,"/",files.run),dir.msy)
    }
    incProgress(0.1, detail = paste0("Processing MSY catch data (this could take a few minutes)"))
    msyCatch<<-ReadCatch(output.run, (data.orig$styr-1):(data.orig$endyr+forecast.orig$Nforecastyrs))
    starter.msy<<-starter.run
    data.msy<<-data.run
    forecast.msy<<-forecast.run
    control.msy<<-control.run
    output.msy<<-output.run
    pars.msy<<-pars.run
  }else{
    if(is.list(starter.msy)){

    }else{
      incProgress(0, detail = "Reading MSY starter file")
      starter.msy<<-rd_starter(paste0(dir.msy,"/starter.ss"))
      incProgress(0, detail = "Reading MSY data file")
      data.msy<<-rd_data(file=paste0(dir.msy,"/",starter.msy$datfile))
      incProgress(0, detail = "Reading MSY forecast file")
      forecast.msy<<-rd_forecast(file=paste0(dir.msy,"/forecast.ss"),Nfleets=data.msy$Nfleet,Nareas = data.msy$N_areas,Nseas = data.msy$nseas)
      incProgress(0, detail = "Reading MSY control file")
      control.msy<<-rd_ctl(file=paste(dir.msy,"/",starter.msy$ctlfile,sep=""),data.msy)
      incProgress(0, detail = "Reading MSY parameter file")
      pars.msy<<-rd_par(file=paste(dir.msy,"/ss3.par",sep=""))
      output.read<-FALSE
      output.temp<-NULL
      incProgress(0.1, detail = "Reading output file")
      while(output.read==FALSE){
        try({output.temp<-rd_output(dir.msy, covar=F, ncol=numCols)})
        if(is.null(output.temp)){
          output.read<-FALSE
          numCols<<-numCols+100
        }else{
          output.read<-TRUE
          output.msy<<-output.temp
        }
      }
      incProgress(0.1, detail = "Reading MSY catch projections (this could take a few minutes)")
      msyCatch<<-ReadCatch(output.msy, (data.orig$styr-1):(data.orig$endyr+forecast.orig$Nforecastyrs))
      #print("MSY Output read successfully")
    }
  }
}

#' Run applied forecast
#'
#' Runs stock synthesis with -nohess and no parameter
#' estimation in order to produce projections. This
#' process is run only once with either catch or F
#' at a fixed value which may be user specified or
#' a ratio of the target forecast calculated value.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @param kobeComp a designator to the saved KOBE results
#' file name. Default=1
#' @author Nathan Vaughan
#' @keywords assessment projection
RunAppliedForecast<-function(input, output, session, kobeComp=1){
print("start applied forecast")
  incProgress(0.1, detail = paste0("Moving on to applied fishing forecast"))
  print(paste("forecast applied = ",input$ForecastApplied))
  print(paste("forecast applied = ",input$Rebuild))
  if(input$ForecastApplied==1){
    starter.run<<-starter.msy
    data.run<<-data.msy
    forecast.run<<-forecast.msy
    control.run<<-control.msy
    pars.run<<-pars.msy
    output.run<<-output.msy

    dir.run<<-paste0(dir.base,"/run")
    if(!dir.exists(dir.run))
    {
      incProgress(0.0, detail = paste0("Building Run directory"))
      #incProgress(0.1, detail = "Building base directory and formating assessment")
      dir.create(dir.run)
      files.msy<-list.files(dir.msy)
      file.copy(paste0(dir.msy,"/",files.msy),dir.run)
    }else{
      incProgress(0.0, detail = paste0("Saving run results"))
      files.run<-list.files(dir.run)
      unlink(paste0(dir.run,"/",files.run))
      files.msy<-list.files(dir.msy)
      file.copy(paste0(dir.msy,"/",files.msy),dir.run)
      #incProgress(0.1, detail = "Run directory present")
    }
  }else{
  #if(input$Rebuild!=1){ #No rebuilding plan
    if(input$ForecastApplied==2){ #Fish at fixed fraction of Fmsy
      print("start applied forecast: Fixed F fraction")
      incProgress(0.1, detail = paste0("Fishing at ",input$ABCfrac,"*Fmsy with no rebuilding plan")) #0.4 total
      projCatch<-msyCatch[msyCatch$Year>=input$TargetYears[1] & msyCatch$Year<=input$TargetYears[2],c(1,2,3,4,10)]
      #projCatch<-msyCatch[msyCatch$Year>=2065 & msyCatch$Year<=2076,c(1,2,3,4,10)]
      projCatch<-aggregate(projCatch,list(season=projCatch[,2],fleet=projCatch[,4]),sum)
      projCatch<-projCatch[,c(1,2,7)]
      projCatch[,3]<-input$ABCfrac*projCatch[,3]/(input$TargetYears[2]-input$TargetYears[1]+1)
      forecast.run$FirstYear_for_caps_and_allocations<<-data.run$endyr+forecast.run$Nforecastyrs+1
      print("start applied forecast: Aggregated projected catch")
      for(i in input$ManagementYearInput:(data.msy$endyr+forecast.msy$Nforecastyrs))
      {
        for(j in 1:data.msy$nseas)
        {
          for(k in 1:data.msy$Nfleet)
          {
            managementLimits$fullCatch[managementLimits$fullCatch[,1]==i & managementLimits$fullCatch[,2]==j & managementLimits$fullCatch[,3]==k,4:5]<-c(projCatch[projCatch[,1]==j & projCatch[,2]==k,3],99)
          }
        }
      }
      managementLimits$fullCatch<-managementLimits$fullCatch[!is.na(managementLimits$fullCatch[,4]),,drop=FALSE]
      print("start applied forecast: Updated management limits")
      forecast.run$Ncatch<<-length(managementLimits$fullCatch[,1])
      forecast.run$ForeCatch<<-as.data.frame(managementLimits$fullCatch)
      wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
      print("start applied forecast: Wrote forecast file")
      shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))
      print("start applied forecast: Ran SS loop")
    }else if(input$ForecastApplied==3){#Fish at fixed Catch
      projCatch<-msyCatch[msyCatch$Year>=input$TargetYears[1] & msyCatch$Year<=input$TargetYears[2],c(1,2,3,4,5)]
      #projCatch<-msyCatch[msyCatch$Year>=2065 & msyCatch$Year<=2076,c(1,2,3,4,10)]
      projCatch<-aggregate(projCatch,list(season=projCatch[,2],fleet=projCatch[,4]),sum)
      projCatch<-projCatch[,c(1,2,7)]
      if(input$ConstCatchInput==""){

        projCatch[,3]<-input$ConstCatchfrac*projCatch[,3]/(input$TargetYears[2]-input$TargetYears[1]+1)
      }else{
        incProgress(0.1, detail = paste0("Fishing at ",input$ABCfrac,"*MSY with no rebuilding plan")) #0.4 total
        projCatch[,3]<-as.numeric(input$ConstCatchInput)*projCatch[,3]/sum(projCatch[,3])
      }
      incProgress(0.1, detail = paste0("Fishing at ",sum(projCatch[,3])," ",weightNames[disp.Desc$Units[1]]," per year with no rebuilding plan")) #0.4 total
      forecast.run$FirstYear_for_caps_and_allocations<<-data.run$endyr+forecast.run$Nforecastyrs+1
      for(i in input$ManagementYearInput:(data.msy$endyr+forecast.msy$Nforecastyrs))
      {
        for(j in 1:data.msy$nseas)
        {
          for(k in 1:data.msy$Nfleet)
          {
            managementLimits$fullCatch[managementLimits$fullCatch[,1]==i & managementLimits$fullCatch[,2]==j & managementLimits$fullCatch[,3]==k,4:5]<-c(projCatch[projCatch[,1]==j & projCatch[,2]==k,3],3)
          }
        }
      }
      managementLimits$fullCatch<-managementLimits$fullCatch[!is.na(managementLimits$fullCatch[,4]),,drop=FALSE]
      forecast.run$Ncatch<<-length(managementLimits$fullCatch[,1])
      forecast.run$ForeCatch<<-as.data.frame(managementLimits$fullCatch)
      wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
      shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))
    }else if(input$ForecastApplied==4){#Fish at multiple fixed catches to create kobe matrix
      CatchOptions<-seq(as.numeric(input$KobeMinCatch),as.numeric(input$KobeMaxCatch),length.out = as.numeric(input$KobeNumCatch))
      for(ii in 1:as.numeric(input$KobeNumCatch))
      {
      projCatch<-msyCatch[msyCatch$Year>=input$TargetYears[1] & msyCatch$Year<=input$TargetYears[2],c(1,2,3,4,5)]
      projCatch<-aggregate(projCatch,list(season=projCatch[,2],fleet=projCatch[,4]),sum)
      projCatch<-projCatch[,c(1,2,7)]
      projCatch[,3]<-as.numeric(CatchOptions[ii])*projCatch[,3]/sum(projCatch[,3])
      incProgress(0.1, detail = paste0("Fishing at ",sum(projCatch[,3])," ",weightNames[disp.Desc$Units[1]]," per year with no rebuilding plan")) #0.4 total
      forecast.run$FirstYear_for_caps_and_allocations<<-data.run$endyr+forecast.run$Nforecastyrs+1
      for(i in input$ManagementYearInput:(data.msy$endyr+forecast.msy$Nforecastyrs))
      {
        for(j in 1:data.msy$nseas)
        {
          for(k in 1:data.msy$Nfleet)
          {
            managementLimits$fullCatch[managementLimits$fullCatch[,1]==i & managementLimits$fullCatch[,2]==j & managementLimits$fullCatch[,3]==k,4:5]<-c(projCatch[projCatch[,1]==j & projCatch[,2]==k,3],3)
          }
        }
      }
      managementLimits$fullCatch<-managementLimits$fullCatch[!is.na(managementLimits$fullCatch[,4]),,drop=FALSE]
      forecast.run$Ncatch<<-length(managementLimits$fullCatch[,1])
      forecast.run$ForeCatch<<-as.data.frame(managementLimits$fullCatch)
      wrt_forecast(forecast.run,dir=dir.run,overwrite = TRUE)
      shell(paste("cd /d ",dir.run," && ss3 -nohess",sep=""))
      output.read<-FALSE
      output.temp<-NULL
      incProgress(0.1, detail = "Reading new forecast output file")
      while(output.read==FALSE){
        try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
        if(is.null(output.temp)){
          output.read<-FALSE
          numCols<<-numCols+100
        }else{
          output.read<-TRUE
          output.run<<-output.temp
        }
      }
      incProgress(0.1, detail = paste0("Reading forecast catch data (this could take a few minutes)"))
      newCatch<<-ReadCatch(output.run, (data.orig$styr-1):(data.orig$endyr+forecast.orig$Nforecastyrs))

      if(ii==1){
       MSYoutput<-buildKobeMatrix(input,output,session,iteration=ii)
      }else{
       buildKobeMatrix(input,output,session,iteration=ii,MSYoutput=MSYoutput)
      }

      file.copy(from=paste0(dir.run,"/report.sso"),to=paste0(dir.base,"/",kobeComp,ii,"KobeReport.sso"),overwrite=TRUE)
      }
      return(NULL)
    }
  # }else{ #Rebuilding plan
  #   if(input$ForecastApplied==2){ #Fish at fixed fraction of Fmsy after fishing at the fraction of Fmsy required to meet rebuilding target
  #
  #   }else if(input$ForecastApplied==3){ #Fish at fixed Catch after fishing at the fixed catch required to meet rebuilding target
  #
  #   }
  # }
  }
  output.read<-FALSE
  output.temp<-NULL
  incProgress(0.1, detail = "Reading new forecast output file")
  while(output.read==FALSE){
    try({output.temp<-rd_output(dir.run, covar=F, ncol=numCols)})
    if(is.null(output.temp)){
      output.read<-FALSE
      numCols<<-numCols+100
    }else{
      output.read<-TRUE
      output.run<<-output.temp
    }
  }
  incProgress(0.1, detail = paste0("Reading forecast catch data (this could take a few minutes)"))
  newCatch<<-ReadCatch(output.run, (data.orig$styr-1):(data.orig$endyr+forecast.orig$Nforecastyrs))
  #print("Moving on to the build update output section")
  buildUpdateQuota(input,output,session)
  data.run<<-data.orig
  control.run<<-control.orig
  pars.run<<-pars.orig
  forecast.run<<-forecast.orig
  output.run<<-output.orig
}

#' Fit Parabola
#'
#' Estimates a parabolic fit through 3 paired estimates of
#' spawning stock biomass ratio and total sustainable yield in
#' order to predict the spawning stock biomass ratio that will
#' correspond to maximum sustainable yield.
#'
#'
#' @param x a vector of three spawning stock biomass ratio values.
#' @param y a vector of three total yield values.
#' @author Nathan Vaughan
#' @keywords model fitting
FitParabola <- function(x,y){
  a1<-(-x[1]^2)+(x[2]^2)
  b1<--x[1]+x[2]
  d1<--y[1]+y[2]
  a2<-(-x[2]^2)+(x[3]^2)
  b2<--x[2]+x[3]
  d2<--y[2]+y[3]
  bm<--(b2/b1)
  a3<-bm*a1+a2
  d3<-bm*d1+d2
  a<-d3/a3
  b<-(d1-a1*a)/b1
  c<-y[1]-a*(x[1]^2)-b*x[1]
  pars<-c(a,b,c)
  pcl<-seq(0.5*min(x),2*max(x),length.out = 1000)
  #plot(x=pcl,y=(a*(pcl^2)+b*pcl+c),xlim=c(0.5*min(x),2*max(x)),lwd=2,col="Blue")
  #points(x=x,y=y,pch=16,col="red")

  s<-a
  vert<--b/(2*a)
  sq<-(vert)^2
  off<-sq*s
  msy<-(c-off)
  #points(x=vert,y=msy,pch=16,col="purple")
  pars<-c(pars,vert,msy)
  return(pars)
}
