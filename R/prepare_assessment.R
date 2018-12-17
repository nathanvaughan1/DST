#' Prepare assessment
#'
#' This takes raw assessment files and prepares them for
#' use in the shiny decision support tool by including
#' needed parameters such as retention and discard
#' parameters even if they were not included in the original
#' assessment.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @param output.prep the output file list object of the assesment being preped.
#' @param starter.prep the starter file list object of the assesment being preped.
#' @param data.prep the data file list object of the assesment being preped.
#' @param control.prep the control file list object of the assesment being preped.
#' @param forecast.prep the forecast file list object of the assesment being preped.
#' @param pars.prep the parameter file list object of the assesment being preped.
#' @param disp.Desc the display file list object of the assesment being preped.
#' @param dir.prep the directory of the assesment being preped.
#' @author Nathan Vaughan
#' @keywords assessment projection
prepareAssessment<-function(input,output,session,output.prep,starter.prep,
                            data.prep,control.prep,forecast.prep,
                            pars.prep,disp.Desc,dir.prep)
{
  growthPars<-unlist(pars.prep$Values[(grep("# MGparm",pars.prep$Labels)-1)])
  control.prep$MG_Params[,3]<-growthPars[1:length(control.prep$MG_Params[,3])]
  if(length(control.prep$MG_Params_TV$ENV_Params[,3])>0){
    control.prep$MG_Params_TV$ENV_Params[,3]<-selectPars[(length(control.prep$MG_Params[,3])+1):(length(control.prep$MG_Params[,3])+length(control.prep$MG_Params_TV$ENV_Params[,3]))]
  }
  if(length(control.prep$MG_Params_TV$BLK_Params[,3])>0){
    control.prep$MG_Params_TV$BLK_Params[,3]<-selectPars[(length(control.prep$MG_Params[,3])+length(control.prep$MG_Params_TV$ENV_Params[,3])+1):(length(control.prep$MG_Params[,3])+length(control.prep$MG_Params_TV$ENV_Params[,3])+length(control.prep$MG_Params_TV$BLK_Params[,3]))]
  }

  srPars<-unlist(pars.prep$Values[(grep("# SR_parm",pars.prep$Labels)-1)])
  control.prep$SR_Params[,3]<-srPars[1:length(control.prep$SR_Params[,3])]

  selectPars<-unlist(pars.prep$Values[(grep("# selparm",pars.prep$Labels)-1)])
  control.prep$Select_Params[,3]<-selectPars[1:length(control.prep$Select_Params[,3])]
  if(length(control.prep$Select_Params_TV$ENV_Params[,3])>0){
    control.prep$Select_Params_TV$ENV_Params[,3]<-selectPars[(length(control.prep$Select_Params[,3])+1):(length(control.prep$Select_Params[,3])+length(control.prep$Select_Params_TV$ENV_Params[,3]))]
  }
  if(length(control.prep$Select_Params_TV$BLK_Params[,3])>0){
    control.prep$Select_Params_TV$BLK_Params[,3]<-selectPars[(length(control.prep$Select_Params[,3])+length(control.prep$Select_Params_TV$ENV_Params[,3])+1):(length(control.prep$Select_Params[,3])+length(control.prep$Select_Params_TV$ENV_Params[,3])+length(control.prep$Select_Params_TV$BLK_Params[,3]))]
  }

  starter.prep$init_values_src<-1
  starter.prep$run_display_detail<-1
  starter.prep$detailed_age_structure<-1
  starter.prep$checkup<-0
  starter.prep$parmtrace<-0
  starter.prep$cumreport<-0
  starter.prep$prior_like<-0
  starter.prep$soft_bounds<-1
  starter.prep$N_bootstraps<-0
  starter.prep$last_estimation_phase<-0
  starter.prep$MCMCburn<-10
  starter.prep$MCMCthin<-2
  starter.prep$jitter_fraction<-0
  starter.prep$minyr_sdreport<--1
  starter.prep$maxyr_sdreport<--2
  starter.prep$N_STD_yrs<-0
  #starter.prep$depl_basis<-1
  #starter.prep$depl_denom_frac<-1
  #starter.prep$SPR_basis<-4
  #starter.prep$F_report_units<-1
  #starter.prep$F_age_range<-NULL
  #starter.prep$F_report_basis<-0

  wrt_starter(starter.prep,dir.prep,overwrite = TRUE,warn=FALSE)

  #forecast.prep$benchmarks<-0
  #forecast.prep$MSY<-4
  #forecast.prep$Forecast<-4

  forecast.prep$Nforecastyrs<-100

  rowsRecDev<-grep("# recdev1",pars.prep$Labels)
  rowsFirstFinit<-grep("# init_F",pars.prep$Labels)[1]
  #pars.prep$Labels<-c(pars.prep$Labels[1:(rowsRecDev)],pars.prep$Labels[1:2],pars.prep$Labels[(rowsFirstFinit):length(pars.prep$Labels)])
  #pars.prep$Labels[[rowsRecDev+1]]<-"# Fcast_recruitments:"
  #pars.prep$Labels[[rowsRecDev+2]]<-"# Fcast_impl_error:"
  #pars.prep$Values<-c(pars.prep$Values[1:(rowsRecDev-1)],pars.prep$Values[1:2],pars.prep$Values[(rowsFirstFinit-1):length(pars.prep$Values)])
  #pars.prep$Values[[rowsRecDev]]<-rep(0,(forecast.prep$Nforecastyrs+(data.prep$endyr-control.prep$SR_EndYr)))
  #pars.prep$Values[[rowsRecDev+1]]<-rep(0,(forecast.prep$Nforecastyrs))

  forecast.prep$F_scalar<-1
  forecast.prep$ControlRuleMethod<-2
  forecast.prep$BforconstantF<-0.01
  forecast.prep$BfornoF<-0.001
  forecast.prep$Flimitfraction<-1
  forecast.prep$N_forecast_loops<-3
  forecast.prep$First_forecast_loop_with_stochastic_recruitment<-3
  forecast.prep$Forecast_loop_control_3<-0
  forecast.prep$Forecast_loop_control_4<-0
  forecast.prep$Forecast_loop_control_5<-0
  forecast.prep$stddev_of_log_catch_ratio<-0
  forecast.prep$Do_West_Coast_gfish_rebuilder_output<-0
  if(forecast.prep$InputBasis!=-1)
  {

    if(forecast.prep$Ncatch>0)
    {
      forecast.prep$ForeCatch$Basis<-forecast.prep$InputBasis
    }
    forecast.prep$InputBasis<--1
  }

  wrt_forecast(forecast.prep,dir=dir.prep,overwrite = TRUE)

  pattern<-matrix(nrow=34,ncol=2)
  pattern[,1]<-c(2,8,6,0,2,2,8,8,6,0,2,2,8,(data.prep$Nages+1),0,2,(data.prep$Nages+1),8,6,6,0,4,6,6,3,3,3,0,0,0,0,0,0,0)
  pattern[,2]<-c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0)

  maxSize<-100
  if(data.prep$lbin_method==1)
  {
    maxSize<-max(data.prep$lbin_vector)
  }else if(data.prep$lbin_method==2)
  {
    maxSize<-data.prep$maximum_size
  }else if(data.prep$lbin_method==3)
  {
    maxSize<-max(data.prep$lbin_vector_pop)
  }

  dummyRetain<-matrix(0,nrow=4,ncol=14, dimnames=list(paste0("Size Retention ",1:4," fleet/Survey ",1),c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase","UseEnv","UseDev","DevMinYr","DevMaxYr","DevStDev","UseBlock","BlockType")))
  dummyRetain[,1]<-c((-5*maxSize),-1000000,0,(-5*maxSize))
  dummyRetain[,2]<-c((6*maxSize),1000000,1,(6*maxSize))
  dummyRetain[,3]<-c(-1,0,1,0)
  dummyRetain[,5]<--1
  dummyRetain[,7]<--1
  dummyDisc<-dummyRetain

  prevParLab<-grep("# selparm",pars.prep$Labels)[1]-1
  prevParVal<-prevParLab-1
  prevRow<-0
  mirrorFleets<-control.prep$Size_Select[(1:data.prep$Nfleet),2]
  mirrorFleets<-mirrorFleets[mirrorFleets<0]
  if(length(mirrorFleets)>0)
  {
    mirrorFleets<--mirrorFleets
    mirrorList<-list()
  }
  for(i in 1:data.prep$Nfleet)
  {
    if(control.prep$Size_Select[i,1]==0){}else{
      prevRow<-prevRow+pattern[control.prep$Size_Select[i,1],1]+pattern[control.prep$Size_Select[i,1],2]*control.prep$Size_Select[i,4]
      prevParLab<-prevParLab+pattern[control.prep$Size_Select[i,1],1]+pattern[control.prep$Size_Select[i,1],2]*control.prep$Size_Select[i,4]
      prevParVal<-prevParVal+pattern[control.prep$Size_Select[i,1],1]+pattern[control.prep$Size_Select[i,1],2]*control.prep$Size_Select[i,4]
    }

    if(control.prep$Size_Select[i,2]<0)
    {
      mirrorRetain<-mirrorList[[(-control.prep$Size_Select[i,2])]][1:4,]
      dimnames(mirrorRetain)[[1]]<-paste0("Size Retention ",1:4," fleet/Survey ",i)
      mirrorDisc<-mirrorList[[(-control.prep$Size_Select[i,2])]][5:8,]
      dimnames(mirrorDisc)[[1]]<-paste0("Size Discard ",1:4," fleet/Survey ",i)
      control.prep$Size_Select[i,2]<-2
      control.prep$Select_Params<-rbind(control.prep$Select_Params[0:(prevRow),],mirrorRetain,mirrorDisc,control.prep$Select_Params[(prevRow+1):length(control.prep$Select_Params[,1]),])
      pars.prep$Labels<-c(pars.prep$Labels[0:prevParLab],dimnames(mirrorRetain)[[1]],dimnames(mirrorDisc)[[1]],pars.prep$Labels[(prevParLab+1):length(pars.prep$Labels)])
      pars.prep$Values<-c(pars.prep$Values[0:prevParVal],mirrorRetain[,3],mirrorDisc[,3],pars.prep$Values[(prevParVal+1):length(pars.prep$Values)])
      prevRow<-prevRow+8
      prevParLab<-prevParLab+8
      prevParVal<-prevParVal+8

    }else if(control.prep$Size_Select[i,2]==2)
    {
      pars.prep$Labels[(prevParLab+1):(prevParLab+8)]<-c(paste0("# selparm[",(prevRow+1):(prevRow+8),"]"))
      prevRow<-prevRow+8
      prevParLab<-prevParLab+8
      prevParVal<-prevParVal+8
    }else if(control.prep$Size_Select[i,2]==0)
    {
      dummyRetain[,3]<-c(-1,0,1,0)
      dimnames(dummyRetain)[[1]]<-paste0("Size Retention ",1:4," fleet/Survey ",i)
      dimnames(dummyDisc)[[1]]<-paste0("Size Discard ",1:4," fleet/Survey ",i)
      control.prep$Size_Select[i,2]<-2
      control.prep$Select_Params<-rbind(control.prep$Select_Params[0:(prevRow),],dummyRetain,dummyDisc,control.prep$Select_Params[(prevRow+1):length(control.prep$Select_Params[,1]),])
      pars.prep$Labels<-c(pars.prep$Labels[0:prevParLab],paste0("# selparm[",(prevRow+1):(prevRow+8),"]"),pars.prep$Labels[(prevParLab+1):length(pars.prep$Labels)])
      pars.prep$Values<-c(pars.prep$Values[0:prevParVal],dummyRetain[,3],dummyDisc[,3],pars.prep$Values[(prevParVal+1):length(pars.prep$Values)])
      prevRow<-prevRow+8
      prevParLab<-prevParLab+8
      prevParVal<-prevParVal+8
    }else if(control.prep$Size_Select[i,2]==1)
    {
      prevRow<-prevRow+4
      prevParLab<-prevParLab+4
      prevParVal<-prevParVal+4
      dimnames(dummyDisc)[[1]]<-paste0("Size Discard ",1:4," fleet/Survey ",i)
      control.prep$Size_Select[i,2]<-2
      control.prep$Select_Params<-rbind(control.prep$Select_Params[0:(prevRow),],dummyDisc,control.prep$Select_Params[(prevRow+1):length(control.prep$Select_Params[,1]),])
      pars.prep$Labels<-c(pars.prep$Labels[0:prevParLab],paste0("# selparm[",(prevRow+1):(prevRow+4),"]"),pars.prep$Labels[(prevParLab+1):length(pars.prep$Labels)])
      pars.prep$Values<-c(pars.prep$Values[0:prevParVal],dummyDisc[,3],pars.prep$Values[(prevParVal+1):length(pars.prep$Values)])
      prevRow<-prevRow+4
      prevParLab<-prevParLab+4
      prevParVal<-prevParVal+4
    }else if(control.prep$Size_Select[i,2]==3)
    {
      dummyRetain[,3]<-c(-1,0,0,0)
      dimnames(dummyRetain)[[1]]<-paste0("Size Retention ",1:4," fleet/Survey ",i)
      dimnames(dummyDisc)[[1]]<-paste0("Size Discard ",1:4," fleet/Survey ",i)
      control.prep$Size_Select[i,2]<-2
      control.prep$Select_Params<-rbind(control.prep$Select_Params[0:(prevRow),],dummyRetain,dummyDisc,control.prep$Select_Params[(prevRow+1):length(control.prep$Select_Params[,1]),])
      pars.prep$Labels<-c(pars.prep$Labels[0:prevParLab],paste0("# selparm[",(prevRow+1):(prevRow+8),"]"),pars.prep$Labels[(prevParLab+1):length(pars.prep$Labels)])
      pars.prep$Values<-c(pars.prep$Values[0:prevParVal],dummyRetain[,3],dummyDisc[,3],pars.prep$Values[(prevParVal+1):length(pars.prep$Values)])
      prevRow<-prevRow+8
      prevParLab<-prevParLab+8
      prevParVal<-prevParVal+8
    }

    if(is.element(i,mirrorFleets))
    {
      mirrorList[[i]]<-control.prep$Select_Params[(prevRow-7):prevRow,]
    }

    if(control.prep$Size_Select[i,3]>0)
    {
      prevRow<-prevRow+4
    }
  }

  wrt_ctl(file=paste0(dir.prep,"/",starter.prep$ctlfile),data.prep,control.prep)
  wrt_par(file=paste(dir.prep,"/ss3.par",sep=""),pars.prep)


  if(forecast.prep$MSY==1)
  {
    incProgress(0.1, detail = paste0("Finding MSY proxy of SPR = ",forecast.prep$SPRtarget)) #0.4 total
    print("MSY == 1, start search")
    keep.searching<-TRUE
    Search.Iters<-0
    search.bounds<-matrix(c(0,0,1,1),nrow=2,ncol=2)
    targetValue<-forecast.prep$SPRtarget
    while(keep.searching==TRUE)
    {
      incProgress(0.1, detail = paste0("Finding MSY proxy of SPR = ",targetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30)")) #0.4 total
      shell(paste("cd /d ",dir.prep," && ss3 -nohess",sep=""))
      print("SS ran successfully :)")
      incProgress(0.0, detail = paste0("Finding MSY proxy of SPR = ",targetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Reading output")) #0.4 total

      output.read<-FALSE
      output.temp<-NULL
      while(output.read==FALSE){
        try({output.temp<-rd_output(dir.prep, covar=F, ncol=numCols)})
        if(is.null(output.temp)){
          output.read<-FALSE
          numCols<-numCols+100
        }else{
          output.read<-TRUE
          output.prep<-output.temp
        }
      }
      print("Output read successfully")
      incProgress(0.0, detail = paste0("Finding MSY proxy of SPR = ",targetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Calculating new target")) #0.4 total

      equilSPR<-mean(output.prep$sprseries$SPR[output.prep$sprseries$Year>=disp.Desc$TargetYears[1] & output.prep$sprseries$Year<=disp.Desc$TargetYears[2]])
      if(abs(equilSPR-targetValue)>0.0001*targetValue && Search.Iters<=30)
      {
        if(equilSPR<=targetValue){
          search.bounds[,1]<-c(equilSPR,forecast.prep$SPRtarget)
          forecast.prep$SPRtarget<-search.bounds[2,1]+(((targetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))+(search.bounds[1,2]-search.bounds[1,1])*0.2*(1-((targetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))))*(search.bounds[2,2]-search.bounds[2,1])
        }
        if(equilSPR>=targetValue){
          search.bounds[,2]<-c(equilSPR,forecast.prep$SPRtarget)
          forecast.prep$SPRtarget<-search.bounds[2,1]+((1-0.2*(search.bounds[1,2]-search.bounds[1,1]))*((targetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1])))*(search.bounds[2,2]-search.bounds[2,1])
        }
        incProgress(0.0, detail = paste0("Finding MSY proxy of SPR = ",targetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Writing new forecast file")) #0.4 total
        wrt_forecast(forecast.prep,dir=dir.prep,overwrite = TRUE)
        Search.Iters<-Search.Iters+1
      }else{
        keep.searching<-FALSE
      }
    }
    forecast.prep$SPRtarget<-targetValue
  }else if(forecast.prep$MSY==2){
    incProgress(0.1, detail = paste0("Finding MSY")) #0.4 total
    print("MSY == 2, start search")
    statusMatrix<-matrix(NA,nrow=4,ncol=30,dimnames = list(c("target SPR","input SPR","achieved SPR","achieved Catch")))
    targetValue<-forecast.prep$SPRtarget
    statusMatrix[,1]<-c(0,0,0,0)
    statusMatrix[,2]<-c(1,1,1,0)
    statusMatrix[1,3]<-0.3
    statusMatrix[2,3]<-0.3
    forecast.prep$MSY<-1
    forecast.prep$Forecast<-1
    forecast.prep$SPRtarget<-as.numeric(statusMatrix[2,3])
    wrt_forecast(forecast.prep,dir=dir.prep,overwrite = TRUE)
    incProgress(0.05, detail = paste0("Finding MSY - search loop ",1," of probably 10-15 (max 30)")) #0.4 total

    shell(paste("cd /d ",dir.prep," && ss3 -nohess",sep=""))
    print("SS ran successfully 1 :)")
    incProgress(0.00, detail = paste0("Finding MSY - search loop ",1," of probably 10-15 (max 30). Reading output file")) #0.4 total

    numCols<-200
    output.read<-FALSE
    output.temp<-NULL
    while(output.read==FALSE){
      try({output.temp<-rd_output(dir.prep, covar=F, ncol=numCols)})
      if(is.null(output.temp)){
        output.read<-FALSE
        numCols<-numCols+100
      }else{
        output.read<-TRUE
        output.prep<-output.temp
      }
    }

    print("Output read successfully 1")
    incProgress(0.00, detail = paste0("Finding MSY - search loop ",1," of probably 10-15 (max 30). Reading catch results")) #0.4 total

    print(output.prep$sprseries$SPR)
    print(disp.Desc)
    statusMatrix[3,3]<-mean(output.prep$sprseries$SPR[output.prep$sprseries$Year>=disp.Desc$TargetYears[1] & output.prep$sprseries$Year<=disp.Desc$TargetYears[2]])
    Catch<-ReadCatch(output.prep,disp.Desc$TargetYears[1]:disp.Desc$TargetYears[2])
    Catch<-Catch[Catch[,1]>=disp.Desc$TargetYears[1] & Catch[,1]<=disp.Desc$TargetYears[2],]
    statusMatrix[4,3]<-sum(Catch[,5])/length(unique(Catch[,1]))

    if(abs(statusMatrix[1,3]-statusMatrix[3,3])>0.05)
    {
      statusMatrix[1,4]<-0.3
      statusMatrix[2,4]<-0.6-statusMatrix[3,3]
    }else{
      statusMatrix[1,4]<-0.2
      statusMatrix[2,4]<-0.5-statusMatrix[2,3]
    }

    forecast.prep$SPRtarget<-as.numeric(statusMatrix[2,4])
    wrt_forecast(forecast.prep,dir=dir.prep,overwrite = TRUE)

    print(statusMatrix)
    incProgress(0.05, detail = paste0("Finding MSY - search loop ",2," of probably 10-15 (max 30)")) #0.4 total

    shell(paste("cd /d ",dir.prep," && ss3 -nohess",sep=""))
    print("SS ran successfully 2 :)")
    incProgress(0.00, detail = paste0("Finding MSY - search loop ",2," of probably 10-15 (max 30). Reading output file")) #0.4 total

    output.read<-FALSE
    output.temp<-NULL
    while(output.read==FALSE){
      try({output.temp<-rd_output(dir.prep, covar=F, ncol=numCols)})
      if(is.null(output.temp)){
        output.read<-FALSE
        numCols<-numCols+100
      }else{
        output.read<-TRUE
        output.prep<-output.temp
      }
    }
    incProgress(0.00, detail = paste0("Finding MSY - search loop ",2," of probably 10-15 (max 30). Reading catch results"))
    print("Output read successfully 2")
    statusMatrix[3,4]<-mean(output.prep$sprseries$SPR[output.prep$sprseries$Year>=disp.Desc$TargetYears[1] & output.prep$sprseries$Year<=disp.Desc$TargetYears[2]])
    Catch<-ReadCatch(output.prep,disp.Desc$TargetYears[1]:disp.Desc$TargetYears[2])
    Catch<-Catch[Catch[,1]>=disp.Desc$TargetYears[1] & Catch[,1]<=disp.Desc$TargetYears[2],]
    statusMatrix[4,4]<-sum(Catch[,5])/length(unique(Catch[,1]))

    if(statusMatrix[4,3]>statusMatrix[4,4])
    {
      statusMatrix[1,5]<-statusMatrix[3,3]+(statusMatrix[3,3]-statusMatrix[3,4])
      statusMatrix[2,5]<-statusMatrix[2,3]+(statusMatrix[2,3]-statusMatrix[2,4])
    }else{
      statusMatrix[1,5]<-statusMatrix[3,4]+(statusMatrix[3,4]-statusMatrix[3,3])
      statusMatrix[2,5]<-statusMatrix[2,4]+(statusMatrix[2,4]-statusMatrix[2,3])
    }

    forecast.prep$SPRtarget<-as.numeric(statusMatrix[2,5])
    wrt_forecast(forecast.prep,dir=dir.prep,overwrite = TRUE)

    print(statusMatrix)

    incProgress(0.05, detail = paste0("Finding MSY - search loop ",3," of probably ~10 (max 30)")) #0.4 total

    shell(paste("cd /d ",dir.prep," && ss3 -nohess",sep=""))#
    print("SS ran successfully 3 :)")
    incProgress(0.00, detail = paste0("Finding MSY - search loop ",3," of probably ~10 (max 30). Reading output file")) #0.4 total

    output.read<-FALSE
    output.temp<-NULL
    while(output.read==FALSE){
      try({output.temp<-rd_output(dir.prep, covar=F, ncol=numCols)})
      if(is.null(output.temp)){
        output.read<-FALSE
        numCols<-numCols+100
      }else{
        output.read<-TRUE
        output.prep<-output.temp
      }
    }
    incProgress(0.00, detail = paste0("Finding MSY - search loop ",3," of probably ~10 (max 30). Reading catch results"))
    print("Output read successfully 3")
    statusMatrix[3,5]<-mean(output.prep$sprseries$SPR[output.prep$sprseries$Year>=disp.Desc$TargetYears[1] & output.prep$sprseries$Year<=disp.Desc$TargetYears[2]])
    Catch<-ReadCatch(output.prep,disp.Desc$TargetYears[1]:disp.Desc$TargetYears[2])
    Catch<-Catch[Catch[,1]>=disp.Desc$TargetYears[1] & Catch[,1]<=disp.Desc$TargetYears[2],]
    statusMatrix[4,5]<-sum(Catch[,5])/length(unique(Catch[,1]))

    keep.searching<-TRUE
    Search.Iters<-0
    stepSize<-1
    newGuess1<-matrix(NA,nrow=0,ncol=5)
    newGuess2<-matrix(NA,nrow=0,ncol=5)
    lowMSY<-matrix(c(0,0,0,0),nrow=4,ncol=1)
    highMSY<-matrix(c(0,0,0,0),nrow=4,ncol=1)
    while(keep.searching==TRUE)
    {

      BestGuesses<-statusMatrix[,order(statusMatrix[4,],decreasing = TRUE)]

      bestMSY<-BestGuesses[,1,drop=FALSE]

      lowMSY<-BestGuesses[,BestGuesses[3,]<bestMSY[3,],drop=FALSE]
      lowMSY<-lowMSY[,!is.na(lowMSY[3,]),drop=FALSE]
      lowMSY<-lowMSY[,lowMSY[3,]==max(lowMSY[3,]),drop=FALSE]
      lowMSY<-lowMSY[,1,drop=FALSE]

      highMSY<-BestGuesses[,BestGuesses[3,]>bestMSY[3,],drop=FALSE]
      highMSY<-highMSY[,!is.na(highMSY[3,]),drop=FALSE]
      highMSY<-highMSY[,highMSY[3,]==min(highMSY[3,]),drop=FALSE]
      highMSY<-highMSY[,1,drop=FALSE]

      newGuess1<-rbind(FitParabola(c(bestMSY[2,1],lowMSY[2,1],highMSY[2,1]),c(bestMSY[4,1],lowMSY[4,1],highMSY[4,1])),newGuess1)
      newGuess2<-rbind(FitParabola(c(bestMSY[3,1],lowMSY[3,1],highMSY[3,1]),c(bestMSY[4,1],lowMSY[4,1],highMSY[4,1])),newGuess2)

      statusMatrix[1,(6+Search.Iters)]<-0.8*newGuess2[1,4]+0.1*lowMSY[3,1]+0.1*highMSY[3,1]
      statusMatrix[2,(6+Search.Iters)]<-0.8*newGuess1[1,4]+0.1*lowMSY[2,1]+0.1*highMSY[2,1]

      forecast.prep$SPRtarget<-as.numeric(statusMatrix[2,(6+Search.Iters)])
      wrt_forecast(forecast.prep,dir=dir.prep,overwrite = TRUE)

      print(statusMatrix)

      incProgress(0.05, detail = paste0("Finding MSY - search loop ",(4+Search.Iters)," of probably ~10 (max 30). Running SS3. Best estim so far MSY = ",bestMSY[4,1]," (",lowMSY[4,1],",",highMSY[4,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total

      shell(paste("cd /d ",dir.prep," && ss3 -nohess",sep=""))#
      print(paste0("SS ran successfully :) ",Search.Iters))
      incProgress(0.00, detail = paste0("Finding MSY - search loop ",(4 +Search.Iters)," of probably ~10 (max 30). Reading output file. Best estim so far MSY = ",bestMSY[4,1]," (",lowMSY[4,1],",",highMSY[4,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total

      output.read<-FALSE
      output.temp<-NULL
      while(output.read==FALSE){
        try({output.temp<-rd_output(dir.prep, covar=F, ncol=numCols)})
        if(is.null(output.temp)){
          output.read<-FALSE
          numCols<-numCols+100
        }else{
          output.read<-TRUE
          output.prep<-output.temp
        }
      }
      incProgress(0.00, detail = paste0("Finding MSY - search loop ",(4 +Search.Iters)," of probably ~10 (max 30). Reading catch data. Best estim so far MSY = ",bestMSY[4,1]," (",lowMSY[4,1],",",highMSY[4,1],") at SPR = ",bestMSY[3,1],"(",lowMSY[3,1],",",highMSY[3,1],")")) #0.4 total

      print(paste0("Output read successfully ",Search.Iters))
      statusMatrix[3,(6+Search.Iters)]<-sum(output.prep$sprseries$SPR[output.prep$sprseries$Year>=disp.Desc$TargetYears[1] & output.prep$sprseries$Year<=disp.Desc$TargetYears[2]])/length(output.prep$sprseries$SPR[output.prep$sprseries$Year>=disp.Desc$TargetYears[1] & output.prep$sprseries$Year<=disp.Desc$TargetYears[2]])
      Catch<-ReadCatch(output.prep,disp.Desc$TargetYears[1]:disp.Desc$TargetYears[2])
      Catch<-Catch[Catch[,1]>=disp.Desc$TargetYears[1] & Catch[,1]<=disp.Desc$TargetYears[2],]
      statusMatrix[4,(6+Search.Iters)]<-sum(Catch[,5])/length(unique(Catch[,1]))

      if(statusMatrix[4,(6+Search.Iters)]>BestGuesses[4,1]){

      }else{
        stepSize<-0.5*stepSize
      }
      Search.Iters<-Search.Iters+1

      if(((highMSY[3,1]-lowMSY[3,1])<0.001)||(stepSize<0.001)||(Search.Iters>30)){
        keep.searching<-FALSE
        outputName1<-"MSYSearch"
        outputName2<-"ParabolaGuesses1"
        outputName3<-"ParabolaGuesses2"

        write.csv(statusMatrix,file=paste0(dir.prep,"/",outputName1,".csv"),row.names = FALSE)
        write.csv(newGuess1,file=paste0(dir.prep,"/",outputName2,".csv"),row.names = FALSE)
        write.csv(newGuess2,file=paste0(dir.prep,"/",outputName3,".csv"),row.names = FALSE)
      }
    }
    forecast.prep$MSY<-2
    forecast.prep$Forecast<-2
    forecast.prep$SPRtarget<-targetValue
  }else if(forecast.prep$MSY==3){
    print("MSY == 3, start search")
    targetValue<-forecast.prep$Btarget
    incProgress(0.1, detail = paste0("Finding MSY proxy of Spawning biomass ratio = ",targetValue)) #0.4 total
    keep.searching<-TRUE
    Search.Iters<-0
    search.bounds<-matrix(c(0,0,1,1),nrow=2,ncol=2)
    while(keep.searching==TRUE)
    {
      incProgress(0.1, detail = paste0("Finding MSY proxy of Spawning biomass ratio = ",targetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30)")) #0.4 total

      shell(paste("cd /d ",dir.prep," && ss3 -nohess",sep=""))#
      print(paste0("SS ran successfully :) ",Search.Iters))
      incProgress(0.0, detail = paste0("Finding MSY proxy of Spawning biomass ratio = ",targetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Reading output file")) #0.4 total

      output.read<-FALSE
      output.temp<-NULL
      while(output.read==FALSE){
        try({output.temp<-rd_output(dir.prep, covar=F, ncol=numCols)})
        if(is.null(output.temp)){
          output.read<-FALSE
          numCols<-numCols+100
        }else{
          output.read<-TRUE
          output.prep<-output.temp
        }
      }
      print(paste0("Output read successfully ",Search.Iters))
      incProgress(0.0, detail = paste0("Finding MSY proxy of Spawning biomass ratio = ",targetValue," - search loop ",(Search.Iters+1)," of probably 3-5 (max 30). Calculating new target")) #0.4 total

      comb.timeseries<-output.prep$timeseries[output.prep$timeseries$Area==1 & output.prep$timeseries$Seas==1,1:8]
      comb.timeseries[,5:8]<-aggregate(output.prep$timeseries[,5:8],list(output.prep$timeseries[,2]),sum)[,2:5]
      meanSSB<-mean(comb.timeseries$SpawnBio[comb.timeseries$Yr>=disp.Desc$TargetYears[1] & comb.timeseries$Yr<=disp.Desc$TargetYears[2]])
      meanSPR<-meanSSB/comb.timeseries$SpawnBio[1]
      if(abs(meanSPR-targetValue)>0.0001*targetValue && Search.Iters<=30)
      {
        if(meanSPR<=targetValue){
          search.bounds[,1]<-c(meanSPR,forecast.prep$Btarget)
          forecast.prep$Btarget<-search.bounds[2,1]+(((targetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))+(search.bounds[1,2]-search.bounds[1,1])*0.2*(1-((targetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1]))))*(search.bounds[2,2]-search.bounds[2,1])
        }
        if(meanSPR>=targetValue){
          search.bounds[,2]<-c(meanSPR,forecast.prep$Btarget)
          forecast.prep$Btarget<-search.bounds[2,1]+((1-0.2*(search.bounds[1,2]-search.bounds[1,1]))*((targetValue-search.bounds[1,1])/(search.bounds[1,2]-search.bounds[1,1])))*(search.bounds[2,2]-search.bounds[2,1])
        }
        wrt_forecast(forecast.prep,dir=dir.prep,overwrite = TRUE)
        Search.Iters<-Search.Iters+1
      }else{
        keep.searching<-FALSE
      }
    }
    forecast.prep$Btarget<-targetValue
  }else{
    shell(paste("cd /d ",dir.prep," && ss3 -nohess",sep=""))#
    print("Basic run successfull")
  }
  wrt_forecast(forecast.prep,dir=dir.prep,overwrite = TRUE)
}
