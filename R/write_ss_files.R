#' write starter file
#'
#' write Stock Synthesis starter file from list object in R which was probably
#' created using \code{\link{rd_starter}}
#'
#'
#' @param mylist List object created by \code{\link{rd_starter}}.
#' @param dir Directory for new starter file. Default=NULL (working directory).
#' @param file Filename for new starter file. Default="starter.ss".
#' @param overwrite Should existing files be overwritten? Default=TRUE.
#' @param verbose Should there be verbose output while running the file?
#' Default=FALSE.
#' @param warn Print warning if overwriting file?
#' @author Ian Taylor
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_forecast}},
#' \code{\link{rd_ctl}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data export
wrt_starter <- function(mylist, dir=NULL, file="starter.ss",
                            overwrite=TRUE, verbose=FALSE, warn=FALSE){
  if(verbose) cat("running wrt_starter\n")
  if(mylist$type!="Stock_Synthesis_starter_file"){
    stop("input 'mylist' should be a list with $type=='Stock_Synthesis_starter_file'\n")
  }
  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(is.null(dir)) dir <- getwd() # set to working directory if no input provided
  if(grepl("/$", dir)) {
    outfile <- paste0(dir, file) # bc trailing backslash
  } else {
    outfile <- paste(dir,file,sep="/")
  }
  if(file.exists(outfile)){
    if(!overwrite){
      stop(paste("file exists:",outfile,"\n  set overwrite=TRUE to replace\n"))
    }else{
      if(warn) {cat("overwriting file:",outfile,"\n")}
      file.remove(outfile)
    }
  }else{
    if(verbose)cat("writing new file:",outfile,"\n")
  }

  # record current max characters per line and then expand in case of long lines
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)

  # simple function to clean up many repeated commands
  # writes the content of an R object, followed by the object name with "#_" in front
  wl <- function(name){
    value = mylist[names(mylist)==name]
    writeLines(paste0(value," #_",name),con=zz)
  }

  writeLines("#C starter file written by R function SS_writestarter")
  writeLines("#C rerun model to get more complete formatting in starter.ss_new")
  writeLines(paste("#C should work with SS version:",mylist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  # strings for control and data file names
  wl("datfile")
  wl("ctlfile")

  # lots of single numerical values
  wl("init_values_src")
  wl("run_display_detail")
  wl("detailed_age_structure")
  wl("checkup")
  wl("parmtrace")
  wl("cumreport")
  wl("prior_like")
  wl("soft_bounds")
  wl("N_bootstraps")
  wl("last_estimation_phase")
  wl("MCMCburn")
  wl("MCMCthin")
  wl("jitter_fraction")
  wl("minyr_sdreport")
  wl("maxyr_sdreport")
  wl("N_STD_yrs")
  if(mylist$N_STD_yrs>0){
    wl("STD_yr_vec")
  }
  wl("converge_criterion")
  wl("retro_yr")
  wl("min_age_summary_bio")
  wl("depl_basis")
  wl("depl_denom_frac")
  wl("SPR_basis")
  wl("F_report_units")
  if(mylist$F_report_units==4){
    cat(mylist[["F_age_range"]],"#_F_age_range\n")
  }
  wl("F_report_basis")
  writeLines("#")
  wl("final")

  # restore printing width to whatever the user had before
  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}

#' write data file
#'
#' write Stock Synthesis data file from list object in R which was probably
#' created using \code{\link{rd_data}}
#'
#'
#' @param datlist List object created by \code{\link{rd_data}}.
#' @param outfile Filename for where to write new data file.
#' @param overwrite Should existing files be overwritten? Default=TRUE.
#' @param verbose Should there be verbose output while running the file?
#' @author Ian Taylor
#' @seealso \code{\link{rd_starter}},
#' \code{\link{rd_forecast}}, \code{\link{rd_ctl}},
#' \code{\link{wrt_starter}}, \code{\link{wrt_forecast}},
#' \code{\link{wrt_data}}, \code{\link{wrt_ctl}}
#' @keywords data export
wrt_data <- function(datlist,outfile,overwrite=TRUE,verbose=FALSE){
  # function to write Stock Synthesis data files

  if(verbose) cat("running wrt_data\n")

  if(datlist$type!="Stock_Synthesis_data_file"){
    stop("input 'datlist' should be a list with $type=='Stock_Synthesis_data_file'")
  }

  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(file.exists(outfile)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",outfile,"\n")
      return()
    }else{
      file.remove(outfile)
    }
  }
  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=FALSE, strip.white=TRUE)
  }
  oldwidth <- options()$width
  oldmax.print <- options()$max.print
  options(width=5000,max.print=9999999)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name,comment=NULL){
    # simple function to clean up many repeated commands
    value = datlist[names(datlist)==name]
    if(is.null(comment)){
      writeLines(paste(value," #_",name,sep=""))
    }else{
      writeLines(paste(value,comment))
    }
  }

  # write a header
  writeLines("#C data file created using the wrt_data function")
  writeLines(paste("#C should work with SS version:",datlist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  # write the contents
  wl("styr")
  wl("endyr")
  wl("nseas")
  writeLines(paste(paste(datlist$months_per_seas,collapse=" "),"#_months_per_seas"))
  wl("spawn_seas")
  wl("Nfleet")
  wl("Nsurveys")
  wl("N_areas")
  writeLines(paste(paste(datlist$fleetnames,collapse="%"),"#_fleetnames"))
  writeLines(paste(paste(datlist$surveytiming,collapse=" "),"#_surveytiming_in_season"))
  writeLines(paste(paste(datlist$areas,collapse=" "),"#_area_assignments_for_each_fishery_and_survey"))
  writeLines(paste(paste(datlist$units_of_catch,collapse=" "),"#_units of catch:  1=bio; 2=num"))
  writeLines(paste(paste(datlist$se_log_catch,collapse=" "),"#_se of log(catch) only used for init_eq_catch and for Fmethod 2 and 3"))
  wl("Ngenders")
  wl("Nages")
  writeLines(paste(paste(datlist$init_equil,collapse=" "),"#_init_equil_catch_for_each_fishery"))
  wl("N_catch",comment="#_N_lines_of_catch_to_read")
  if(!is.null(datlist$catch)) printdf(datlist$catch)
  wl("N_cpue")
  if(datlist$N_cpue>0){
    printdf(datlist$CPUEinfo)
    printdf(datlist$CPUE)
  }
  # wl("discard_units")
  wl("N_discard_fleets")
  writeLines("#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)")
  writeLines("#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal")
  if(!is.null(datlist$discard_fleet_info)) printdf(datlist$discard_fleet_info)
  wl("N_discard")
  if(!is.null(datlist$discard_data)) printdf(datlist$discard_data)
  wl("N_meanbodywt")
  wl("DF_for_meanbodywt", comment="#_DF_for_meanbodywt_T-distribution_like")
  if(!is.null(datlist$meanbodywt)) printdf(datlist$meanbodywt)

  # length data
  wl("lbin_method",comment="# length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector")
  if(datlist$lbin_method==2){
    wl("binwidth",comment="# binwidth for population size comp")
    wl("minimum_size",comment="# minimum size in the population (lower edge of first bin and size at age 0.00)")
    wl("maximum_size",comment="# maximum size in the population (lower edge of last bin)")
  }
  if(datlist$lbin_method==3){
    wl("N_lbinspop")
    writeLines("#_lbin_vector_pop")
    writeLines(paste(datlist$lbin_vector_pop,collapse=" "))
  }
  wl("comp_tail_compression")
  wl("add_to_comp")
  wl("max_combined_lbin",comment="#_combine males into females at or below this bin number")
  wl("N_lbins")
  writeLines("#_lbin_vector")
  writeLines(paste(datlist$lbin_vector,collapse=" "))
  wl("N_lencomp",comment="#_N_Length_comp_observations")
  if(!is.null(datlist$lencomp)) printdf(datlist$lencomp)
  wl("N_agebins")
  writeLines("#_agebin_vector")
  writeLines(paste(datlist$agebin_vector,collapse=" "))
  wl("N_ageerror_definitions")
  if(!is.null(datlist$ageerror)) printdf(datlist$ageerror)
  wl("N_agecomp")
  wl("Lbin_method", comment="#_Lbin_method: 1=poplenbins; 2=datalenbins; 3=lengths")
  wl("max_combined_age", comment="#_combine males into females at or below this bin number")
  if(!is.null(datlist$agecomp)) printdf(datlist$agecomp)
  wl("N_MeanSize_at_Age_obs")
  #    datlist$MeanSize_at_Age_obs2 <- matrix(datlist$N_MeanSize_at_Age_obs)
  if(!is.null(datlist$MeanSize_at_Age)) printdf(datlist$MeanSize_at_Age_obs)
  wl("N_environ_variables")
  wl("N_environ_obs")
  if(!is.null(datlist$envdat)) printdf(datlist$envdat)
  wl("N_sizefreq_methods")
  wl("do_tags")
  if(datlist$do_tags != 0){
    wl("N_tag_groups")
    wl("N_recap_events")
    wl("mixing_latency_period")
    wl("max_periods")
    if(!is.null(datlist$tag_releases)) printdf(datlist$tag_releases)
    if(!is.null(datlist$tag_recaps)) printdf(datlist$tag_recaps)
  }
  wl("morphcomp_data")
  writeLines("#")
  writeLines("999")
  options(width=oldwidth,max.print=oldmax.print)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}

#' write control file
#'
#' write Stock Synthesis control file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param data_input List object created by \code{\link{rd_data}}.
#' @param control_input List object created by \code{\link{rd_ctl}}.
#' @param overwrite Should existing files be overwritten? Default=TRUE.
#' @author Nathan Vaughan
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_forecast}},
#' \code{\link{rd_data}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data export
wrt_ctl<-function(file, data_input, control_input, overwrite=TRUE){
  ControlFile<-control_input

  on.exit({if(sink.number()>0) sink()})
  if(file.exists(file)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",file,"\n")
      return()
    }else{
      file.remove(file)
    }
  }

  file.create(file)
  output<-file(file)


  sink(output,append = FALSE)


  writeLines("#C control file created using the writeControlFile function")
  writeLines(paste("#C should work with SS version:",3.24))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines(paste("#C source file : ",ControlFile$sourcefile,sep = ""))
  writeLines(paste("#C file type : ",ControlFile$type,sep = ""))
  writeLines("#")
  writeLines(" ")
  writeLines(" ")
  writeLines(" ")

  if(!is.null(ControlFile$Num_GP))
  {
    writeLines(paste(ControlFile$Num_GP," #Number of growth patterns (single value)"))
  }else{print("Error: Should always have a number of growth patterns value") ;  sink(); return(NULL)}

  if(!is.null(ControlFile$Num_SubMorphs))
  {
    writeLines(paste(ControlFile$Num_SubMorphs," #Number of growth pattern sub morphs (single value)"))

    if(ControlFile$Num_SubMorphs>1)
    {
      if(!is.null(ControlFile$SubMorphs_SD))
      {
        writeLines(paste(ControlFile$SubMorphs_SD," #Growth pattern sub morph standard deviation (single value"))
      }else{print("Error: Sub-morph SD should be a single value") ;  sink(); return(NULL)}


      if(!is.null(ControlFile$SubMorphs_Dist))
      {
        outText<-paste(ControlFile$SubMorphs_Dist,collapse = " ")
        writeLines(paste(outText," #Distribution among sub morphs (-1 or vector of length Nsubmorphs)"))
      }else{print("Error: Distribution among submorphs must be -1 or a vector whose length is equal to the number of sub morphs") ;  sink(); return(NULL)}
    }
  }else{print("Error: The number of sub-morphs should be a single value") ;  sink(); return(NULL)}

  if((ControlFile$Num_GP*data_input$nseas*data_input$N_areas)>1)
  {
    if(!is.null(ControlFile$Num_RecruitAssign))
    {
      writeLines(paste(ControlFile$Num_RecruitAssign," #Number of recruitment assignments which GP, area, seasons have recruitment (single integer value)"))
    }else{print("Error: The number of recruitment assignments should be a single value") ;  sink(); return(NULL)}

    if(!is.null(ControlFile$Recuit_Interact))
    {
      writeLines(paste(ControlFile$Recuit_Interact," #Should recruitment interaction effect parameters be included (0 or 1 value)"))
    }else{print("Error: Recruitment interactions should be a single value equal to 0 or 1") ;  sink(); return(NULL)}

    for(i in 1:ControlFile$Num_RecruitAssign)
    {
      if(!is.null(ControlFile$RecruitAssign[[i]]))
      {
        outText<-paste(ControlFile$RecruitAssign[[i]],collapse = " ")
        writeLines(paste(outText," #Recruitment assignment to GP,Seas,Area (3 value vector)"))
      }else{print("Error: Recruitment assignments should contain 3 values representing the growth pattern, season, and area") ;  sink(); return(NULL)}
    }
    if(data_input$N_areas>1)
    {
      if(!is.null(ControlFile$Num_MoveDefs))
      {
        writeLines(paste(ControlFile$Num_MoveDefs," #Number of movement definitions between areas (single value)"))
      }else{print("Error: Number of movement definitions should be a single value") ;  sink(); return(NULL)}

      if(as.numeric(ControlFile$Num_MoveDefs)>0)
      {
        if(!is.null(ControlFile$First_age))
        {
          writeLines(paste(ControlFile$First_age," #First age to move (single value)"))
        }else{print("Error: First age that moves should be a single value") ;  sink(); return(NULL)}

        for(i in 1:ControlFile$Num_MoveDefs)
        {
          if(!is.null(ControlFile$MoveDefs[[i]]))
          {
            outText<-paste(ControlFile$MoveDefs[[i]],collapse = " ")
            writeLines(paste(outText," #Movement definitions represent Seas,GP,Source area,Destination area,Min age, Max age (6 value vector)"))
          }else{print("Error: Movement definitions should contain 6 values representing the season, growth pattern, source area, destination area, min age, and max age") ;  sink(); return(NULL)}
        }
      }
    }
  }

  if(!is.null(ControlFile$Num_BlockPatterns))
  {
    writeLines(paste(ControlFile$Num_BlockPatterns," #Number of time varying parameter block patterns (single value)"))
  }else{print("Error: The number of block patterns should be a single value") ;  sink(); return(NULL)}

  if(ControlFile$Num_BlockPatterns>0)
  {
    if(!is.null(ControlFile$BlocksPerPattern))
    {
      outText<-paste(ControlFile$BlocksPerPattern,collapse = " ")
      writeLines(paste(outText," #Number of blocks per pattern (vector of length Nblockpatterns)"))
    }else{print("Error: There should be a value of number of blocks per pattern for each pattern determined by number of block patterns") ;  sink(); return(NULL)}

    for(i in 1:ControlFile$Num_BlockPatterns)
    {
      if(!is.null(ControlFile$BlockPatterns[[i]]))
      {
        outText<-paste(ControlFile$BlockPatterns[[i]],collapse = " ")
        writeLines(paste(outText," #Block pattern vector with sequential start and end year for each block (vector of length 2*BlocksPerPattern[i])"))
      }else{print("Error: Block patterns must be described by a start and end year for each block") ;  sink(); return(NULL)}
    }
  }

  #Read in the Control values for the biological parameters

  if(!is.null(ControlFile$frac_Female))
  {
    writeLines(paste(ControlFile$frac_Female," #Proportion of individuals female (single value)"))
  }else{print("Error: The fraction female should be a single value") ;  sink(); return(NULL)}

  NumS<-data_input$Ngenders

  if(!is.null(ControlFile$M_option))
  {
    writeLines(paste(ControlFile$M_option," #Natural maturity model choice (0)=1 parameter, (1)=N breakpoints, (2)=Lorenzen Function, (3)=Age specific M no seas interp, (4)=Age specific M seasonal interpolation (single value)"))
  }else{print("Error: The natural matury option should be a single value integer 0,1,2,3, or 4") ;  sink(); return(NULL)}

  if(ControlFile$M_option==0)
  {
    NumM<-1
  }else if(ControlFile$M_option==1)
  {
    if(!is.null(ControlFile$M_numBreaks))
    {
      writeLines(paste(ControlFile$M_numBreaks," #Number of natural mortality break points (single value)"))
    }else{print("Error: Natural mortality breakpoints should be a single value");  sink(); return(NULL)}

    if(!is.null(ControlFile$M_ageBreaks))
    {
      outText<-paste(ControlFile$M_ageBreaks,collapse = " ")
      writeLines(paste(outText," #Vector of natural mortality age breakpoints (vector of length Nbreaks)"))
    }else{print("Error: Length of age breaks vector not equal to number of breaks");  sink(); return(NULL)}
    NumM<-ControlFile$M_numBreaks
  }else if(ControlFile$M_option==2)
  {
    if(!is.null(ControlFile$M_refAge))
    {
      writeLines(paste(ControlFile$M_refAge," #Reference age (integer) for the Lorenzen function (single value)"))
    }else{print("Error: Lorenzen function reference age should be a single value");  sink(); return(NULL)}
    NumM<-1
  }else if(ControlFile$M_option==3 || ControlFile$M_option==4)
  {
    if(NumS==1)
    {
      for(i in 1:ControlFile$Num_GP)
      {
        if(!is.null(ControlFile$AgeSpecificM[[i]]))
        {
          outText<-paste(ControlFile$AgeSpecificM[[i]],collapse = " ")
          writeLines(paste(outText," #Vector of age specific natural mortality values (vector of length Nbreaks)"))
        }else
        {
          print("Error: Age specific natural mortality must be represented by a vector of length equal to the number of ages plus 1")
          sink()
          return(NULL)
        }
      }
    }else{
      for(i in 1:(ControlFile$Num_GP*2))
      {
        if(!is.null(ControlFile$AgeSpecificM[[i]]))
        {
          outText<-paste(ControlFile$AgeSpecificM[[i]],collapse = " ")
          writeLines(paste(outText," #Vector of age specific natural mortality values (vector of length Nbreaks)"))
        }else
        {
          print("Error: Age specific natural mortality must be represented by a vector of length equal to the number of ages plus 1")
          sink()
          return(NULL)
        }
      }
    }
    NumM<-0
  }

  if(!is.null(ControlFile$Growth_Model))
  {
    writeLines(paste(ControlFile$Growth_Model," #Growth model used (1)=von Bertalanffy, (2)=Richards Curve, (3)=von Bertalanffy with age specific K deviations (single value)"))
  }else{print("Error: Growth Model should be 1, 2, or 3");  sink(); return(NULL)}

  if(!is.null(ControlFile$Growth_Amin))
  {
    writeLines(paste(ControlFile$Growth_Amin," #Reference age for first size at age parameter (single value)"))
  }else{print("Error: Reference Age 1 should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$Growth_Amax))
  {
    writeLines(paste(ControlFile$Growth_Amax," #Reference age for second size at age parameter (single value)"))
  }else{print("Error: Reference Age 2 should be a single value");  sink(); return(NULL)}

  if(ControlFile$Growth_Model==1)
  {
    NumG=0
  }else if(ControlFile$Growth_Model==2)
  {
    NumG=1
  }else if(ControlFile$Growth_Model==3)
  {
    if(!is.null(ControlFile$NumKmult))
    {
      writeLines(paste(ControlFile$NumKmult," #Number of age specific K deviations (single value)"))

      if(!is.null(ControlFile$KMultAges))
      {
        NumG<-ControlFile$NumKmult
        outText<-paste(ControlFile$KMultAges,collapse = " ")
        writeLines(paste(outText," #Vector of age specific K deviations (vector of length NumKmult)"))
      }else{print("Error: Vector of age specific K deviations should be a vector of length NumKmult");  sink(); return(NULL)}
    }else{print("Error: Number of age specific K deviations should be a single value");  sink(); return(NULL)}
  }

  if(!is.null(ControlFile$SDadd_LAA))
  {
    writeLines(paste(ControlFile$SDadd_LAA," #Standard deviation of additions to length at age (single value)"))
  }else{print("Error: SD added to length at age should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$CV_pattern))
  {
    writeLines(paste(ControlFile$CV_pattern," #CV pattern (0)CV=f(LAA), (1)CV=f(A), (2)SD=f(LAA), (3)SD=f(A), (4)Lognormal distribution of size at age (single value)"))
  }else{print("Error: CV Pattern should be 0, 1, 2, 3, or 4");  sink(); return(NULL)}

  if(!is.null(ControlFile$MaturityOpt))
  {
    writeLines(paste(ControlFile$MaturityOpt," #Maturity option (1)=length logistic, (2)=age logistic, (3)=read age mature for female GP, (4)=read age fecundity for female GP, (5)=read empirical wtatage.ss file, (6)=empirical length at maturity (single value)"))
  }else{print("Error: Maturity option should be 1, 2, 3, 4, 5, or 6");  sink(); return(NULL)}

  if(ControlFile$MaturityOpt==3 || ControlFile$MaturityOpt==4)
  {
    for(i in 1:ControlFile$Num_GP)
    {
      if(!is.null(ControlFile$MaturityVect[[i]]))
      {
        outText<-paste(ControlFile$MaturityVect[[i]],collapse = " ")
        writeLines(paste(outText," #Age specific Maturity/Fecundity vector for options 3 or 4 above (vector Nages+1)"))
      }else{print("Error: Maturity vector should be of length equal to number of ages plus 1");  sink(); return(NULL)}
    }
  }

  if(ControlFile$MaturityOpt==6)
  {
    for(i in 1:ControlFile$Num_GP)
    {
      if(!is.null(ControlFile$MaturityVect[[i]]))
      {
        outText<-paste(ControlFile$MaturityVect[[i]],collapse = " ")
        writeLines(paste(outText," #Vector of length specific maturity for option 6 above (vector of length Num length bins)"))
      }else{print("Error: Maturity vector should be of length equal to number of lengths");  sink(); return(NULL)}
    }
  }

  if(!is.null(ControlFile$FirstMatAge))
  {
    writeLines(paste(ControlFile$FirstMatAge," #First mature age (overridden if option 3,4,or 5 but still required) (single value)"))
  }else{print("Error: First mature age should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$FecundityOpt))
  {
    writeLines(paste(ControlFile$FecundityOpt," #Fecundity options (1)fec=wt*(a+b*wt), (2)fec=a*L^b, (3)fec=a*W^b, (4)fec=a+b*L, (5)E=a+b*W (single value)"))
  }else{print("Error: Fecundity option should be 1, 2, 3, 4, or 5");  sink(); return(NULL)}

  if(!is.null(ControlFile$HermaphOpt))
  {
    writeLines(paste(ControlFile$HermaphOpt," #Hermaphroditism option (0)=No, (1)=female to male transition (single value)"))
  }else{print("Error: Hermaphroditism option should be 0 or 1");  sink(); return(NULL)}

  if(ControlFile$HermaphOpt==1)
  {
    if(!is.null(ControlFile$HermaphSeas))
    {
      writeLines(paste(ControlFile$HermaphSeas," #When female to male transition occurs; if -1 at end of each season; or positive integer to select single season (single value)"))
    }else{print("Error: Hermaphroditism season should be -1 or positive integer");  sink(); return(NULL)}

    if(!is.null(ControlFile$MalesSpawn))
    {
      writeLines(paste(ControlFile$MalesSpawn," #should males be included in spawning biomass (0)No, (1)Yes (single value)"))
    }else{print("Error: Include males in SSB should be 0 or 1");  sink(); return(NULL)}
    NumH<-3
  }else
  {
    NumH<-0
  }

  if(!is.null(ControlFile$OffsetMethod))
  {
    writeLines(paste(ControlFile$OffsetMethod," #Offset method (1)direct, (2)base gender GP specific, (3)base gender+GP (single value)"))
  }else{print("Error: Offset method should be 1, 2, or 3");  sink(); return(NULL)}

  if(!is.null(ControlFile$TVAconstraint))
  {
    writeLines(paste(ControlFile$TVAconstraint," #Time varying parameter adjustment constraint (1)None, (2)Logistic transform  (single value)"))
  }else{print("Error: Time-varying adjustment constraint method should be 1 or 2");  sink(); return(NULL)}

  #Write the natural mortality, growth, and other biological parameters
  if(length(ControlFile$MG_Params[,1])>0)
  {
    writeLines(paste("# Mortality and growth parameter lines"))
    outText<-paste(colnames(ControlFile$MG_Params),collapse = " ")
    writeLines(paste("# ",outText))

    for(i in 1:length(ControlFile$MG_Params[,1]))
    {
      if(!is.null(ControlFile$MG_Params[i,]))
      {
        outText<-paste(ControlFile$MG_Params[i,],collapse = " ")
        writeLines(paste(outText," # ",rownames(ControlFile$MG_Params)[i]))
      }else{print("Error: A full parameter line needs 14 values");  sink(); return(NULL)}
    }

    if(length(ControlFile$MG_Params_TV$ENV_Params[,1])>0)
    {
      writeLines(paste("# Mortality and growth parameter time varying environmental effect short parameter lines"))
      outText<-paste(colnames(ControlFile$MG_Params_TV$ENV_Params)[1:7],collapse = " ")
      writeLines(paste("# ",outText))

      for(i in 1:length(ControlFile$MG_Params_TV$ENV_Params[,1]))
      {
        if(!is.null(ControlFile$MG_Params_TV$ENV_Params[i,]))
        {
          outText<-paste(ControlFile$MG_Params_TV$ENV_Params[i,],collapse = " ")
          writeLines(paste(outText," # ",rownames(ControlFile$MG_Params_TV$ENV_Params)[i]," time varying environmental effect ",i))
        }else{print("Error: A short parameter line needs 7 values");  sink(); return(NULL)}
      }
    }

    if(length(ControlFile$MG_Params_TV$BLK_Params[,1])>0)
    {
      writeLines(paste("# Mortality and growth parameter time varying block effect short parameter lines"))
      outText<-paste(colnames(ControlFile$MG_Params_TV$BLK_Params)[1:7],collapse = " ")
      writeLines(paste("# ",outText))

      for(i in 1:length(ControlFile$MG_Params_TV$BLK_Params[,1]))
      {
        if(!is.null(ControlFile$MG_Params_TV$BLK_Params[i,]))
        {
          outText<-paste(ControlFile$MG_Params_TV$BLK_Params[i,],collapse = " ")
          writeLines(paste(outText," # ",rownames(ControlFile$MG_Params_TV$BLK_Params)[i]))
        }else{print("Error: A short parameter line needs 7 values");  sink(); return(NULL)}
      }
    }
  }

  if(!is.null(ControlFile$MG_Params_TV$Seasonality))
  {
    outText<-paste(ControlFile$MG_Params_TV$Seasonality,collapse = " ")
    writeLines(paste(outText," # Growth and mortality parameter seasonal effects"))
  }else{print("Error: Seasonality fo parameters should be defined by a vector of 10 values");  sink(); return(NULL)}

  if(length(ControlFile$MG_Params_TV$Seasonality[ControlFile$MG_Params_TV$Seasonality!=0])>0)
  {
    for(i in 1:length(data_input$nseas*ControlFile$MG_Params_TV$Seasonality[ControlFile$MG_Params_TV$Seasonality!=0]))
    {
      if(!is.null(ControlFile$MG_Params_TV$Seas_Params[i,]))
      {
        outText<-paste(ControlFile$MG_Params_TV$Seas_Params[i,],collapse = " ")
        writeLines(paste(outText," # Seasonal effects short parameter lines"))
      }else{print("Error: There should be 7 values in a short parameter line, I think this should have been a seasonal parameter???");  sink(); return(NULL)}
    }
  }

  if(!is.null(ControlFile$MG_Params_TV$Dev_Phase))
  {
    writeLines(paste(ControlFile$MG_Params_TV$Dev_Phase," # Parameter deviation start phase (single value)"))
  }
  #Begin reading the spawner recruitment section
  writeLines(paste("# Begin Spawner recruitment section"))

  if(!is.null(ControlFile$SR))
  {
    writeLines(paste(ControlFile$SR," # Choice of stock recruitment relationship (single integer between 1 and 7)"))
  }else{print("Error: The choice of stock recruitment relationship should be a single integer value between 1 and 7");  sink(); return(NULL)}

  #Read stock recruit short parameter lines

  if(!is.null(ControlFile$SR_Params))
  {
    outText<-paste(colnames(ControlFile$SR_Params),collapse = " ")
    writeLines(paste(" # ",outText))
    for(i in 1:length(ControlFile$SR_Params[,1]))
    {
      if(!is.null(ControlFile$SR_Params[i,]))
      {
        outText<-paste(ControlFile$SR_Params[i,],collapse = " ")
        writeLines(paste(outText," # ",rownames(ControlFile$SR_Params)[i]))
      }else{print("Error: There should be 7 values in a short parameter line, I think this should have been a seasonal parameter???");  sink(); return(NULL)}
    }
  }

  #Read additional stock recruitment conditions

  if(!is.null(ControlFile$SR_EnvLnk))
  {
    writeLines(paste(ControlFile$SR_EnvLnk," # Stock recruitment environmental link (single value)"))
  }else{print("Error: Stock recruitment environmental link variable should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$SR_EnvTrg))
  {
    writeLines(paste(ControlFile$SR_EnvTrg," # Stock recruitment environmental target (single value)"))
  }else{print("Error: Stock recruitment environmental target variable should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$SR_DoDev))
  {
    writeLines(paste(ControlFile$SR_DoDev," # Stock recruitment implement recruitment deviations (single value)"))
  }else{print("Error: Stock recruitment deviations should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$SR_BeginYr))
  {
    writeLines(paste(ControlFile$SR_BeginYr," # Stock recruitment deviations begining year (single value)"))
  }else{print("Error: Stock recruitment deviations begining year should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$SR_EndYr))
  {
    writeLines(paste(ControlFile$SR_EndYr," # Stock recruitment deviations end year (single value)"))
  }else{print("Error: Stock recruitment deviations end year should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$SR_Phase))
  {
    writeLines(paste(ControlFile$SR_Phase," # Stock recruitment deviations phase (single value)"))
  }else{print("Error: Stock recruitment deviations phase should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$SR_AdvOpt))
  {
    writeLines(paste(ControlFile$SR_AdvOpt," # Stock recruitment advanced options switch (single value 0 or 1)"))
  }else{print("Error: Stock recruitment advanced options should be a single value");  sink(); return(NULL)}

  #Begin reading advanced stock recruitement options
  if(ControlFile$SR_AdvOpt==1)
  {
    for(i in 1:13)
    {
      if(!is.null(ControlFile$AdvOpts[[i]]))
      {
        writeLines(paste(ControlFile$AdvOpts[[i]]," # Advanced stock recruitment options value (single value)"))
      }else{print("Error: Advanced options should be a single value");  sink(); return(NULL)}
    }


    if(ControlFile$AdvOpts[[10]]!=0)
    {
      outText<-paste(colnames(ControlFile$RecCycl),collapse = " ")
      writeLines(paste(" # ",outText))
      for(i in 1:ControlFile$AdvOpts[[10]])
      {
        if(!is.null(ControlFile$RecCycl[i,]))
        {
          outText<-paste(ControlFile$RecCycl[i,],collapse = " ")
          writeLines(paste(outText," # Recruitment cycle parameter line ",i))
        }else{print("Error: Recruitment cycle should be a full parameter line");  sink(); return(NULL)}
      }
    }

    if(ControlFile$AdvOpts[[13]]!=0)
    {
      for(i in 1:ControlFile$AdvOpts[[13]])
      {
        if(!is.null(ControlFile$RecDevs[[i]]))
        {
          outText<-paste(ControlFile$RecDevs[[i]],collapse = " ")
          writeLines(paste(outText," # Recruitment deviations (two values year then deviation)"))
        }else{print("Error: Recruitment deviation should be two values a year and a deviation");  sink(); return(NULL)}
      }
    }
  }
  #Now move on to writing Fishing mortality method parameters and controls

  if(!is.null(ControlFile$F_Ball))
  {
    writeLines(paste(ControlFile$F_Ball," # A ballpark estimate of F (single value)"))
  }else{print("Error: F ballpark estimate should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$F_BallYr))
  {
    writeLines(paste(ControlFile$F_BallYr," # Reference year for ballpark estimate of F (single value)"))
  }else{print("Error: F ballpark estimate reference year should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$F_Method))
  {
    writeLines(paste(ControlFile$F_Method," # Which F method to use (1)Pope's, (2)Continuous F, (3)Hybrid (single value interger 1, 2, or 3)"))
  }else{print("Error: F method should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$F_Max))
  {
    writeLines(paste(ControlFile$F_Max," # Maximum F in each season/area (single value)"))
  }else{print("Error: F maximum should be a single value");  sink(); return(NULL)}

  if(ControlFile$F_Method==2)
  {
    if(!is.null(ControlFile$F_Popes))
    {
      outText<-paste(ControlFile$F_Popes,collapse = " ")
      writeLines(paste(outText," # Popes method parameters starting F, active phase, number of detailed F's (3 values)"))
    }else{print("Error: Popes F method params should be 3 values");  sink(); return(NULL)}

    if(!is.null(ControlFile$F_Detailed))
    {
      if(length(ControlFile$F_Detailed[,1])>0)
      {
        writeLines(paste("# Detailed F values for Pope's approximation"))
        outText<-paste(colnames(ControlFile$F_Detailed),collapse = " ")
        writeLines(paste("# ",outText))

        for(i in 1:length(ControlFile$F_Detailed[,1]))
        {
          outText<-paste(ControlFile$F_Detailed[i,],collapse = " ")
          writeLines(paste(outText," # Detailed F input parameter (6 values)"))
        }
      }
    }
  }

  if(ControlFile$F_Method==3)
  {
    if(!is.null(ControlFile$F_TunIts))
    {
      writeLines(paste(ControlFile$F_TunIts," # Number of tuning iterations for hybrid method (single value)"))
    }else{print("Error: Number of tuning iterations should be a single value");  sink(); return(NULL)}
  }

  #Read in Initial Fishing mortality parameter lines
  writeLines(paste(" # Initial F parameters"))
  outText<-paste(colnames(ControlFile$F_init),collapse = " ")
  writeLines(paste(" # ",outText))
  for(i in 1:data_input$Nfleet)
  {
    if(!is.null(ControlFile$F_init[i,]))
    {
      outText<-paste(ControlFile$F_init[i,],collapse = " ")
      writeLines(paste(outText," # Initial F params for fleet ",data_input$fleetnames[i]," (short parameter line 7 values)"))
    }else{print("Error: There should be 7 values in a short parameter line, I think this should have been initial F???");  sink(); return(NULL)}
  }

  #Now read in the Catchability parameters for all fleets and surveys
  writeLines(paste(" # Catchability setup"))
  outText<-paste(colnames(ControlFile$Q),collapse = " ")
  writeLines(paste(" # ",outText))
  for(i in 1:(data_input$Nfleet+data_input$Nsurveys))
  {
    outText<-paste(ControlFile$Q[i,],collapse = " ")
    writeLines(paste(outText," # Catchability parameters for fleet/survey ",data_input$fleetnames[i]," (4 setup values)"))
  }

  if(!is.null(ControlFile$Q_Cust))
  {
    writeLines(paste(ControlFile$Q_Cust," # Should custom random effects be used (0)just one param, (1)individual param for each random effect (single value)"))
  }

  if(!is.null(ControlFile$Qrand))
  {
    #print(paste0("#writing Rand Q ",length(ControlFile$Qrand[,1])))
    if(length(ControlFile$Qrand[,1])>0)
    {
      writeLines(paste(" # Random effect catchability parameters"))
      outText<-paste(colnames(ControlFile$Qrand),collapse = " ")
      writeLines(paste(" # ",outText))
      for(i in 1:length(ControlFile$Qrand[,1]))
      {
        outText<-paste(ControlFile$Qrand[i,],collapse = " ")
        writeLines(paste(outText," # Random effect catchability short parameter line (7 values)"))
      }
    }
  }

  writeLines(paste(" # Size selectivity parameter setup"))
  outText<-paste(colnames(ControlFile$Size_Select),collapse = " ")
  writeLines(paste(" # ",outText))
  #print(paste0("#writing size select num fleet +survey = ",(data_input$Nfleet+data_input$Nsurveys),"length list = ",length(ControlFile$Size_Select[,1])))
  for(i in 1:(data_input$Nfleet+data_input$Nsurveys))
  {
    outText<-paste(ControlFile$Size_Select[i,],collapse = " ")
    writeLines(paste(outText," # Size selectivity setup line (4 values)"))
  }

  writeLines(paste(" # Age selectivity parameter setup"))
  outText<-paste(colnames(ControlFile$Age_Select),collapse = " ")
  writeLines(paste(" # ",outText))
  for(i in 1:(data_input$Nfleet+data_input$Nsurveys))
  {
    outText<-paste(ControlFile$Age_Select[i,],collapse = " ")
    writeLines(paste(outText," # Age selectivity setup line (4 values)"))
  }

  #Write the selectivity parameter table

  if(!is.null(ControlFile$Select_Params))
  {
    if(length(ControlFile$Select_Params[,1])>0)
    {
      writeLines(paste(" # Selectivity parameter lines"))
      outText<-paste(colnames(ControlFile$Select_Params),collapse = " ")
      writeLines(paste(" # ",outText))

      for(i in 1:length(ControlFile$Select_Params[,1]))
      {
        outText<-paste(ControlFile$Select_Params[i,],collapse = " ")
        writeLines(paste(outText," # ",rownames(ControlFile$Select_Params)[i],"Selectivity parameter line (14 values)"))
      }
    }
  }

  #Write time varying parameters for selectivity and retention

  if(length(ControlFile$Select_Params_TV$ENV_Params[,1])>0)
  {
    if(!is.null(ControlFile$Select_Params_TV$Cust_ENV))
    {
      writeLines(paste(ControlFile$Select_Params_TV$Cust_ENV,"# Custom Time varying environmentally linked selectivity (single value)"))
    }else{print("Error: The allocation of custom environmental linkages should be a single value");  sink(); return(NULL)}

    writeLines(paste(" # Time varying environmental effect parameters"))
    outText<-paste(colnames(ControlFile$Select_Params_TV$ENV_Params)[1:7],collapse = " ")
    writeLines(paste(" # ",outText))

    for(i in 1:length(ControlFile$Select_Params_TV$ENV_Params[,1]))
    {
      outText<-paste(ControlFile$Select_Params_TV$ENV_Params[i,],collapse = " ")
      writeLines(paste(outText," # ",rownames(ControlFile$Select_Params_TV$ENV_Params)[i],"Time varying parameter line (7 values)"))
    }
  }

  if(length(ControlFile$Select_Params_TV$BLK_Params[,1])>0)
  {
    if(!is.null(ControlFile$Select_Params_TV$Cust_Blk))
    {
      writeLines(paste(ControlFile$Select_Params_TV$Cust_Blk,"# Custom Time varying block linked selectivity (single value)"))
    }else{print("Error: The allocation of custom block linkages should be a single value");  sink(); return(NULL)}

    writeLines(paste(" # Time varying block effect parameters"))
    outText<-paste(colnames(ControlFile$Select_Params_TV$BLK_Params)[1:7],collapse = " ")
    writeLines(paste(" # ",outText))

    for(i in 1:length(ControlFile$Select_Params_TV$BLK_Params[,1]))
    {
      outText<-paste(ControlFile$Select_Params_TV$BLK_Params[i,],collapse = " ")
      writeLines(paste(outText," # ",rownames(ControlFile$Select_Params_TV$BLK_Params)[i],"Time varying parameter line (7 values)"))
    }
  }

  if(!is.null(ControlFile$Select_Params_TV$Dev_Phase))
  {
    writeLines(paste(ControlFile$Select_Params_TV$Dev_Phase,"# Phase to start time varying parameter deviations (single value)"))
  }

  if(!is.null(ControlFile$Select_Params_TV$Adj_Meth))
  {
    writeLines(paste(ControlFile$Select_Params_TV$Adj_Meth,"# adjustment method for time varying parameters (single value)"))
  }

  if(!is.null(ControlFile$Read_Tag))
  {
    writeLines(paste(ControlFile$Read_Tag,"# Should tag recapture parameters be read (0)No, (1)Yes (single value)"))
  }else{print("Error: Decision to read tag recapture parameters should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$TagRecap_Params))
  {
    if(length(ControlFile$TagRecap_Params[,1])>0)
    {
      writeLines(paste(" # Tag recapture parameters"))
      outText<-paste(colnames(ControlFile$TagRecap_Params),collapse = " ")
      writeLines(paste(" # ",outText))

      for(i in 1:length(ControlFile$TagRecap_Params[,1]))
      {
        outText<-paste(ControlFile$TagRecap_Params[i,],collapse = " ")
        writeLines(paste(outText," # Tag recapture parameter line (14 values)"))
      }
    }
  }

  if(!is.null(ControlFile$Var_adj))
  {
    writeLines(paste(ControlFile$Var_adj,"# Read variance adjustment factors (0)No, (1)Yes (single value)"))

    if(!is.null(ControlFile$Var_Adj_Fact))
    {
      for(i in 1:6)
      {
        outText<-paste(ControlFile$Var_Adj_Fact[i,],collapse = " ")
        writeLines(paste(outText," # Variance adjustment factors (Nfleet+Nsurveys values)"))
      }
    }
  }else{print("Error: Read variance adjustment factors should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$Max_Lambda))
  {
    writeLines(paste(ControlFile$Max_Lambda,"# Max lambda phase (single value)"))
  }else{print("Error: Max lambda phase should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$SD_offset))
  {
    writeLines(paste(ControlFile$SD_offset,"# sd_offset (0)exclude Log(s) term, (1)include Log(s) term (single value)"))
  }else{print("Error: sd offset should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$Num_changes))
  {
    writeLines(paste(ControlFile$Num_changes,"# number of changes to make to default Lambdas (single value default 1)"))

    if(!is.null(ControlFile$Lambda_Changes))
    {
      writeLines(paste(" # Lambda changes"))
      writeLines(paste(" # Component fleet/survey Phase Lambda SizefreqMethod"))

      for(i in 1:length(ControlFile$Lambda_Changes[,1]))
      {
        outText<-paste(ControlFile$Lambda_Changes[i,],collapse = " ")
        writeLines(paste(outText," # Lambda change info (5 values)"))
      }
    }
  }else{print("Error: number of changes to make to default lambdas should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$Extra))
  {
    writeLines(paste(ControlFile$Extra," # read specs for extra variance estimate (0)No, (1)Yes (1 values)"))
  }else{print("Error: Getting extra variance estimate should be a single value");  sink(); return(NULL)}

  if(!is.null(ControlFile$Extra_control))
  {
    writeLines(paste(" # Fleet len/age year NselexBins GrowthPattern NgrowthAges AreaNatAge YearNatAge NatAgesReported"))
    outText<-paste(ControlFile$Extra_control,collapse = " ")
    writeLines(paste(outText," # Read in control values for extra variance estimate (9 values)"))
  }

  if(!is.null(ControlFile$Extra_selex))
  {
    outText<-paste(ControlFile$Extra_selex,collapse = " ")
    writeLines(paste(outText," # Extra selection bins (NselexBins values)"))
  }

  if(!is.null(ControlFile$Extra_growth))
  {
    outText<-paste(ControlFile$Extra_growth,collapse = " ")
    writeLines(paste(outText," # Extra growth bins (NgrowthAges values)"))
  }

  if(!is.null(ControlFile$Extra_NatAge))
  {
    outText<-paste(ControlFile$Extra_NatAge,collapse = " ")
    writeLines(paste(outText," # Extra Numbers at Age bins (NagesReported values)"))
  }

  if(!is.null(ControlFile$End))
  {
    outText<-paste(ControlFile$End,collapse = " ")
    writeLines(paste(outText," # File must end with 999 to ensure correct read"))
    sink()
    return(NULL)
  }else{print("Error: Something went wrong, this should be the end of the file and read 999")}
}

#' write forecast file
#'
#' write Stock Synthesis forecast file into list object in R
#'
#'
#' @param mylist List object created by \code{\link{rd_forecast}}.
#' @param dir Directory for new forecast file. Default=NULL (working directory).
#' @param file Filename for new forecast file. Default="forecast.ss".
#' @param overwrite Should existing files be overwritten? Default=TRUE.
#' @param verbose Should there be verbose output while running the file?
#' Default=FALSE
#' @author Ian Taylor
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_forecast}},
#' \code{\link{rd_data}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data export
wrt_forecast <-  function(mylist, dir=NULL, file="forecast.ss",
                         overwrite=TRUE, verbose=FALSE){
  # function to write Stock Synthesis forecast files
  if(verbose) cat("running wrtforecast\n")

  if(mylist$type!="Stock_Synthesis_forecast_file"){
    stop("input 'mylist' should be a list with $type=='Stock_Synthesis_forecast_file'")
  }

  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})

  if(is.null(dir)) dir <- getwd() # set to working directory if no input provided
  outfile <- paste(dir,file,sep="/")
  if(file.exists(outfile)){
    if(!overwrite){
      stop(paste("file exists:",outfile,"\n  set overwrite=TRUE to replace\n"))
    }else{
      if(verbose) cat("overwriting file:",outfile,"\n")
      file.remove(outfile)
    }
  }else{
    if(verbose) cat("writing new file:",outfile,"\n")
  }

  # preliminary setup
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)
  wl <- function(name){
    # simple function to clean up many repeated commands
    value = mylist[names(mylist)==name]
    writeLines(paste(value," #_",name,sep=""),con=zz)
  }
  printdf <- function(dataframe){
    # function to print data frame with hash mark before first column name
    names(dataframe)[1] <- paste("#_",names(dataframe)[1],sep="")
    print(dataframe, row.names=FALSE, strip.white=TRUE)
  }

  writeLines("#C forecast file written by R function SS_writeforecast")
  writeLines("#C rerun model to get more complete formatting in forecast.ss_new")
  writeLines(paste("#C should work with SS version:",mylist$SSversion))
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#")

  wl("benchmarks")
  wl("MSY")
  wl("SPRtarget")
  wl("Btarget")
  writeLines("#_Bmark_years: beg_bio end_bio beg_selex end_selex beg_alloc end_alloc")
  writeLines(paste(paste(mylist$Bmark_years,collapse=" ")))
  wl("Bmark_relF_Basis")
  wl("Forecast")
  wl("Nforecastyrs")
  wl("F_scalar")
  writeLines("#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF")
  writeLines(paste(paste(mylist$Fcast_years,collapse=" ")))
  wl("ControlRuleMethod")
  wl("BforconstantF")
  wl("BfornoF")
  wl("Flimitfraction")
  wl("N_forecast_loops")

  wl("First_forecast_loop_with_stochastic_recruitment")
  wl("Forecast_loop_control_3")
  wl("Forecast_loop_control_4")
  wl("Forecast_loop_control_5")
  wl("FirstYear_for_caps_and_allocations")
  wl("stddev_of_log_catch_ratio")
  wl("Do_West_Coast_gfish_rebuilder_output")
  wl("Ydecl")
  wl("Yinit")
  wl("fleet_relative_F")
  wl("basis_for_fcast_catch_tuning")
  if(mylist$fleet_relative_F==2){
    printdf(mylist$rel_F)
  }
  writeLines("# max totalcatch by fleet (-1 to have no max)")
  writeLines(paste(paste(mylist$max_totalcatch_by_fleet,collapse=" ")))
  writeLines("# max totalcatch by area (-1 to have no max)")
  writeLines(paste(paste(mylist$max_totalcatch_by_area,collapse=" ")))
  writeLines("# fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)")
  writeLines(paste(paste(mylist$fleet_assignment_to_allocation_group,collapse=" ")))
  if(any(mylist$fleet_assignment_to_allocation_group!=0)){
    writeLines(paste("# allocation fraction for each of:",mylist$N_allocation_groups," allocation groups"))
    writeLines(paste(paste(mylist$allocation_among_groups,collapse=" ")))
  }
  wl("Ncatch")
  wl("InputBasis")
  if(mylist$Ncatch>0){
    printdf(mylist$ForeCatch)
  }
  writeLines("#")
  writeLines("999 # verify end of input ")

  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}

#' write parameter file
#'
#' write Stock Synthesis parameter file into list object in R
#'
#'
#' @param file Filename for new parameter file.
#' @param pars_input List object created by \code{\link{rd_par}}.
#' @param overwrite Should existing files be overwritten? Default=TRUE.
#' @author Nathan Vaughan
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_forecast}},
#' \code{\link{rd_data}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data export
wrt_par <- function(file,pars_input,overwrite=TRUE){
  cat("Running wrt_par")
  on.exit({if(sink.number()>0) sink()})
  if(file.exists(file)){
    if(!overwrite){
      cat("File exists and input 'overwrite'=FALSE:",outfile,"\n")
      return()
    }else{
      cat("overwriting file:",file,"\n")
      file.remove(file)
    }
  }else{
    cat("writing file:",file,"\n")
  }
  file.create(file)
  output<-file(file)
  sink(output,append = TRUE)
  writeLines(pars_input$Labels[[1]])
  for(i in 1:length(pars_input$Values))
  {
    outText<-paste(pars_input$Labels[[i+1]],collapse = " ")
    writeLines(outText)
    outText<-paste(pars_input$Values[[i]],collapse = " ")
    writeLines(outText)
  }
  sink()
  cat("wrote file:",file,"\n")
  close(output)

}

#' write display file
#'
#' write display descriptions file into list object in R
#'
#'
#' @param mylist List object created by \code{\link{rd_display}}.
#' @param file Filename and directory for new forecast file.
#' Default="Display_Descriptions.txt".
#' @param overwrite Should existing files be overwritten? Default=TRUE.
#' @param verbose Should there be verbose output while running the file?
#' Default=FALSE
#' @author Ian Taylor
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_forecast}},
#' \code{\link{rd_data}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data export
wrt_display <-  function(mylist, file=paste(getwd(),"Display_Descriptions.txt",sep="/"),
                         overwrite=TRUE, verbose=FALSE){
  # function to write Stock Synthesis forecast files
  if(verbose) cat("running wrt_Display \n")

  if(mylist$type!="DST_Display_Descriptions"){
    stop("input 'mylist' should be a list with $type=='DST_Display_Descriptions'")
  }

  # this command will hopefully prevent earlier issues of getting stuck with all R
  # output written to the file after the function crashes before closing connection
  ## on.exit({if(sink.number()>0) sink(); close(zz)})
  on.exit({if(sink.number()>0) sink()})


  outfile <- file
  if(file.exists(outfile)){
    if(!overwrite){
      stop(paste("file exists:",outfile,"\n  set overwrite=TRUE to replace\n"))
    }else{
      if(verbose) cat("overwriting file:",outfile,"\n")
      file.remove(outfile)
    }
  }else{
    if(verbose) cat("writing new file:",outfile,"\n")
  }

  # preliminary setup
  oldwidth <- options()$width
  options(width=1000)

  if(verbose) cat("opening connection to",outfile,"\n")
  zz <- file(outfile, open="at")
  sink(zz)

  writeLines("#C Display descriptions for DST interface written by R function wrt_Display")
  writeLines(paste("#C file write time:",Sys.time()))
  writeLines("#C Decision Support Tool: Descriptive values input file")
  writeLines("#C This file contains all the descriptive details required to display assessment results in a public facing application")

  writeLines("#")

  writeLines("#C Detailed description of the fishery and assessment that will be displayed in a popup window.")
  writeLines(paste0(mylist$AssesDesc))
  writeLines("#C Display name for the Assessment this is the title users will see when choosing an assessment.")
  writeLines(paste0(mylist$Title))
  writeLines("#C Allocation Group number, Display names for allocation groups, detailed description for group.")
  writeLines("#C One number, one name, and one description per row seperated by a , with a row for each allocation group including unallocated.")
  for(i in 1:length(mylist$GroupNames[,1]))
  {
    writeLines(paste0(mylist$GroupNames[i,1],",",mylist$GroupNames[i,2],",",mylist$GroupNames[i,3]))
  }
  writeLines("#C Fleet number, Fleet display names, and detailed description for fleet. These should be more interpretable than those in the assessment file.")
  writeLines("#C One number, one name, and one description per row seperated by a , with a row for each fleet not including surveys.")
  for(i in 1:length(mylist$FleetNames[,1]))
  {
    writeLines(paste0(mylist$FleetNames[i,1],",",mylist$FleetNames[i,2],",",mylist$FleetNames[i,3]))
  }
  writeLines("#C Season number, Display names for Season, detailed description of season.")
  writeLines("#C One number, one name, and one description per row seperated by a , with a row for each season.")
  for(i in 1:length(mylist$SeasonNames[,1]))
  {
    writeLines(paste0(mylist$SeasonNames[i,1],",",mylist$SeasonNames[i,2],",",mylist$SeasonNames[i,3]))
  }
  writeLines("#C Area number, Display names for area, detailed description of area.")
  writeLines("#C One number, one name, and one description per row seperated by a , with a row for each area.")
  for(i in 1:length(mylist$AreaNames[,1]))
  {
    writeLines(paste0(mylist$AreaNames[i,1],",",mylist$AreaNames[i,2],",",mylist$AreaNames[i,3]))
  }

  writeLines("#C Year range to achieve population/catch target (two rows first row start year and second row end year over which to average)")
  writeLines(paste0(mylist$TargetYears[1]))
  writeLines(paste0(mylist$TargetYears[2]))
  writeLines("#C Proposed fishing mortality as fraction of optimal target sustainable fishing mortality")
  writeLines(paste0(mylist$ABCFrac))
  writeLines("#C maintain relative fleet intensity based on harvest rate fraction (1) or catch fraction (2)")
  writeLines(paste0(mylist$FleetRel))
  writeLines("#C Implement rebuilding plan 1=yes 2=no")
  writeLines(paste0(mylist$ImplRebuild))
  writeLines("#C Years to allowed to achieve rebuilding ")
  writeLines(paste0(mylist$RebuildYears))
  writeLines("#C Fraction of MSY to achieve by rebuild target year")
  writeLines(paste0(mylist$RebuildFrac))
  writeLines("#C Constant Catch to apply ")
  writeLines(paste0(mylist$ConstCatch))
  writeLines("#C Weight units 1=MT and 2=1000's Lbs")
  writeLines(paste0(mylist$Units[1]))
  writeLines("#C Length units 1=cm and 2=inches")
  writeLines(paste0(mylist$Units[2]))

  options(width=oldwidth)
  sink()
  close(zz)
  if(verbose) cat("file written to",outfile,"\n")
}
