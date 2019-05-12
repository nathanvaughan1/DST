#' read starter file
#'
#' read Stock Synthesis starter file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param verbose Should there be verbose output while running the file?
#' @author Ian Taylor
#' @seealso \code{\link{rd_forecast}}, \code{\link{rd_data}},
#' \code{\link{rd_ctl}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data import
rd_starter <- function (file = "starter.ss", verbose = FALSE) {
  if (verbose) {
    message("running rd_starter")
  }
  size <- file.info(file)$size
  if (is.na(size) || size == 0)
    stop("file empty or missing:", file)
  starter <- readLines(file, warn = F)
  mylist <- list()
  mylist$sourcefile <- file
  mylist$type <- "Stock_Synthesis_starter_file"
  mylist$SSversion <- "3.24 or earlier"
  starter2 <- NULL
  for (i in 1:length(starter)) {
    mysplit <- strsplit(starter[i], split = "#")[[1]][1]
    if (!is.na(mysplit) && length(mysplit) > 0)
      starter2 <- c(starter2, mysplit)
  }
  strings <- NULL
  for (i in 1:length(starter2)) {
    mysplit <- strsplit(starter2[i], split = "[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit != ""]
    strings <- c(strings, mysplit)
  }
  strings <- strings[is.na(suppressWarnings(as.numeric(strings)))]
  if (length(strings) > 2) {
    warning("Too many strings in starter file?\n", "Choosing first 2 of these as data and control file names:\n",
            paste(strings, collapse = "\n"))
  }
  mylist$datfile <- strings[1]
  mylist$ctlfile <- strings[2]
  if (verbose) {
    message("  data, control files: ", mylist$datfile, ", ",
            mylist$ctlfile, sep = "")
  }
  allnums <- NULL
  for (i in 1:length(starter)) {
    mysplit <- strsplit(starter[i], split = "[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit != ""]
    nums <- suppressWarnings(as.numeric(mysplit))
    if (sum(is.na(nums)) > 0)
      maxcol <- min((1:length(nums))[is.na(nums)]) - 1
    else maxcol <- length(nums)
    if (maxcol > 0) {
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }
  i <- 1
  mylist$init_values_src <- allnums[i]
  i <- i + 1
  mylist$run_display_detail <- allnums[i]
  i <- i + 1
  mylist$detailed_age_structure <- allnums[i]
  i <- i + 1
  mylist$checkup <- allnums[i]
  i <- i + 1
  mylist$parmtrace <- allnums[i]
  i <- i + 1
  mylist$cumreport <- allnums[i]
  i <- i + 1
  mylist$prior_like <- allnums[i]
  i <- i + 1
  mylist$soft_bounds <- allnums[i]
  i <- i + 1
  mylist$N_bootstraps <- allnums[i]
  i <- i + 1
  mylist$last_estimation_phase <- allnums[i]
  i <- i + 1
  mylist$MCMCburn <- allnums[i]
  i <- i + 1
  mylist$MCMCthin <- allnums[i]
  i <- i + 1
  mylist$jitter_fraction <- allnums[i]
  i <- i + 1
  mylist$minyr_sdreport <- allnums[i]
  i <- i + 1
  mylist$maxyr_sdreport <- allnums[i]
  i <- i + 1
  mylist$N_STD_yrs <- N_STD_yrs <- allnums[i]
  i <- i + 1
  if (N_STD_yrs > 0) {
    mylist$STD_yr_vec <- allnums[i:(i + N_STD_yrs - 1)]
    i <- i + N_STD_yrs
  }
  mylist$converge_criterion <- allnums[i]
  i <- i + 1
  if (verbose) {
    message("  converge_criterion = ", mylist$converge_criterion)
  }
  mylist$retro_yr <- allnums[i]
  i <- i + 1
  mylist$min_age_summary_bio <- allnums[i]
  i <- i + 1
  mylist$depl_basis <- allnums[i]
  i <- i + 1
  mylist$depl_denom_frac <- allnums[i]
  i <- i + 1
  mylist$SPR_basis <- allnums[i]
  i <- i + 1
  if (verbose) {
    message("  SPR_basis = ", mylist$SPR_basis)
  }
  mylist$F_report_units <- allnums[i]
  i <- i + 1
  if (!is.na(mylist$F_report_units) && mylist$F_report_units ==
      4) {
    mylist$F_age_range <- allnums[i]
    i <- i + 1
    mylist$F_age_range[2] <- allnums[i]
    i <- i + 1
  }
  else {
    mylist$F_age_range <- NA
    mylist$F_age_range[2] <- NA
  }
  mylist$F_report_basis <- allnums[i]
  i <- i + 1
  if (verbose) {
    message("  F_report_basis = ", mylist$F_report_basis)
  }
  i.final <- length(allnums)
  if (i < i.final) {
    if (verbose) {
      message("Assuming version 3.30 based on number of numeric values.")
    }
    mylist$MCMC_output_detail <- allnums[i]
    i <- i + 1
    mylist$ALK_tolerance <- allnums[i]
    i <- i + 1
    if (verbose) {
      message("  MCMC_output_detail = ", mylist$MCMC_output_detail)
      message("  ALK_tolerance = ", mylist$ALK_tolerance)
    }
  }
  mylist$final <- final <- allnums[i]
  if (!is.na(final) && final %in% c(3.3, 999)) {
    if (verbose) {
      message("Read of starter file complete. Final value: ",
              final)
    }
  }
  else {
    warning("Final value is ", allnums[i], " but should be either 3.30 or 999")
  }
  if (final == 3.3) {
    mylist$SSversion <- "3.30"
  }
  return(mylist)
}

#' read data file
#'
#' read Stock Synthesis data file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param verbose Should there be verbose output while running the file?
#' Default=FALSE.
#' @param echoall Debugging tool (not fully implemented) of echoing blocks of
#' data as it is being read.
#' @param section Which data set to read. Only applies for a data.ss_new file
#' created by Stock Synthesis. Allows the choice of either expected values
#' (section=2) or bootstrap data (section=3+). Leaving default of section=NULL
#' will read input data, (equivalent to section=1).
#' @author Ian Taylor
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_forecast}},
#' \code{\link{rd_ctl}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data import
rd_data <- function(file,verbose=FALSE,echoall=FALSE,section=NULL){
  # function to read Stock Synthesis data files

  if(verbose) cat("running rd_data\n")
  dat <- readLines(file,warn=FALSE)

  # split apart any bootstrap or expected value sections in data.ss_new
  if(!is.null(section)){
    Nsections <- as.numeric(substring(dat[grep("Number_of_datafiles",dat)],24))
    if(!section %in% 1:Nsections) stop("The 'section' input should be within the 'Number_of_datafiles' in a data.ss_new file.\n")
    if(section==1){
      end <- grep("#_expected values with no error added",dat)
      if (length(end) == 0) end <- length(dat)
      dat <- dat[grep("#_observed data:",dat):end]
    }
    if(section==2){
      end <- grep("#_bootstrap file: 1",dat)
      if (length(end) == 0) end <- length(dat)
      dat <- dat[grep("#_expected values with no error added",dat):end]
    }
    if(section>=3){
      start <- grep(paste("#_bootstrap file:",section-2),dat)
      end <- grep(paste("#_bootstrap file:",section-1),dat)
      if(length(end)==0) end <- length(dat)
      dat <- dat[start:end]
    }
  }

  # parse all the numeric values into a long vector (allnums)
  temp <- strsplit(dat[2]," ")[[1]][1]
  if(!is.na(temp) && temp=="Start_time:") dat <- dat[-(1:2)]
  allnums <- NULL
  for(i in 1:length(dat)){
    # split along blank spaces
    mysplit <- strsplit(dat[i],split="[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit!=""]
    # if final value is a number is followed immediately by a pound ("1#"),
    # this needs to be split
    nvals <- length(mysplit)
    if(nvals>0) mysplit[nvals] <- strsplit(mysplit[nvals],"#")[[1]][1]
    # convert to numeric
    nums <- suppressWarnings(as.numeric(mysplit))
    if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
    else maxcol <- length(nums)
    if(maxcol > 0){
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }
  # set initial position in the vector of numeric values
  i <- 1
  # create empty list to store quantities
  datlist <- list()

  # specifications
  datlist$sourcefile <- file
  datlist$type <- "Stock_Synthesis_data_file"
  datlist$SSversion <- NULL # "SSv3.21"

  if(verbose) cat("SSversion =",datlist$SSversion,"\n")

  # model dimensions
  datlist$styr  <- allnums[i]; i <- i+1
  datlist$endyr <- allnums[i]; i <- i+1
  datlist$nseas <- nseas <- allnums[i]; i <- i+1
  datlist$months_per_seas <- allnums[i:(i+nseas-1)]; i <- i+nseas
  datlist$spawn_seas <- allnums[i]; i <- i+1
  datlist$Nfleet <- Nfleet <- allnums[i]; i <- i+1
  datlist$Nsurveys <- Nsurveys <- allnums[i]; i <- i+1
  Ntypes <- Nfleet+Nsurveys
  datlist$N_areas <- allnums[i]; i <- i+1

  # an attempt at getting the fleet names based on occurance of %-sign
  fleetnames.good <- NULL
  if(Ntypes>1){
    percentlines <- grep('%',dat)
    for(iline in percentlines){
      fleetnames <- dat[iline]
      fleetnames <- strsplit(fleetnames,'%')[[1]]
      # strip any white space off the end of the fleetnames
      fleetnames[length(fleetnames)] <- strsplit(fleetnames[length(fleetnames)],"[[:blank:]]+")[[1]][1]
      if(length(fleetnames)==Ntypes) fleetnames.good <- fleetnames
    }
    fleetnames <- fleetnames.good
    if(is.null(fleetnames))
      fleetnames <- c(paste("fishery",1:Nfleet),paste("survey",1:Nsurveys))
  }else{
    fleetnames <- "fleet1"
  }
  #if(verbose) cat("fleetnames:",fleetnames,'\n')
  datlist$fleetnames <- fleetnames
  datlist$surveytiming <- surveytiming <- allnums[i:(i+Ntypes-1)]; i <- i+Ntypes
  datlist$areas <- areas <- allnums[i:(i+Ntypes-1)]; i <- i+Ntypes
  if(verbose){
    cat("areas:",areas,'\n')
    cat("fleet info:\n")
    print(data.frame(fleet  = 1:Ntypes,
                     name   = fleetnames,
                     area   = areas,
                     timing = surveytiming,
                     type   = c(rep("FISHERY",Nfleet), rep("SURVEY",Nsurveys))))
  }
  # fleet info
  fleetinfo1 <- data.frame(rbind(surveytiming,areas))
  names(fleetinfo1) <- fleetnames
  fleetinfo1$input <- c("#_surveytiming","#_areas")
  datlist$fleetinfo1 <- fleetinfo1
  ## if(verbose){
  ##   cat("fleetinfo1:\n")
  ##   print(t(fleetinfo1))
  ## }

  datlist$units_of_catch <- units_of_catch <- allnums[i:(i+Nfleet-1)]; i <- i+Nfleet
  datlist$se_log_catch <- se_log_catch <- allnums[i:(i+Nfleet-1)]; i <- i+Nfleet
  fleetinfo2 <- data.frame(rbind(units_of_catch,se_log_catch))
  names(fleetinfo2) <- fleetnames[1:Nfleet]
  fleetinfo2$input <- c("#_units_of_catch","#_se_log_catch")
  datlist$fleetinfo2 <- fleetinfo2
  ## if(verbose){
  ##   cat("fleetinfo2:\n")
  ##   print(t(fleetinfo2))
  ## }


  # more dimensions
  datlist$Ngenders <- Ngenders <- allnums[i]; i <- i+1
  datlist$Nages <- Nages <- allnums[i]; i <- i+1
  datlist$init_equil <- allnums[i:(i+Nfleet-1)]; i <- i+Nfleet

  # catch
  datlist$N_catch <- N_catch <- allnums[i]; i <- i+1
  if(verbose) cat("N_catch =",N_catch,"\n")
  Nvals <- N_catch*(Nfleet+2)
  catch <- data.frame(matrix(allnums[i:(i+Nvals-1)],
                             nrow=N_catch,ncol=(Nfleet+2),byrow=TRUE))
  names(catch) <- c(fleetnames[1:Nfleet],"year","seas")
  datlist$catch <- catch
  i <- i+Nvals
  if(echoall) print(catch)

  # CPUE
  datlist$N_cpue <- N_cpue <- allnums[i]; i <- i+1
  if(verbose) cat("N_cpue =",N_cpue,"\n")
  if(N_cpue > 0){
    CPUEinfo <- data.frame(matrix(allnums[i:(i+Ntypes*3-1)],
                                  nrow=Ntypes,ncol=3,byrow=TRUE))
    i <- i+Ntypes*3
    names(CPUEinfo) <- c("Fleet","Units","Errtype")
    CPUE <- data.frame(matrix(
      allnums[i:(i+N_cpue*5-1)],nrow=N_cpue,ncol=5,byrow=TRUE))
    i <- i+N_cpue*5
    names(CPUE) <- c('year','seas','index','obs','se_log')
  }else{
    CPUEinfo <- NULL
    CPUE <- NULL
  }
  datlist$CPUEinfo <- CPUEinfo
  datlist$CPUE <- CPUE
  if(echoall){
    print(CPUEinfo)
    print(CPUE)
  }

  # discards
  # datlist$discard_units <- discard_units <- allnums[i]; i <- i+1
  datlist$N_discard_fleets <- N_discard_fleets <- allnums[i]; i <- i+1
  if(verbose) cat("N_discard_fleets =",N_discard_fleets,"\n")
  N_discard <- 0 # temporarily set to 0
  if(N_discard_fleets > 0){
    # fleet info
    Ncols <- 3
    discard_fleet_info <- data.frame(matrix(
      allnums[i:(i+N_discard_fleets*Ncols-1)],nrow=N_discard_fleets,ncol=Ncols,byrow=TRUE))
    i <- i+N_discard_fleets*Ncols
    names(discard_fleet_info) <- c("Fleet","units","errtype")
  }else{
    discard_fleet_info <- NULL
  }
  datlist$N_discard <- N_discard <- allnums[i]; i <- i+1
  if(verbose) cat("N_discard =",N_discard,"\n")
  if(N_discard > 0){
    # discard data
    Ncols <- 5
    discard_data <- data.frame(matrix(
      allnums[i:(i+N_discard*Ncols-1)],nrow=N_discard,ncol=Ncols,byrow=TRUE))
    i <- i+N_discard*Ncols
    names(discard_data) <- c('Yr','Seas','Flt','Discard','Std_in')
  }else{
    discard_data <- NULL
  }
  datlist$discard_fleet_info <- discard_fleet_info
  datlist$discard_data <- discard_data

  # meanbodywt
  datlist$N_meanbodywt <- N_meanbodywt <- allnums[i]; i <- i+1
  if(verbose) cat("N_meanbodywt =",N_meanbodywt,"\n")
  datlist$DF_for_meanbodywt <- allnums[i]
  i <- i+1
  if(echoall) cat("DF_for_meanbodywt =",datlist$DF_for_meanbodywt,"\n")

  if(N_meanbodywt > 0){
    Ncols <- 6
    meanbodywt <- data.frame(matrix(
      allnums[i:(i+N_meanbodywt*Ncols-1)],nrow=N_meanbodywt,ncol=Ncols,byrow=TRUE))
    i <- i+N_meanbodywt*Ncols
    names(meanbodywt) <- c('Year','Seas','Type','Partition','Value','CV')
  }else{
    meanbodywt <- NULL
  }
  datlist$meanbodywt <- meanbodywt
  if(echoall) print(meanbodywt)

  # length data
  datlist$lbin_method <- lbin_method <- allnums[i]; i <- i+1
  if(echoall) cat("lbin_method =",lbin_method,"\n")
  if(lbin_method==2){
    datlist$binwidth <- allnums[i]; i <- i+1
    datlist$minimum_size <- allnums[i]; i <- i+1
    datlist$maximum_size <- allnums[i]; i <- i+1
    if(echoall) cat("bin width, min, max =",datlist$binwidth,", ",datlist$minimum_size,", ",datlist$maximum_size,"\n")
  }else{
    datlist$binwidth <- NA
    datlist$minimum_size <- NA
    datlist$maximum_size <- NA
  }
  if(lbin_method==3){
    datlist$N_lbinspop <- N_lbinspop <- allnums[i]; i <- i+1
    datlist$lbin_vector_pop <- allnums[i:(i+N_lbinspop-1)]; i <- i+N_lbinspop
    if(echoall) cat("N_lbinspop =",N_lbinspop,"\nlbin_vector_pop:\n")
  }else{
    datlist$N_lbinspop <- NA
    datlist$lbin_vector_pop <- NA
  }

  datlist$comp_tail_compression <- allnums[i]; i <- i+1
  datlist$add_to_comp <- allnums[i]; i <- i+1
  datlist$max_combined_age <- allnums[i]; i <- i+1
  datlist$N_lbins <- N_lbins <- allnums[i]; i <- i+1
  datlist$lbin_vector <- lbin_vector <- allnums[i:(i+N_lbins-1)]; i <- i+N_lbins
  if(echoall) print(lbin_vector)

  datlist$N_lencomp <- N_lencomp <- allnums[i]; i <- i+1

  # if(verbose) cat(datlist[-15:0 + length(datlist)])
  if(verbose) cat("N_lencomp =",N_lencomp,"\n")

  if(N_lencomp > 0){
    Ncols <- N_lbins*Ngenders+6
    lencomp <- data.frame(matrix(
      allnums[i:(i+N_lencomp*Ncols-1)],nrow=N_lencomp,ncol=Ncols,byrow=TRUE))
    i <- i+N_lencomp*Ncols
    names(lencomp) <- c("Yr","Seas","FltSvy","Gender","Part","Nsamp",
                        if(Ngenders==1){paste("l",lbin_vector,sep="")}else{NULL},
                        if(Ngenders>1){ c(paste("f",lbin_vector,sep=""),paste("m",lbin_vector,sep="")) }else{ NULL } )
  }else{
    lencomp <- NULL
  }
  datlist$lencomp <- lencomp

  # age data
  datlist$N_agebins <- N_agebins <- allnums[i]; i <- i+1
  if(verbose) cat("N_agebins =",N_agebins,"\n")
  if(N_agebins > 0){
    agebin_vector <- allnums[i:(i+N_agebins-1)]; i <- i+N_agebins
  }else{
    agebin_vector <- NULL
  }
  datlist$agebin_vector <- agebin_vector
  if(echoall) print(agebin_vector)

  datlist$N_ageerror_definitions <- N_ageerror_definitions <- allnums[i]; i <- i+1
  if(N_ageerror_definitions > 0){
    Ncols <- Nages+1
    ageerror <- data.frame(matrix(
      allnums[i:(i+2*N_ageerror_definitions*Ncols-1)],
      nrow=2*N_ageerror_definitions,ncol=Ncols,byrow=TRUE))
    i <- i+2*N_ageerror_definitions*Ncols
    names(ageerror) <- paste("age",0:Nages,sep="")
  }else{
    ageerror <- NULL
  }
  datlist$ageerror <- ageerror

  datlist$N_agecomp <- N_agecomp <- allnums[i]; i <- i+1
  if(verbose) cat("N_agecomp =",N_agecomp,"\n")

  datlist$Lbin_method <- allnums[i]; i <- i+1
  datlist$max_combined_lbin <- allnums[i]; i <- i+1

  if(N_agecomp > 0){
    if(N_agebins==0) stop("N_agecomp =",N_agecomp," but N_agebins = 0")
    Ncols <- N_agebins*Ngenders+9
    agecomp <- data.frame(matrix(allnums[i:(i+N_agecomp*Ncols-1)],
                                 nrow=N_agecomp,ncol=Ncols,byrow=TRUE))
    i <- i+N_agecomp*Ncols
    names(agecomp) <- c("Yr","Seas","FltSvy","Gender","Part","Ageerr","Lbin_lo","Lbin_hi","Nsamp",
                        if(Ngenders==1){paste("a",agebin_vector,sep="")}else{NULL},
                        if(Ngenders>1){ c(paste("f",agebin_vector,sep=""),paste("m",agebin_vector,sep="")) }else{ NULL } )
  }else{
    agecomp <- NULL
  }
  datlist$agecomp <- agecomp

  # MeanSize_at_Age
  datlist$N_MeanSize_at_Age_obs <- N_MeanSize_at_Age_obs <- allnums[i]; i <- i+1
  if(verbose) cat("N_MeanSize_at_Age_obs =",N_MeanSize_at_Age_obs,"\n")
  if(N_MeanSize_at_Age_obs > 0){
    Ncols <- 2*N_agebins*Ngenders + 7
    MeanSize_at_Age_obs <- data.frame(matrix(
      allnums[i:(i+N_MeanSize_at_Age_obs*Ncols-1)],nrow=N_MeanSize_at_Age_obs,ncol=Ncols,byrow=TRUE))
    i <- i+N_MeanSize_at_Age_obs*Ncols
    names(MeanSize_at_Age_obs) <- c('Yr','Seas','FltSvy','Gender','Part','AgeErr','Ignore',
                                    if(Ngenders==1){paste("a",agebin_vector,sep="")}else{NULL},
                                    if(Ngenders>1){ c(paste("f",agebin_vector,sep=""),paste("m",agebin_vector,sep="")) }else{ NULL },
                                    if(Ngenders==1){paste("N_a",agebin_vector,sep="")}else{NULL},
                                    if(Ngenders>1){ c(paste("N_f",agebin_vector,sep=""),paste("N_m",agebin_vector,sep="")) }else{ NULL } )
  }else{
    MeanSize_at_Age_obs <- NULL
  }
  datlist$MeanSize_at_Age_obs <- MeanSize_at_Age_obs

  # other stuff
  datlist$N_environ_variables <- N_environ_variables <- allnums[i]; i <- i+1
  datlist$N_environ_obs <- N_environ_obs <- allnums[i]; i <- i+1
  if(N_environ_obs > 0){
    Ncols <- 3
    envdat <- data.frame(matrix(
      allnums[i:(i+Ncols*N_environ_obs-1)],nrow=N_environ_obs,ncol=Ncols,byrow=TRUE))
    i <- i+N_environ_obs*Ncols
    names(envdat) <- c("Yr","Variable","Value")
  }else{
    envdat <- NULL
  }
  datlist$envdat <- envdat

  datlist$N_sizefreq_methods <- N_sizefreq_methods <- allnums[i]; i <- i+1
  if(verbose) cat("N_sizefreq_methods =",N_sizefreq_methods,"\n")
  if(N_sizefreq_methods > 0){
    # get details of generalized size frequency methods
    datlist$nbins_per_method   <- nbins_per_method   <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    datlist$units_per_method   <- units_per_method   <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    datlist$scale_per_method   <- scale_per_method   <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    datlist$mincomp_per_method <- mincomp_per_method <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    datlist$Nobs_per_method    <- Nobs_per_method    <- allnums[i:(i+N_sizefreq_methods-1)]
    i <- i+N_sizefreq_methods
    if(verbose){
      cat("details of generalized size frequency methods:\n")
      print(data.frame(method  = 1:N_sizefreq_methods,
                       nbins   = nbins_per_method,
                       units   = units_per_method,
                       scale   = scale_per_method,
                       mincomp = mincomp_per_method,
                       nobs    = Nobs_per_method))
    }
    # get list of bin vectors
    sizefreq_bins_list <- list()
    for(imethod in 1:N_sizefreq_methods){
      sizefreq_bins_list[[imethod]] <- allnums[i:(i+nbins_per_method[imethod]-1)]
      i <- i+nbins_per_method[imethod]
    }
    datlist$sizefreq_bins_list <- sizefreq_bins_list
    # read generalized size frequency data
    sizefreq_data_list <- list()
    for(imethod in 1:N_sizefreq_methods){
      Ncols <- 7+2*nbins_per_method[imethod]
      Nrows <- Nobs_per_method[imethod]
      sizefreq_data_tmp <- data.frame(matrix(allnums[i:(i+Nrows*Ncols-1)],
                                             nrow=Nrows,ncol=Ncols,byrow=TRUE))
      names(sizefreq_data_tmp) <- c("Method","Yr","Seas","FltSvy","Gender","Part","Nsamp",
                                    paste("f",sizefreq_bins_list[[imethod]],sep=""),
                                    paste("m",sizefreq_bins_list[[imethod]],sep=""))
      if(verbose){
        cat("Method =",imethod,"  (first two rows, ten columns):\n")
        print(sizefreq_data_tmp[1:min(Nrows,2),1:min(Ncols,10)])
      }
      if(any(sizefreq_data_tmp$Method!=imethod))
        stop("Problem with method in size frequency data:\n",
             "Expecting method: ",imethod,"\n",
             "Read method(s): ",paste(unique(sizefreq_data_tmp$Method),collapse=", "))
      sizefreq_data_list[[i]] <- sizefreq_data_tmp
      i <- i+Nrows*Ncols
    }
    datlist$sizefreq_data_list <- sizefreq_data_list
  }else{
    datlist$nbins_per_method   <- NULL
    datlist$units_per_method   <- NULL
    datlist$scale_per_method   <- NULL
    datlist$mincomp_per_method <- NULL
    datlist$Nobs_per_method    <- NULL
    datlist$sizefreq_bins_list <- NULL
    datlist$sizefreq_data_list <- NULL
  }

  datlist$do_tags <- do_tags <- allnums[i]; i <- i+1
  if(verbose) cat("do_tags =",do_tags,"\n")

  if(do_tags != 0){
    datlist$N_tag_groups <- N_tag_groups <- allnums[i]; i <- i+1
    datlist$N_recap_events <- N_recap_events <- allnums[i]; i <- i+1
    datlist$mixing_latency_period <- mixing_latency_period <- allnums[i]; i <- i+1
    datlist$max_periods <- max_periods <- allnums[i]; i <- i+1

    # read tag release data
    if(N_tag_groups > 0){
      Ncols <- 8
      tag_releases <- data.frame(matrix(allnums[i:(i+N_tag_groups*Ncols-1)],nrow=N_tag_groups,ncol=Ncols,byrow=TRUE))
      i <- i+N_tag_groups*Ncols
      names(tag_releases) <- c('TG', 'Area', 'Yr', 'Season', 'tfill', 'Gender', 'Age', 'Nrelease')
      if(verbose){
        cat("Head of tag release data:\n")
        print(head(tag_releases))
      }
    }else{
      tag_releases <- NULL
    }
    datlist$tag_releases <- tag_releases

    # read tag recapture data
    if(N_recap_events > 0){
      Ncols <- 5
      tag_recaps <- data.frame(matrix(allnums[i:(i+N_recap_events*Ncols-1)],nrow=N_recap_events,ncol=Ncols,byrow=TRUE))
      i <- i+N_recap_events*Ncols
      names(tag_recaps) <- c('TG', 'Yr', 'Season', 'Fleet', 'Nrecap')
      if(verbose){
        cat("Head of tag recapture data:\n")
        print(head(tag_recaps))
      }
    }else{
      tag_recaps <- NULL
    }
    datlist$tag_recaps <- tag_recaps
  }

  datlist$morphcomp_data <- do_morphcomps <- allnums[i]; i <- i+1
  if(verbose) cat("do_morphcomps =",do_morphcomps,"\n")

  if(allnums[i]==999){
    if(verbose) cat("read of data file complete (final value = 999)\n")
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
  }

  # return the result
  return(datlist)
}

#' read control file
#'
#' read Stock Synthesis control file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param data_input List object created by \code{\link{rd_data}}.
#' @author Nathan Vaughan
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_forecast}},
#' \code{\link{rd_data}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data import
rd_ctl <- function(file, data_input){
  ctl <- readLines(con=file,warn=FALSE)

  ControlFile<-list()

  ControlFile$sourcefile<-file
  ControlFile$type<-"Stock_Synthesis_control_file"

  ctlComs<-ctlVals<-ctl2<-strsplit(ctl,"#")
  for(i in 1:length(ctl2))
  {
    ctlComs[[i]]<-ctl2[[i]][-1]
    tmp<-unlist(strsplit(unlist(strsplit(ctl2[[i]][1],"\t"))," "))
    ctlVals[[i]]<-as.numeric(tmp[tmp!=""])
  }
  ctlVals<-ctlVals[lengths(ctlVals)>0]
  ctlVals<-ctlVals[!is.na(ctlVals)]

  readLine<-1

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Num_GP<-ctlVals[[readLine]]
    print(paste("Number of Growth patterns = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: The number of Growth patterns should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Num_SubMorphs<-ctlVals[[readLine]]
    print(paste("Number of sub-morphs within growth patterns = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: The number of sub-morphs should be a single value")
    return(NULL)
  }

  if(ControlFile$Num_SubMorphs>1)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$SubMorphs_SD<-ctlVals[[readLine]]
      print(paste("Morph between/within stdev ratio = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Sub-morph SD should be a single value")
      return(NULL)
    }


    if(ctlVals[[readLine]]==-1 || length(ctlVals[[readLine]])==ControlFile$Num_SubMorphs)
    {
      ControlFile$SubMorphs_Dist<-ctlVals[[readLine]]
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Distribution among sub-morphs = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Dist among submorphs must be -1 or a vector whose length is equal to the number of sub morphs")
      return(NULL)
    }
  }

  if((ControlFile$Num_GP*data_input$nseas*data_input$N_areas)>1)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$Num_RecruitAssign<-ctlVals[[readLine]]
      print(paste("Number of recruitment assignments = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: The number of recruitment assignments should be a single value")
      return(NULL)
    }

    if(length(ctlVals[[readLine]])==1 && (ctlVals[[readLine]]==0 || ctlVals[[readLine]]==1))
    {
      ControlFile$Recuit_Interact<-ctlVals[[readLine]]
      print(paste("Recruitment interactions = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Recruitment interactions should be a single value equal to 0 or 1")
      return(NULL)
    }

    RecruitAssign<-list()
    for(i in 1:ControlFile$Num_RecruitAssign)
    {
      if(length(ctlVals[[readLine]])==3)
      {
        RecruitAssign[[i]]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Recruitment assignment ",i," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: Recruitment assignments should contain 3 values representing the growth pattern, season, and area")
        return(NULL)
      }
    }
    ControlFile$RecruitAssign<-RecruitAssign

    if(data_input$N_areas>1)
    {
      if(length(ctlVals[[readLine]])==1)
      {
        ControlFile$Num_MoveDefs<-ctlVals[[readLine]]
        print(paste("Number of movement definitions = ",ctlVals[[readLine]],sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: Number of movement definitions should be a single value")
        return(NULL)
      }

      if(as.numeric(ControlFile$Num_MoveDefs)>0)
      {
        if(length(ctlVals[[readLine]])==1)
        {
          ControlFile$First_age<-ctlVals[[readLine]]
          print(paste("First age that moves = ",ctlVals[[readLine]],sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: First age that moves should be a single value")
          return(NULL)
        }

        MoveDefs<-list()
        for(i in 1:ControlFile$Num_MoveDefs)
        {
          if(length(ctlVals[[readLine]])==6)
          {
            MoveDefs[[i]]<-ctlVals[[readLine]]
            outText<-paste(ctlVals[[readLine]],collapse = " ")
            print(paste("Movement definition ",i," = ",outText,sep=""))
            readLine<-readLine+1
          }else
          {
            print("Error: Movement definitinos should contain 6 values representing the season, growth pattern, source area, destination area, min age, and max age")
            return(NULL)
          }
        }
        ControlFile$MoveDefs<-MoveDefs
      }
    }
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Num_BlockPatterns<-ctlVals[[readLine]]
    print(paste("Number of block patterns = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: The number of block patterns should be a single value")
    return(NULL)
  }

  if(ControlFile$Num_BlockPatterns>0)
  {
    if(length(ctlVals[[readLine]])==ControlFile$Num_BlockPatterns)
    {
      ControlFile$BlocksPerPattern<-ctlVals[[readLine]]
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Number of blocks per pattern = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be a value of number of blocks per pattern for each pattern determined by number of block patterns")
      return(NULL)
    }

    BlockPatterns<-list()
    for(i in 1:ControlFile$Num_BlockPatterns)
    {
      if(length(ctlVals[[readLine]])==2*ControlFile$BlocksPerPattern[i])
      {
        BlockPatterns[[i]]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Block Pattern ",i," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: Block patterns must be described by a start and end year for each block")
        return(NULL)
      }
    }
    ControlFile$BlockPatterns<-BlockPatterns
  }

  #Read in the Control values for the biological parameters

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$frac_Female<-ctlVals[[readLine]]
    print(paste("The fraction female = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: The fraction female should be a single value")
    return(NULL)
  }

  NumS<-data_input$Ngenders

  if(length(ctlVals[[readLine]])==1 && ctlVals[[readLine]]<=4)
  {
    ControlFile$M_option<-ctlVals[[readLine]]
    print(paste("Natural maturity option  selected = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: The natural matury option should be a single value integer 0,1,2,3, or 4")
    return(NULL)
  }

  if(ControlFile$M_option==0)
  {
    NumM<-1
  }else if(ControlFile$M_option==1)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$M_numBreaks<-ctlVals[[readLine]]
      print(paste("Number of natural maturity breakpoints  selected = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Natural mortality breakpoints should be a single value")
      return(NULL)
    }

    if(length(ctlVals[[readLine]])==ControlFile$M_numBreaks)
    {
      ControlFile$M_ageBreaks<-ctlVals[[readLine]]
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Natural maturity breakpoint ages  selected = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Length of age breaks vector not equal to number of breaks")
      return(NULL)
    }
    NumM<-ControlFile$M_numBreaks
  }else if(ControlFile$M_option==2)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$M_refAge<-ctlVals[[readLine]]
      print(paste("Lorenzen function reference age = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Lorenzen function reference age should be a single value")
      return(NULL)
    }
    NumM<-1
  }else if(ControlFile$M_option==3 || ControlFile$M_option==4)
  {
    if(NumS==1)
    {
      AgeSpecificM<-list()
      for(i in 1:ControlFile$Num_GP)
      {
        if(length(ctlVals[[readLine]])==(data_input$Nages + 1))
        {
          AgeSpecificM[[i]]<-ctlVals[[readLine]]
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Age specific natural mortality ",i," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: Age specific natural mortality must be represented by a vector of length equal to the number of ages plus 1")
          return(NULL)
        }
      }
      ControlFile$AgeSpecificM<-AgeSpecificM
    }else{
      AgeSpecificM<-list()
      for(i in 1:(ControlFile$Num_GP*2))
      {
        if(length(ctlVals[[readLine]])==(data_input$Nages + 1))
        {
          AgeSpecificM[[i]]<-ctlVals[[readLine]]
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Age specific natural mortality ",i," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: Age specific natural mortality must be represented by a vector of length equal to the number of ages plus 1")
          return(NULL)
        }
      }
      ControlFile$AgeSpecificM<-AgeSpecificM
    }
    NumM<-0
  }

  if(length(ctlVals[[readLine]])==1 && ctlVals[[readLine]]>=1 && ctlVals[[readLine]]<=3)
  {
    ControlFile$Growth_Model<-ctlVals[[readLine]]
    print(paste("Growth Model = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Growth Model should be 1, 2, or 3")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Growth_Amin<-ctlVals[[readLine]]
    print(paste("Reference Age 1 = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Reference Age 1 should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Growth_Amax<-ctlVals[[readLine]]
    print(paste("Reference Age 2 = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Reference Age 2 should be a single value")
    return(NULL)
  }

  if(ControlFile$Growth_Model==1)
  {
    NumG=0
  }else if(ControlFile$Growth_Model==2)
  {
    NumG=1
  }else if(ControlFile$Growth_Model==3)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$NumKmult<-ctlVals[[readLine]]
      print(paste("Number of K multipliers = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1

      NumG=(ControlFile$NumKmult)

      if(length(ctlVals[[readLine]])==ControlFile$NumKmult)
      {
        ControlFile$KMultAges<-ctlVals[[readLine]]
        print(paste("Ages for K Multiplier = ",ctlVals[[readLine]],sep=""))
        readLine<-readLine+1
      }else
      {
        print(paste0("Error: Number of ages for K should be a vector of length ",ControlFile$NumKmult))
        return(NULL)
      }

    }else
    {
      print("Error: number of K multipliers should be a single value")
      return(NULL)
    }
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SDadd_LAA<-ctlVals[[readLine]]
    print(paste("SD to add to Length at age = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: SD added to length at age should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1 && ctlVals[[readLine]]>=0 && ctlVals[[readLine]]<=4)
  {
    ControlFile$CV_pattern<-ctlVals[[readLine]]
    print(paste("CV Pattern = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: CV Pattern should be 0, 1, 2, 3, or 4")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1 && ctlVals[[readLine]]>=1 && ctlVals[[readLine]]<=6)
  {
    ControlFile$MaturityOpt<-ctlVals[[readLine]]
    print(paste("Maturity option = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Maturity option should be 1, 2, 3, 4, 5, or 6")
    return(NULL)
  }

  if(ControlFile$MaturityOpt==3 || ControlFile$MaturityOpt==4)
  {
    MaturityVect<-list()
    for(i in 1:ControlFile$Num_GP)
    {
      if(length(ctlVals[[readLine]])==(data_input$Nages + 1))
      {
        MaturityVect[[i]]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Maturity vector ",i," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: Maturity vector should be of length equal to number of ages plus 1")
        return(NULL)
      }
    }
    ControlFile$MaturityVect<-MaturityVect
  }

  if(ControlFile$MaturityOpt==6)
  {
    MaturityVect<-list()
    for(i in 1:ControlFile$Num_GP)
    {
      if(length(ctlVals[[readLine]])==(data_input$N_lbinspop))
      {
        MaturityVect[[i]]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Maturity vector ",i," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: Maturity vector should be of length equal to number of lengths")
        return(NULL)
      }
    }
    ControlFile$MaturityVect<-MaturityVect
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$FirstMatAge<-ctlVals[[readLine]]
    print(paste("First mature age = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: First mature age should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1 && ctlVals[[readLine]]>=1 && ctlVals[[readLine]]<=5)
  {
    ControlFile$FecundityOpt<-ctlVals[[readLine]]
    print(paste("Fecundity option = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Fecundity option should be 1, 2, 3, 4, or 5")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1 && (ctlVals[[readLine]]==1 || ctlVals[[readLine]]==0))
  {
    ControlFile$HermaphOpt<-ctlVals[[readLine]]
    print(paste("Hermaphroditism option = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Hermaphroditism option should be 0 or 1")
    return(NULL)
  }

  if(ControlFile$HermaphOpt==1)
  {
    if(length(ctlVals[[readLine]])==1 && (ctlVals[[readLine]]==-1 || ctlVals[[readLine]]>0))
    {
      ControlFile$HermaphSeas<-ctlVals[[readLine]]
      print(paste("Hermaphroditism season = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Hermaphroditism season should be -1 or positive integer")
      return(NULL)
    }

    if(length(ctlVals[[readLine]])==1 && (ctlVals[[readLine]]==1 || ctlVals[[readLine]]==0))
    {
      ControlFile$MalesSpawn<-ctlVals[[readLine]]
      print(paste("Include male biomass in calculation of SSB = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Include males in SSB should be 0 or 1")
      return(NULL)
    }
    NumH<-3
  }else
  {
    NumH<-0
  }

  if(length(ctlVals[[readLine]])==1 && (ctlVals[[readLine]]>=1 || ctlVals[[readLine]]<=3))
  {
    ControlFile$OffsetMethod<-ctlVals[[readLine]]
    print(paste("Offset method = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Offset method should be 1, 2, or 3")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1 && (ctlVals[[readLine]]==1 || ctlVals[[readLine]]==2))
  {
    ControlFile$TVAconstraint<-ctlVals[[readLine]]
    print(paste("Time-varying adjustment constraint method = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Time-varying adjustment constraint method should be 1 or 2")
    return(NULL)
  }

  #Read the natural mortality, growth, and other biological parameters

  MG_Params<-matrix(nrow=0,ncol=14)
  colnames(MG_Params)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase","UseEnv","UseDev","DevMinYr","DevMaxYr","DevStDev","UseBlock","BlockType")
  for(i in 1:NumS)
  {
    for(k in 1:ControlFile$Num_GP)
    {
      if(NumM>0)
      {
        for(j in 1:NumM)
        {
          if(length(ctlVals[[readLine]])==14)
          {
            MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
            rownames(MG_Params)[length(MG_Params[,1])]<-"natM"
            outText<-paste(ctlVals[[readLine]],collapse = " ")
            print(paste("natural mortality = ",outText,sep=""))
            readLine<-readLine+1
          }else
          {
            print("Error: There should be 14 values in a parameter line, I think this should have been a natural Mortality Param???")
            return(NULL)
          }
        }
      }

      if(length(ctlVals[[readLine]])==14)
      {
        MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
        rownames(MG_Params)[length(MG_Params[,1])]<-"LminA"
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Length at min age = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 14 values in a parameter line, I think this should have been the Length at min age Param???")
        return(NULL)
      }

      if(length(ctlVals[[readLine]])==14)
      {
        MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
        rownames(MG_Params)[length(MG_Params[,1])]<-"LmaxA"
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Length at max age = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 14 values in a parameter line, I think this should have been the Length at max age Param???")
        return(NULL)
      }

      if(length(ctlVals[[readLine]])==14)
      {
        MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
        rownames(MG_Params)[length(MG_Params[,1])]<-"VBK"
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("von Bertalanffy K = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 14 values in a parameter line, I think this should have been the VBK Param???")
        return(NULL)
      }

      if(NumG==1)
      {
        if(length(ctlVals[[readLine]])==14)
        {
          MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
          rownames(MG_Params)[length(MG_Params[,1])]<-"RichCo"
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Richards Coefficient = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: There should be 14 values in a parameter line, I think this should have been the Richards coefficient???")
          return(NULL)
        }
      }

      if(NumG>1)
      {
        for(j in 1:NumG)
        {
          if(length(ctlVals[[readLine]])==14)
          {
            MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
            rownames(MG_Params)[length(MG_Params[,1])]<-paste("Kdev_",j,"_age_",ControlFile$KMultAges[j])
            outText<-paste(ctlVals[[readLine]],collapse = " ")
            print(paste("K Deviation ",j," age ",ControlFile$KMultAges[j]," = ",outText,sep=""))
            readLine<-readLine+1
          }else
          {
            print("Error: There should be 14 values in a parameter line, I think this should have been a age specific K deviation???")
            return(NULL)
          }
        }
      }

      if(length(ctlVals[[readLine]])==14)
      {
        MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
        rownames(MG_Params)[length(MG_Params[,1])]<-"CV_young"
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("CV for young = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 14 values in a parameter line, I think this should have been CV for young???")
        return(NULL)
      }

      if(length(ctlVals[[readLine]])==14)
      {
        MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
        rownames(MG_Params)[length(MG_Params[,1])]<-"CV_old"
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("CV for old = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 14 values in a parameter line, I think this should have been CV for old???")
        return(NULL)
      }

    }
  }

  if(length(ctlVals[[readLine]])==14)
  {
    MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
    rownames(MG_Params)[length(MG_Params[,1])]<-"Fwtln1"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Female wt-ln parameter 1 = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: There should be 14 values in a parameter line, I think this should have been Female wt-ln parameter 1???")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==14)
  {
    MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
    rownames(MG_Params)[length(MG_Params[,1])]<-"Fwtln2"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Female wt-ln parameter 2 = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: There should be 14 values in a parameter line, I think this should have been Female wt-ln parameter 2???")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==14)
  {
    MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
    rownames(MG_Params)[length(MG_Params[,1])]<-"Fmat1"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Female maturity parameter 1 = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: There should be 14 values in a parameter line, I think this should have been Female maturity parameter 1???")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==14)
  {
    MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
    rownames(MG_Params)[length(MG_Params[,1])]<-"Fmat2"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Female maturity parameter 2 = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: There should be 14 values in a parameter line, I think this should have been Female maturity parameter 2???")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==14)
  {
    MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
    rownames(MG_Params)[length(MG_Params[,1])]<-"Ffec1"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Female fecundity parameter 1 = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: There should be 14 values in a parameter line, I think this should have been Female fecundity parameter 1???")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==14)
  {
    MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
    rownames(MG_Params)[length(MG_Params[,1])]<-"Ffec2"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Female fecundity parameter 2 = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: There should be 14 values in a parameter line, I think this should have been Female fecundity parameter 2???")
    return(NULL)
  }

  if(NumS==2)
  {
    if(length(ctlVals[[readLine]])==14)
    {
      MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
      rownames(MG_Params)[length(MG_Params[,1])]<-"Mwtln1"
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Male wt-ln parameter 1 = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 14 values in a parameter line, I think this should have been male wt-ln parameter 1???")
      return(NULL)
    }

    if(length(ctlVals[[readLine]])==14)
    {
      MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
      rownames(MG_Params)[length(MG_Params[,1])]<-"Mwtln2"
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Male wt-ln parameter 2 = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 14 values in a parameter line, I think this should have been male wt-ln parameter 2???")
      return(NULL)
    }
  }

  if(NumH>0)
  {
    if(length(ctlVals[[readLine]])==14)
    {
      MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
      rownames(MG_Params)[length(MG_Params)]<-"Hinfage"
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Hermaphrodite inflection age = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 14 values in a parameter line, I think this should have been hermaphrodite inflection age???")
      return(NULL)
    }

    if(length(ctlVals[[readLine]])==14)
    {
      MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
      rownames(MG_Params)[length(MG_Params[,1])]<-"Hstdev"
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Hermaphrodite standard deviation = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 14 values in a parameter line, I think this should have been hermaphrodite standard deviation???")
      return(NULL)
    }

    if(length(ctlVals[[readLine]])==14)
    {
      MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
      rownames(MG_Params)[length(MG_Params[,1])]<-"Hasmpt"
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Hermaphrodite asymptotic rate = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 14 values in a parameter line, I think this should have been hermaphrodite asymptotic rate???")
      return(NULL)
    }
  }

  for(i in 1:ControlFile$Num_GP)
  {
    if(length(ctlVals[[readLine]])==14)
    {
      MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
      rownames(MG_Params)[length(MG_Params[,1])]<-paste("GPmain",i)
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Growth pattern recruitment apportionment main effect ",i," = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 14 values in a parameter line, I think this should have been Growth pattern recruitment apportionment main effect???")
      return(NULL)
    }
  }

  for(i in 1:data_input$N_areas)
  {
    if(length(ctlVals[[readLine]])==14)
    {
      MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
      rownames(MG_Params)[length(MG_Params[,1])]<-paste("Amain",i)
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Area recruitment apportionment main effect ",i," = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 14 values in a parameter line, I think this should have been Area recruitment apportionment main effect???")
      return(NULL)
    }
  }

  for(i in 1:data_input$nseas)
  {
    if(length(ctlVals[[readLine]])==14)
    {
      MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
      rownames(MG_Params)[length(MG_Params[,1])]<-paste("Smain",i)
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Season recruitment apportionment main effect ",i," = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 14 values in a parameter line, I think this should have been Season recruitment apportionment main effect???")
      return(NULL)
    }
  }

  if(!is.null(ControlFile$Recuit_Interact))
  {
    if(ControlFile$Recuit_Interact==1)
    {
      for(i in 1:(ControlFile$Num_GP*data_input$nseas*data_input$N_areas))
      {
        if(length(ctlVals[[readLine]])==14)
        {
          MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
          rownames(MG_Params)[length(MG_Params[,1])]<-paste("Recint",i)
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Recruitment apportionment interaction effect ",i," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: There should be 14 values in a parameter line, I think this should have been Recruitment apportionment interaction effect???")
          return(NULL)
        }
      }
    }
  }

  if(length(ctlVals[[readLine]])==14)
  {
    MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
    rownames(MG_Params)[length(MG_Params[,1])]<-paste("CoGrowDev")
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Cohort growth deviations = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: There should be 14 values in a parameter line, I think this should have been cohort growth deviation???")
    return(NULL)
  }

  if(data_input$N_areas>1)
  {
    if(!is.null(ControlFile$Num_MoveDefs))
    {
      if(as.numeric(ControlFile$Num_MoveDefs)>0)
      {
        for(i in 1:(2*ControlFile$Num_MoveDefs))
        {
          if(length(ctlVals[[readLine]])==14)
          {
            MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
            rownames(MG_Params)[length(MG_Params[,1])]<-paste("Move",i)
            outText<-paste(ctlVals[[readLine]],collapse = " ")
            print(paste("Movement parameter",i," = ",outText,sep=""))
            readLine<-readLine+1
          }else
          {
            print("Error: There should be 14 values in a parameter line, I think this should have been a movement parameter???")
            return(NULL)
          }
        }
      }
    }
  }

  if(!is.null(data_input$ageerror))
  {
    if(data_input$ageerror$age0[2]<0)
    {
      for(i in 1:7)
      {
        if(length(ctlVals[[readLine]])==14)
        {
          MG_Params<-rbind(MG_Params,ctlVals[[readLine]])
          rownames(MG_Params)[length(MG_Params[,1])]<-paste("AgeErr",i)
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Ageign error",i," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: There should be 14 values in a parameter line, I think this should have been an ageing error parameter???")
          return(NULL)
        }
      }
    }
  }
  ControlFile$MG_Params<-MG_Params

  ControlFile$MG_Params_TV<-list()
  ENV_Params<-MG_Params[MG_Params[,8]!=0,,drop=FALSE]
  if(length(ENV_Params[,8])>0)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$MG_Params_TV$Cust_ENV<-ctlVals[[readLine]]
      print(paste("Custom time varying parameters with environmental linkages = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: The allocation of custom environmental linkages should be a single value")
      return(NULL)
    }

    if(ControlFile$MG_Params_TV$Cust_ENV==1)
    {
      ENV_Params<-ENV_Params[,1:7,drop=FALSE]
      for(i in 1:length(ENV_Params[,7]))
      {
        if(length(ctlVals[[readLine]])==7)
        {
          ENV_Params[i,]<-ctlVals[[readLine]]
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste(rownames(ENV_Params)[i]," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: There should be 7 values in a short parameter line, I think this should have been an environmental linkage parameter???")
          return(NULL)
        }
      }
    }else
    {
      ENV_Params<-ENV_Params[1,1:7,drop=FALSE]
      if(length(ctlVals[[readLine]])==7)
      {
        ENV_Params[1,]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste(rownames(ENV_Params)[1]," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 7 values in a short parameter line, I think this should have been an environmental linkage parameter???")
        return(NULL)
      }
    }
  }
  ControlFile$MG_Params_TV$ENV_Params<-ENV_Params

  BLK_ParamsTV<-MG_Params[MG_Params[,13]!=0,,drop=FALSE]
  BLK_Params<-matrix(nrow=0,ncol=7)
  colnames(BLK_Params)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase")
  if(length(BLK_ParamsTV[,13])>0)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$MG_Params_TV$Cust_Blk<-ctlVals[[readLine]]
      print(paste("Custom time varying parameters with block patterns = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: The allocation of custom block patterns should be a single value")
      return(NULL)
    }

    if(ControlFile$MG_Params_TV$Cust_Blk==1)
    {
      for(i in 1:length(BLK_ParamsTV[,7]))
      {
        if(BLK_ParamsTV[i,13]>0)
        {
          if(BLK_ParamsTV[i,13]<=ControlFile$Num_BlockPatterns)
          {
            for(j in 1:ControlFile$BlocksPerPattern[BLK_ParamsTV[,13]])
            {
              if(length(ctlVals[[readLine]])==7)
              {
                BLK_Params<-rbind(BLK_Params,ctlVals[[readLine]])
                rownames(BLK_Params)[length(BLK_Params[,1])]<-paste("TV_Param",i,"_Block",j)
                outText<-paste(ctlVals[[readLine]],collapse = " ")
                print(paste(rownames(BLK_Params)[length(rownames(BLK_Params))]," = ",outText,sep=""))
                readLine<-readLine+1
              }else
              {
                print("Error: There should be 7 values in a short parameter line, I think this should have been a block pattern parameter???")
                return(NULL)
              }
            }
          }else{
            print("Error: I think this is a block pattern parameter but you a referencing a pattern higher than the number of declared patterns???")
            return(NULL)
          }
        }else{
          for(j in 1:3)
          {
            if(length(ctlVals[[readLine]])==7)
            {
              BLK_Params<-rbind(BLK_Params,ctlVals[[readLine]])
              rownames(BLK_Params)[length(BLK_Params[,1])]<-paste("TV_Param",i,"_Block",j)
              outText<-paste(ctlVals[[readLine]],collapse = " ")
              print(paste(rownames(BLK_Params)[length(rownames(BLK_Params))]," = ",outText,sep=""))
              readLine<-readLine+1
            }else
            {
              print("Error: There should be 7 values in a short parameter line, I think this should have been a block pattern parameter???")
              return(NULL)
            }
          }
        }
      }
    }else
    {
      BLK_Params<-BLK_Params[1,1:7,drop=FALSE]
      if(length(ctlVals[[readLine]])==7)
      {
        BLK_Params[1,]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste(rownames(BLK_Params)[1]," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 7 values in a short parameter line, I think this should have been a block pattern parameter???")
        return(NULL)
      }
    }
  }

  ControlFile$MG_Params_TV$BLK_Params<-BLK_Params

  ControlFile$MG_Params_TV$Seasonality<-0
  if(length(ctlVals[[readLine]])==10)
  {
    ControlFile$MG_Params_TV$Seasonality<-ctlVals[[readLine]]
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Seasonality of parameter values = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Seasonality fo parameters should be defined by a vector of 10 values")
    return(NULL)
  }


  if(length(ControlFile$MG_Params_TV$Seasonality[ControlFile$MG_Params_TV$Seasonality!=0])>0)
  {
    Seas_Params<-MG_Params[1:(data_input$nseas*length(ControlFile$MG_Params_TV$Seasonality[ControlFile$MG_Params_TV$Seasonality!=0])),7,drop=FALSE]
    for(i in 1:length(data_input$nseas*ControlFile$MG_Params_TV$Seasonality[ControlFile$MG_Params_TV$Seasonality!=0]))
    {
      if(length(ctlVals[[readLine]])==7)
      {
        Seas_Params[i,]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Seasonal Parameter ",i," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 7 values in a short parameter line, I think this should have been a seasonal parameter???")
        return(NULL)
      }
    }
    ControlFile$MG_Params_TV$Seas_Params<-Seas_Params
  }


  DEV_Params<-MG_Params[MG_Params[,9]!=0,,drop=FALSE]

  if(length(DEV_Params[,13])>0)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$MG_Params_TV$Dev_Phase<-ctlVals[[readLine]]
      print(paste("Custom time varying parameter deviation start phase = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Parameter deviation start phase should be a single value")
      return(NULL)
    }
  }

  #Begin reading the spawner recruitment section

  print(ctlVals[[readLine]])
  if(length(ctlVals[[readLine]])==1 && ctlVals[[readLine]]>0 && ctlVals[[readLine]]<=7)
  {
    ControlFile$SR<-ctlVals[[readLine]]
    print(paste("The chosen stock recruitment relationship = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: The choice of stock recruitment relationship should be a single integer value between 1 and 7")
    return(NULL)
  }

  #Read stock recruit short parameter lines

  SR_Params<-matrix(nrow=0,ncol=7)
  colnames(SR_Params)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase")
  if(length(ctlVals[[readLine]])==7)
  {
    SR_Params<-rbind(SR_Params,ctlVals[[readLine]])
    rownames(SR_Params)[length(SR_Params[,1])]<-"logRO"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("The log of virgin recruitment level = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: The log of virgin recruitment level should be a short parameter line")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==7)
  {
    SR_Params<-rbind(SR_Params,ctlVals[[readLine]])
    rownames(SR_Params)[length(SR_Params[,1])]<-"h"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Steepness of the S-R relationship = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Steepness should be a short parameter line")
    return(NULL)
  }

  if(ControlFile$SR==5 || ControlFile$SR==7)
  {
    if(length(ctlVals[[readLine]])==7)
    {
      SR_Params<-rbind(SR_Params,ctlVals[[readLine]])
      rownames(SR_Params)[length(SR_Params[,1])]<-"3rdPar"
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      ControlFile$Par3<-ctlVals[[readLine]]
      print(paste("Optional third parameter = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Optional third recruitment parameter should be a short parameter line")
      return(NULL)
    }
  }

  if(length(ctlVals[[readLine]])==7)
  {
    SR_Params<-rbind(SR_Params,ctlVals[[readLine]])
    rownames(SR_Params)[length(SR_Params[,1])]<-"SigmaR"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Standard deviation of log recruitment = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Standard deviation of log recruitment should be a short parameter line")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==7)
  {
    SR_Params<-rbind(SR_Params,ctlVals[[readLine]])
    rownames(SR_Params)[length(SR_Params[,1])]<-"SR_EnvLink"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Stock recruitment environmental linkage = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment environmental linkage should be a short parameter line")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==7)
  {
    SR_Params<-rbind(SR_Params,ctlVals[[readLine]])
    rownames(SR_Params)[length(SR_Params[,1])]<-"logR1"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Stock recruitment R1 offset = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment R1 offset should be a short parameter line")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==7)
  {
    SR_Params<-rbind(SR_Params,ctlVals[[readLine]])
    rownames(SR_Params)[length(SR_Params[,1])]<-"AutoCor"
    outText<-paste(ctlVals[[readLine]],collapse = " ")
    print(paste("Autocorrelation in recruitment = ",outText,sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Autocorrelation in recruitment should be a short parameter line")
    return(NULL)
  }

  ControlFile$SR_Params<-SR_Params
  #Read additional stock recruitment conditions

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SR_EnvLnk<-ctlVals[[readLine]]
    print(paste("Stock recruitment environmental link variable = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment environmental link variable should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SR_EnvTrg<-ctlVals[[readLine]]
    print(paste("Stock recruitment environmental target variable = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment environmental target variable should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SR_DoDev<-ctlVals[[readLine]]
    print(paste("Stock recruitment deviations = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment deviations should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SR_BeginYr<-ctlVals[[readLine]]
    print(paste("Stock recruitment deviations begining year = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment deviations begining year should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SR_EndYr<-ctlVals[[readLine]]
    print(paste("Stock recruitment deviations end year = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment deviations end year should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SR_Phase<-ctlVals[[readLine]]
    print(paste("Stock recruitment deviations phase = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment deviations phase should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SR_AdvOpt<-ctlVals[[readLine]]
    print(paste("Stock recruitment advanced options = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Stock recruitment advanced options should be a single value")
    return(NULL)
  }

  #Begin reading advanced stock recruitement options
  if(ControlFile$SR_AdvOpt==1)
  {
    AdvOpts<-list()
    for(i in 1:13)
    {
      if(length(ctlVals[[readLine]])==1)
      {
        AdvOpts[[i]]<-ctlVals[[readLine]]
        print(paste("Advanced options ",i," = ",ctlVals[[readLine]],sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: Advanced options should be a single value")
        return(NULL)
      }
    }
    ControlFile$AdvOpts<-AdvOpts


    if(ControlFile$AdvOpts[[10]]!=0)
    {
      RecCycl<-matrix(nrow=ControlFile$AdvOpts[[10]],ncol=14)
      colnames(RecCycl)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase","UseEnv","UseDev","DevMinYr","DevMaxYr","DevStDev","UseBlock","BlockType")
      for(i in 1:ControlFile$AdvOpts[[10]])
      {
        if(length(ctlVals[[readLine]])==14)
        {
          RecCycl[i,]<-ctlVals[[readLine]]
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Recruitment cycle period ",i," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: Recruitment cycle should be a full parameter line")
          return(NULL)
        }
      }
      ControlFile$RecCycl<-RecCycl
    }

    if(ControlFile$AdvOpts[[13]]!=0)
    {
      RecDevs<-list()
      for(i in 1:ControlFile$AdvOpts[[13]])
      {
        if(length(ctlVals[[readLine]])==2)
        {
          RecDevs[[i]]<-ctlVals[[readLine]]
          print(paste("Recruitment deviation ",i," = ",ctlVals[[readLine]][1]," ",ctlVals[[readLine]][2],sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: Recruitment deviation should be two values a year and a deviation")
          return(NULL)
        }
      }
      ControlFile$RecDevs<-RecDevs
    }
  }
  #Now move on to reading in Fishing mortality method parameters and controls


  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$F_Ball<-ctlVals[[readLine]]
    print(paste("F ballpark estimate = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: F ballpark estimate should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$F_BallYr<-ctlVals[[readLine]]
    print(paste("F ballpark estimate reference year = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: F ballpark estimate reference year should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$F_Method<-ctlVals[[readLine]]
    print(paste("F method = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: F method should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$F_Max<-ctlVals[[readLine]]
    print(paste("F maximum = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: F maximum should be a single value")
    return(NULL)
  }

  if(ControlFile$F_Method==2)
  {
    if(length(ctlVals[[readLine]])==3)
    {
      ControlFile$F_Popes<-ctlVals[[readLine]]
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Popes F method params = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Popes F method params should be 3 values")
      return(NULL)
    }

    if(ControlFile$F_Popes[3]>0)
    {
      F_Detailed<-matrix(nrow=0,ncol=6)
      colnames(F_Detailed)<-c("fleet","yr","seas","F","se","Phase")
      for(i in 1:ControlFile$F_Popes[3])
      {
        if(length(ctlVals[[readLine]])==6)
        {
          F_Detailed<-rbind(F_Detailed,ctlVals[[readLine]])
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Detailed F input ",i," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: There should be 6 values in a line of detailed F input???")
          return(NULL)
        }
      }
      ControlFile$F_Detailed<-F_Detailed
    }
  }

  if(ControlFile$F_Method==3)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$F_TunIts<-ctlVals[[readLine]]
      print(paste("Number of tuning iterations = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Number of tuning iterations should be a single value")
      return(NULL)
    }
  }

  #Read in Initial Fishing mortality parameter lines
  F_init<-matrix(nrow=0,ncol=7)
  colnames(F_init)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase")

  for(i in 1:data_input$Nfleet)
  {
    if(length(ctlVals[[readLine]])==7)
    {
      F_init<-rbind(F_init,ctlVals[[readLine]])
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Initial F for fleet ",i," = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 7 values in a short parameter line, I think this should have been initial F???")
      return(NULL)
    }
  }
  ControlFile$F_init<-F_init

  #Now read in the Catchability parameters for all fleets and surveys
  Q<-matrix(nrow=(data_input$Nfleet+data_input$Nsurveys),ncol=4)
  colnames(Q)<-c("doPower","EnvVar","extraSD","devType")
  for(i in 1:(data_input$Nfleet+data_input$Nsurveys))
  {
    if(length(ctlVals[[readLine]])==4)
    {
      Q[i,]<-ctlVals[[readLine]]
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Catchability for fleet/Survey ",i," = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 4 values in this parameter line, I think this should have been a fleet/Survey Q???")
      return(NULL)
    }
  }
  ControlFile$Q<-Q

  QNrand<-Q[Q[,4]>=3,,drop=FALSE]
  if(length(QNrand[,4])>0)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$Q_Cust<-ctlVals[[readLine]]
      print(paste("Custom random Q = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Custom random Q should be a single value")
      return(NULL)
    }
  }

  if(max(Q)>0)
  {
    Qrand<-matrix(nrow=0,ncol=7)
    colnames(Qrand)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase")
    r<-1
    while(length(ctlVals[[readLine]])==7)
    {
      Qrand<-rbind(Qrand,ctlVals[[readLine]])
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Catchability parameters ",r," = ",outText,sep=""))
      readLine<-readLine+1
      r<-r+1
    }
    ControlFile$Qrand<-Qrand
  }


  Size_Select<-matrix(nrow=(data_input$Nfleet+data_input$Nsurveys),ncol=4)
  colnames(Size_Select)<-c("Pattern","Discard","Male","Special")
  for(i in 1:(data_input$Nfleet+data_input$Nsurveys))
  {
    if(length(ctlVals[[readLine]])==4)
    {
      Size_Select[i,]<-ctlVals[[readLine]]
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Size selectivity for fleet/Survey ",i," = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 4 values in this parameter line, I think this should have been fleet/Survey size selectivity???")
      return(NULL)
    }
  }
  ControlFile$Size_Select<-Size_Select

  Age_Select<-matrix(nrow=(data_input$Nfleet+data_input$Nsurveys),ncol=4)
  colnames(Age_Select)<-c("Pattern","Discard","Male","Special")
  for(i in 1:(data_input$Nfleet+data_input$Nsurveys))
  {
    if(length(ctlVals[[readLine]])==4)
    {
      Age_Select[i,]<-ctlVals[[readLine]]
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Age selectivity for fleet/Survey ",i," = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 4 values in this parameter line, I think this should have been fleet/Survey age selectivity???")
      return(NULL)
    }
  }
  ControlFile$Age_Select<-Age_Select

  #Read in the selectivity parameter table

  pattern<-matrix(nrow=34,ncol=2)
  pattern[,1]<-c(2,8,6,0,2,2,8,8,6,0,2,2,8,(data_input$Nages+1),0,2,(data_input$Nages+1),8,6,6,0,4,6,6,3,3,3,0,0,0,0,0,0,0)
  pattern[,2]<-c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0)

  Select_Params<-matrix(nrow=0,ncol=14)
  colnames(Select_Params)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase","UseEnv","UseDev","DevMinYr","DevMaxYr","DevStDev","UseBlock","BlockType")

  for(i in 1:length(Size_Select[,1]))
  {
    if(Size_Select[i,1]!=0)
    {
      if(pattern[Size_Select[i,1],1]!=0)
      {
        nParams<-pattern[Size_Select[i,1],1]+pattern[Size_Select[i,1],2]*Size_Select[i,4]
        for(j in 1:nParams)
        {
          if(length(ctlVals[[readLine]])==14)
          {
            Select_Params<-rbind(Select_Params,ctlVals[[readLine]])
            rownames(Select_Params)[length(Select_Params[,1])]<-paste("Size Select ",j," fleet/Survey ",i)
            outText<-paste(ctlVals[[readLine]],collapse = " ")
            print(paste("Size Selectivity parameter ",j," for fleet/survey ",i," = ",outText,sep=""))
            readLine<-readLine+1
          }else
          {
            print("Error: There should be 14 values in a parameter line, I think this should have been a Size Selectivity parameter???")
            return(NULL)
          }
        }
      }
    }

    if(i<=data_input$Nfleet)
    {
      if(Size_Select[i,2]==1 | Size_Select[i,2]==2)
      {
        for(j in 1:4)
        {
          if(length(ctlVals[[readLine]])==14)
          {
            Select_Params<-rbind(Select_Params,ctlVals[[readLine]])
            rownames(Select_Params)[length(Select_Params[,1])]<-paste("Size Retention ",j," fleet/Survey ",i)
            outText<-paste(ctlVals[[readLine]],collapse = " ")
            print(paste("Size Retention parameter ",j," for fleet/survey ",i," = ",outText,sep=""))
            readLine<-readLine+1
          }else
          {
            print("Error: There should be 14 values in a parameter line, I think this should have been a Size Retention parameter???")
            return(NULL)
          }
        }
      }

      if(Size_Select[i,2]==2)
      {
        for(j in 1:4)
        {
          if(length(ctlVals[[readLine]])==14)
          {
            Select_Params<-rbind(Select_Params,ctlVals[[readLine]])
            rownames(Select_Params)[length(Select_Params[,1])]<-paste("Size Discard ",j," fleet/Survey ",i)
            outText<-paste(ctlVals[[readLine]],collapse = " ")
            print(paste("Size Discard parameter ",j," for fleet/survey ",i," = ",outText,sep=""))
            readLine<-readLine+1
          }else
          {
            print("Error: There should be 14 values in a parameter line, I think this should have been a Size discard parameter???")
            return(NULL)
          }
        }
      }
    }
    if(Size_Select[i,3]>0)
    {
      if(Size_Select[i,3]<3){nSSpar<-4}else{if(Size_Select[i,1]==1){nSSpar<-3}else{nSSpar<-5}}

      for(j in 1:nSSpar)
      {
        if(length(ctlVals[[readLine]])==14)
        {
          Select_Params<-rbind(Select_Params,ctlVals[[readLine]])
          rownames(Select_Params)[length(Select_Params[,1])]<-paste("Size SexOffset ",j," fleet/Survey ",i)
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Size SexOffset parameter ",j," for fleet/survey ",i," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: There should be 14 values in a parameter line, I think this should have been a Size SexOffset parameter???")
          return(NULL)
        }
      }
    }
  }

  for(i in 1:length(Age_Select[,1]))
  {
    if(Age_Select[i,1]!=0)
    {
      if(pattern[Age_Select[i,1],1]!=0)
      {
        nParams<-pattern[Age_Select[i,1],1]+pattern[Age_Select[i,1],2]*Age_Select[i,4]
        for(j in 1:nParams)
        {
          if(length(ctlVals[[readLine]])==14)
          {
            Select_Params<-rbind(Select_Params,ctlVals[[readLine]])
            rownames(Select_Params)[length(Select_Params[,1])]<-paste("Age Select ",j," fleet/Survey ",i)
            outText<-paste(ctlVals[[readLine]],collapse = " ")
            print(paste("Age Selectivity parameter ",j," for fleet/survey ",i," = ",outText,sep=""))
            readLine<-readLine+1
          }else
          {
            print("Error: There should be 14 values in a parameter line, I think this should have been a Age Selectivity parameter???")
            return(NULL)
          }
        }
      }
    }
    if(Age_Select[i,3]>0)
    {
      if(Age_Select[i,3]<3){nSSpar<-4}else{if(Age_Select[i,1]==1){nSSpar<-3}else{nSSpar<-5}}

      for(j in 1:nSSpar)
      {
        if(length(ctlVals[[readLine]])==14)
        {
          Select_Params<-rbind(Select_Params,ctlVals[[readLine]])
          rownames(Select_Params)[length(Select_Params[,1])]<-paste("Age SexOffset ",j," fleet/Survey ",i)
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste("Age SexOffset parameter ",j," for fleet/survey ",i," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: There should be 14 values in a parameter line, I think this should have been a Age SexOffset parameter???")
          return(NULL)
        }
      }
    }
  }

  ControlFile$Select_Params<-Select_Params

  #Now read time varying parameters for selectivity and retention


  ControlFile$Select_Params_TV<-list()

  ENV_Params<-Select_Params[Select_Params[,8]!=0,,drop=FALSE]

  if(length(ENV_Params[,8])>0)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$Select_Params_TV$Cust_ENV<-ctlVals[[readLine]]
      print(paste("Custom time varying parameters with environmental linkages = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: The allocation of custom environmental linkages should be a single value")
      return(NULL)
    }

    if(ControlFile$Select_Params_TV$Cust_ENV==1)
    {
      ENV_Params<-ENV_Params[,1:7]
      for(i in 1:length(ENV_Params[,7]))
      {
        if(length(ctlVals[[readLine]])==7)
        {
          ENV_Params[i,]<-ctlVals[[readLine]]
          outText<-paste(ctlVals[[readLine]],collapse = " ")
          print(paste(rownames(ENV_Params)[i]," = ",outText,sep=""))
          readLine<-readLine+1
        }else
        {
          print("Error: There should be 7 values in a short parameter line, I think this should have been an environmental linkage parameter???")
          return(NULL)
        }
      }
    }else
    {
      ENV_Params<-ENV_Params[1,1:7,drop=FALSE]
      if(length(ctlVals[[readLine]])==7)
      {
        ENV_Params[1,]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste(rownames(ENV_Params)[1]," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 7 values in a short parameter line, I think this should have been an environmental linkage parameter???")
        return(NULL)
      }
    }
  }

  ControlFile$Select_Params_TV$ENV_Params<-ENV_Params

  BLK_ParamsTV<-Select_Params[Select_Params[,13]!=0,,drop=FALSE]

  BLK_Params<-matrix(nrow=0,ncol=7)

  colnames(BLK_Params)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase")

  if(length(BLK_ParamsTV[,13])>0)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$Select_Params_TV$Cust_Blk<-ctlVals[[readLine]]
      print(paste("Custom time varying parameters with block patterns = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: The allocation of custom block patterns should be a single value")
      return(NULL)
    }

    if(ControlFile$Select_Params_TV$Cust_Blk==1)
    {
      for(i in 1:length(BLK_ParamsTV[,13]))
      {
        if(BLK_ParamsTV[i,13]>0)
        {
          if(BLK_ParamsTV[i,13]<=ControlFile$Num_BlockPatterns)
          {
            for(j in 1:ControlFile$BlocksPerPattern[BLK_ParamsTV[i,13]])
            {
              if(length(ctlVals[[readLine]])==7)
              {
                BLK_Params<-rbind(BLK_Params,ctlVals[[readLine]])
                rownames(BLK_Params)[length(BLK_Params[,1])]<-paste("TV_Param",i,"_Block",j)
                outText<-paste(ctlVals[[readLine]],collapse = " ")
                print(paste(rownames(BLK_Params)[length(rownames(BLK_Params))]," = ",outText,sep=""))
                readLine<-readLine+1
              }else
              {
                print("Error: There should be 7 values in a short parameter line, I think this should have been a block pattern parameter???")
                return(NULL)
              }
            }
          }else
          {
            print("Error: I think this is a block pattern parameter but you are referencing a pattern higher than the number of declared patterns???")
            return(NULL)
          }
        }else
        {
          for(j in 1:3)
          {
            if(length(ctlVals[[readLine]])==7)
            {
              BLK_Params<-rbind(BLK_Params,ctlVals[[readLine]])
              rownames(BLK_Params)[length(BLK_Params[,1])]<-paste("TV_Param",i,"_Block",j)
              outText<-paste(ctlVals[[readLine]],collapse = " ")
              print(paste(rownames(BLK_Params)[length(rownames(BLK_Params))]," = ",outText,sep=""))
              readLine<-readLine+1
            }else
            {
              print("Error: There should be 7 values in a short parameter line, I think this should have been a block pattern parameter???")
              return(NULL)
            }
          }
        }
      }
    }else
    {
      BLK_Params<-BLK_Params[1,1:7,drop=FALSE]
      if(length(ctlVals[[readLine]])==7)
      {
        BLK_Params[1,]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste(rownames(BLK_Params)[1]," = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 7 values in a short parameter line, I think this should have been a block pattern parameter???")
        return(NULL)
      }
    }
  }

  ControlFile$Select_Params_TV$BLK_Params<-BLK_Params

  DEV_Params<-Select_Params[Select_Params[,9]!=0,,drop=FALSE]

  if(length(DEV_Params[,13])>0)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$Select_Params_TV$Dev_Phase<-ctlVals[[readLine]]
      print(paste("Custom time varying parameter deviation start phase = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Parameter deviation start phase should be a single value")
      return(NULL)
    }
  }


  if((length(ENV_Params[,1])+length(BLK_ParamsTV[,1])+length(DEV_Params[,1]))>0)
  {
    if(length(ctlVals[[readLine]])==1)
    {
      ControlFile$Select_Params_TV$Adj_Meth<-ctlVals[[readLine]]
      print(paste("Custom time varying parameter adjustment method = ",ctlVals[[readLine]],sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: Parameter adjustment method should be a single value")
      return(NULL)
    }
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Read_Tag<-ctlVals[[readLine]]
    print(paste("Read tag recapture parameters = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Decision to read tag recapture parameters should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==14)
  {
    TagRecap_Params<-matrix(nrow=0,ncol=14)
    colnames(TagRecap_Params)<-c("Low","Hi","Init","PriorValue","PriorType","PriorStDev","Phase","UseEnv","UseDev","DevMinYr","DevMaxYr","DevStDev","UseBlock","BlockType")

    while(length(ctlVals[[readLine]])==14)
    {
      if(length(ctlVals[[readLine]])==14)
      {
        TagRecap_Params<-rbind(TagRecap_Params,ctlVals[[readLine]])
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Ttag recapture parameter = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 14 values in a parameter line, I think this should have been a tag recapture parameter???")
        return(NULL)
      }
    }
    ControlFile$TagRecap_Params<-TagRecap_Params
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Var_adj<-ctlVals[[readLine]]
    print(paste("Read variance adjustment factors = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Read variance adjustment factors should be a single value")
    return(NULL)
  }

  if(ControlFile$Var_adj==1)
  {
    Var_Adj_Fact<-matrix(nrow=6,ncol=(data_input$Nfleet+data_input$Nsurveys))

    for(i in 1:6)
    {
      if(length(ctlVals[[readLine]])==(data_input$Nfleet+data_input$Nsurveys))
      {
        Var_Adj_Fact[i,]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Variance adjustment factor = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be Nfleet + NSurvey values in an adjustment factor line???")
        return(NULL)
      }
    }
    ControlFile$Var_Adj_Fact<-Var_Adj_Fact
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Max_Lambda<-ctlVals[[readLine]]
    print(paste("Max lambda phase = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Max lambda phase should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$SD_offset<-ctlVals[[readLine]]
    print(paste("sd offset = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: sd offset should be a single value")
    return(NULL)
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Num_changes<-ctlVals[[readLine]]
    print(paste("number of changes to make to default lambdas = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: number of changes to make to default lambdas should be a single value")
    return(NULL)
  }

  if(ControlFile$Num_changes>0)
  {
    Lambda_Changes<-matrix(nrow=ControlFile$Num_changes,ncol=5)

    for(i in 1:ControlFile$Num_changes)
    {
      if(length(ctlVals[[readLine]])==5)
      {
        Lambda_Changes[i,]<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("Lambda change info = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: There should be 5 values in a lambda change info line???")
        return(NULL)
      }
    }
    ControlFile$Lambda_Changes<-Lambda_Changes
  }

  if(length(ctlVals[[readLine]])==1)
  {
    ControlFile$Extra<-ctlVals[[readLine]]
    print(paste("Get extra variance estimates for one selectivity pattern = ",ctlVals[[readLine]],sep=""))
    readLine<-readLine+1
  }else
  {
    print("Error: Getting extra variance estimate should be a single value")
    return(NULL)
  }

  if(ControlFile$Extra==1)
  {
    if(length(ctlVals[[readLine]])==9)
    {
      ControlFile$Extra_control<-ctlVals[[readLine]]
      outText<-paste(ctlVals[[readLine]],collapse = " ")
      print(paste("Control values for extra variance estimate = ",outText,sep=""))
      readLine<-readLine+1
    }else
    {
      print("Error: There should be 9 Control values for the extra variance estimate???")
      return(NULL)
    }

    if(ControlFile$Extra_control[4]>0)
    {
      if(length(ctlVals[[readLine]])==ControlFile$Extra_control[4])
      {
        ControlFile$Extra_selex<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("selex bins = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: wrong number of selex bins???")
        return(NULL)
      }
    }

    if(ControlFile$Extra_control[6]>0)
    {
      if(length(ctlVals[[readLine]])==ControlFile$Extra_control[6])
      {
        ControlFile$Extra_growth<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("growth bins = ",outText,sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: wrong number of growth bins???")
        return(NULL)
      }
    }

    if(ControlFile$Extra_control[9]>0)
    {
      if(length(ctlVals[[readLine]])==ControlFile$Extra_control[9])
      {
        ControlFile$Extra_NatAge<-ctlVals[[readLine]]
        outText<-paste(ctlVals[[readLine]],collapse = " ")
        print(paste("NatAge bins = ",ctlVals[[readLine]],sep=""))
        readLine<-readLine+1
      }else
      {
        print("Error: wrong number of NatAge bins???")
        return(NULL)
      }
    }
  }

  if(ctlVals[[readLine]]==999)
  {
    ControlFile$End<-ctlVals[[readLine]]
    print(paste("Congratulations this should be a working control file = ",ctlVals[[readLine]],sep=""))
    return(ControlFile)
  }else
  {
    print("Error: Something went wrong, this should be the end of the file and read 999")
    return(NULL)
  }
}

#' read forecast file
#'
#' read Stock Synthesis forecast file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param Nfleets Number of fleets in the assessment.
#' @param Nareas Number of areas in the assessment.
#' @param Nseas Number of seasons in the assessment.
#' @param verbose Should there be verbose output while running the file?
#' @author Ian Taylor
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_ctl}},
#' \code{\link{rd_data}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data import
rd_forecast <- function(file='forecast.ss', Nfleets, Nareas, Nseas, verbose=FALSE){
  # function to read Stock Synthesis forecast files
  if(verbose) cat("running SS_readsforecast\n")
  forecast <- readLines(file,warn=F)
  mylist <- list()

  mylist$sourcefile <- file
  mylist$type <- "Stock_Synthesis_forecast_file"
  mylist$SSversion <- "SSv3.21_or_later"

  # get numbers (could be better integrated with function above)
  allnums <- NULL
  for(i in 1:length(forecast)){
    # split apart numbers from text in file
    mysplit <- strsplit(forecast[i],split="[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit!=""]
    nums <- suppressWarnings(as.numeric(mysplit))
    if(sum(is.na(nums)) > 0) maxcol <- min((1:length(nums))[is.na(nums)])-1
    else maxcol <- length(nums)
    if(maxcol > 0){
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }

  # go through numerical values and save as elements of a big list
  i <- 1
  print(allnums[i])
  mylist$benchmarks <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$MSY <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$SPRtarget <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Btarget <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Bmark_years <- allnums[i:(i+5)]; i <- i+6
  print(allnums[i])
  mylist$Bmark_relF_Basis <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Forecast <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Nforecastyrs <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$F_scalar <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Fcast_years <- allnums[i:(i+3)]; i <- i+4
  print(allnums[i])
  mylist$ControlRuleMethod <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$BforconstantF <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$BfornoF <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Flimitfraction <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$N_forecast_loops <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$First_forecast_loop_with_stochastic_recruitment <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Forecast_loop_control_3 <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Forecast_loop_control_4 <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Forecast_loop_control_5 <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$FirstYear_for_caps_and_allocations <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$stddev_of_log_catch_ratio <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Do_West_Coast_gfish_rebuilder_output <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Ydecl <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$Yinit <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$fleet_relative_F <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$basis_for_fcast_catch_tuning <- allnums[i]; i <- i+1
  if(mylist$fleet_relative_F==2){
    rel_F <- data.frame(matrix(
      allnums[i:(i+Nfleets*Nseas-1)],nrow=Nseas,ncol=Nfleets,byrow=TRUE))
    names(rel_F) <- paste0("Fleet ",1:Nfleets)
    i <- i+Nfleets*Nseas

    if(verbose){
      cat("  Relative F inputs by fleet and season \n")
      print(rel_F)
    }
  }else{
    rel_F<-NULL
  }
  print(rel_F)
  mylist$rel_F <- rel_F
  print(allnums[i:(i+Nfleets-1)])
  mylist$max_totalcatch_by_fleet <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
  if(verbose) cat("  max_totalcatch_by_fleet =",mylist$max_totalcatch_by_fleet,"\n")
  print(allnums[i:(i+Nareas-1)])
  mylist$max_totalcatch_by_area <- allnums[i:(i+Nareas-1)]; i <- i+Nareas
  if(verbose) cat("  max_totalcatch_by_area =",mylist$max_totalcatch_by_area,"\n")
  print(allnums[i:(i+Nfleets-1)])
  mylist$fleet_assignment_to_allocation_group <- allnums[i:(i+Nfleets-1)]; i <- i+Nfleets
  # allocation groups
  if(verbose) cat("  fleet_assignment_to_allocation_group =",mylist$fleet_assignment_to_allocation_group,"\n")
  if(any(mylist$fleet_assignment_to_allocation_group!=0)){
    mylist$N_allocation_groups <- max(mylist$fleet_assignment_to_allocation_group)
    mylist$allocation_among_groups <- allnums[i:(i+mylist$N_allocation_groups-1)]; i <- i+mylist$N_allocation_groups
  }else{
    mylist$N_allocation_groups <- 0
    mylist$allocation_among_groups <- NULL
  }
  print(mylist$N_allocation_groups)
  print(mylist$allocation_among_groups)
  print(allnums[i])
  mylist$Ncatch <- Ncatch <- allnums[i]; i <- i+1
  print(allnums[i])
  mylist$InputBasis <- InputBasis <- allnums[i]; i <- i+1
  # forcast catch levels
  if(Ncatch==0){
    ForeCatch <- NULL
  }else if(InputBasis==-1){
    ForeCatch <- data.frame(matrix(
      allnums[i:(i+Ncatch*5-1)],nrow=Ncatch,ncol=5,byrow=TRUE))
    i <- i+Ncatch*5
    names(ForeCatch) <- c("Year","Seas","Fleet","Catch_or_F","Basis")
    if(verbose){
      cat("  Catch inputs (Ncatch =",Ncatch,"\n")
      print(ForeCatch)
    }
  }else{
    ForeCatch <- data.frame(matrix(
      allnums[i:(i+Ncatch*4-1)],nrow=Ncatch,ncol=4,byrow=TRUE))
    i <- i+Ncatch*4
    ForeCatch <- cbind(ForeCatch,rep(InputBasis,length(ForeCatch[,1])))
    mylist$InputBasis <- InputBasis <- -1
    names(ForeCatch) <- c("Year","Seas","Fleet","Catch_or_F","Basis")
    if(verbose){
      cat("  Catch inputs (Ncatch =",Ncatch,"\n")
      print(ForeCatch)
    }
  }
  print(ForeCatch)
  mylist$ForeCatch <- ForeCatch
  # check final value
  if(allnums[i]==999){
    if(verbose) cat("read of forecast file complete (final value = 999)\n")
  }else{
    cat("Error: final value is", allnums[i]," but should be 999\n")
  }
  #close(con)
  # all done
  return(mylist)
}

#' read parameter file
#'
#' read Stock Synthesis parameter file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @author Nathan Vaughan
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_ctl}}, \code{\link{rd_forecast}},
#' \code{\link{rd_data}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data import
rd_par <- function(file){
  SS3par <- readLines(con=file,warn=FALSE)
  SplitPar<-strsplit(SS3par,"\\s")
  SS3parLabels<-list()
  SS3parValues<-list()

  for(i in 1:length(SS3par))
  {
    temp<-SplitPar[[i]]
    temp<-temp[temp!=""]
    if(temp[1]=="#")
    {
      SS3parLabels[[length(SS3parLabels)+1]]<-SS3par[i]
    }else{
      SS3parValues[[length(SS3parValues)+1]]<-as.numeric(temp)
    }
  }
  SS3pars<-list()
  SS3pars$Labels<-SS3parLabels
  SS3pars$Values<-SS3parValues
  #close(con)
  return(SS3pars)
}

#' read display details file
#'
#' read display details file into list object in R
#'
#'
#' @param file Filename either with full path or relative to working directory.
#' @param forecast.internal List object created by \code{\link{rd_forecast}}.
#' @param data.internal List object created by \code{\link{rd_data}}.
#' @author Nathan Vaughan
#' @seealso \code{\link{rd_starter}}, \code{\link{rd_ctl}}, \code{\link{rd_forecast}},
#' \code{\link{rd_data}}, \code{\link{wrt_starter}},
#' \code{\link{wrt_forecast}}, \code{\link{wrt_data}},
#' \code{\link{wrt_ctl}}
#' @keywords data import
rd_display <- function(file,forecast.internal=forecast.orig, data.internal=data.orig){
  dDs <- readLines(con=file,warn=FALSE)

  DisplayVals<-list()
  DisplayVals$type<-"DST_Display_Descriptions"

  dDComs<-dDVals<-dD2<-strsplit(dDs,"#")
  dDs<-NULL
  for(i in 1:length(dD2))
  {
    dDComs[[i]]<-dD2[[i]][-1]
    dDVals[[i]]<-dD2[[i]][1]
  }
  dDVals<-dDVals[lengths(dDVals)>0]
  dDVals<-dDVals[!is.na(dDVals)]
  dDVals<-dDVals[dDVals!=""]

  readLine<-1

  DisplayVals$AssesDesc<-dDVals[[readLine]]
  print(paste("Assessment Description = ",dDVals[[readLine]],sep=""))
  readLine<-readLine+1

  DisplayVals$Title<-dDVals[[readLine]]
  print(paste("Assessment Title = ",dDVals[[readLine]],sep=""))
  readLine<-readLine+1

  DisplayVals$GroupNames<-matrix(NA,nrow=length(unique(forecast.internal$fleet_assignment_to_allocation_group)),ncol=3)
  for(i in 1:length(unique(forecast.internal$fleet_assignment_to_allocation_group)))
  {
    tmp<-unlist(strsplit(dDVals[[readLine]],","))
    DisplayVals$GroupNames[i,1]<-as.numeric(tmp[1])
    DisplayVals$GroupNames[i,2]<-tmp[2]
    DisplayVals$GroupNames[i,3]<-paste(tmp[3:length(tmp)],collapse=",")
    readLine<-readLine+1
  }

  DisplayVals$FleetNames<-matrix(NA,nrow=length(forecast.internal$fleet_assignment_to_allocation_group),ncol=3)
  for(i in 1:length(forecast.internal$fleet_assignment_to_allocation_group))
  {
    tmp<-unlist(strsplit(dDVals[[readLine]],","))
    DisplayVals$FleetNames[i,1]<-as.numeric(tmp[1])
    DisplayVals$FleetNames[i,2]<-tmp[2]
    DisplayVals$FleetNames[i,3]<-paste(tmp[3:length(tmp)],collapse=",")
    readLine<-readLine+1
  }

  DisplayVals$SeasonNames<-matrix(NA,nrow=data.internal$nseas,ncol=3)
  for(i in 1:data.internal$nseas)
  {
    tmp<-unlist(strsplit(dDVals[[readLine]],","))
    DisplayVals$SeasonNames[i,1]<-as.numeric(tmp[1])
    DisplayVals$SeasonNames[i,2]<-tmp[2]
    DisplayVals$SeasonNames[i,3]<-paste(tmp[3:length(tmp)],collapse=",")
    readLine<-readLine+1
  }

  DisplayVals$AreaNames<-matrix(NA,nrow=data.internal$N_areas,ncol=3)
  for(i in 1:data.internal$N_areas)
  {
    tmp<-unlist(strsplit(dDVals[[readLine]],","))
    DisplayVals$AreaNames[i,1]<-as.numeric(tmp[1])
    DisplayVals$AreaNames[i,2]<-tmp[2]
    DisplayVals$AreaNames[i,3]<-paste(tmp[3:length(tmp)],collapse=",")
    readLine<-readLine+1
  }

  DisplayVals$TargetYears<-vector(length=2)
  DisplayVals$TargetYears[1]<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$TargetYears[2]<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$ABCFrac<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$FleetRel<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$ImplRebuild<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$RebuildYears<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$RebuildFrac<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$ConstCatch<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$Units<-vector(length=2)
  DisplayVals$Units[1]<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  DisplayVals$Units[2]<-as.numeric(dDVals[[readLine]])
  readLine<-readLine+1
  return(DisplayVals)
}


#' A function to create a list object for the output from Stock Synthesis
#'
#' Reads the Report.sso and (optionally) the covar.sso, CompReport.sso and
#' other files files produced by Stock Synthesis and formats the important
#' content of these files into a list in the R workspace. A few statistics
#' unavailable elsewhere are taken from the .par and .cor files. Summary
#' information and statistics can be returned to the R console or just
#' contained within the list produced by this function.
#'
#'
#' @param dir Locates the directory of the files to be read in, double
#' backslashes (or forwardslashes) and quotes necessary.
#' @param model Name of the executable (leaving off the .exe).  Deafult="ss3"
#' @param repfile Name of the big report file (could be renamed by user).
#' Default="Report.sso".
#' @param compfile Name of the composition report file.
#' Default="CompReport.sso".
#' @param covarfile Name of the covariance output file.  Default="covar.sso".
#' @param forefile Name of the forecast file.  Default="Forecast-report.sso".
#' @param wtfile Name of the file containing weight at age data.
#' Default="wtatage.ss_new".
#' @param ncols The maximum number of columns in files being read in.  If this
#' value is too big the function runs more slowly, too small and errors will
#' occur.  A warning will be output to the R command line if the value is too
#' small. It should be bigger than the maximum age + 10 and the number of years
#' + 10. Default=200.
#' @param forecast Read the forecast-report file? Default=TRUE.
#' @param warn Read the Warning.sso file? Default=TRUE.
#' @param covar Read covar.sso to get variance information and identify bad
#' correlations? Default=TRUE.
#' @param readwt Read the weight-at-age file? Default=TRUE.
#' @param checkcor Check for bad correlations? Default=TRUE.
#' @param cormax The specified threshold for defining high correlations.  A
#' quantity with any correlation above this value is identified.  Default=0.95.
#' @param cormin The specified threshold for defining low correlations.  Only
#' quantities with all correlations below this value are identified (to find
#' variables that appear too independent from the model results). Default=0.01.
#' @param printhighcor The maximum number of high correlations to print to the
#' R GUI. Default=10.
#' @param printlowcor The maximum number of low correlations to print to the R
#' GUI. Default=10.
#' @param verbose Return updates of function progress to the R GUI?
#' Default=TRUE.
#' @param printstats Print summary statistics about the output to the R GUI?
#' Default=TRUE.
#' @param hidewarn Hides some warnings output from the R GUI.  Default=FALSE.
#' @param NoCompOK Allow the function to work without a CompReport file.
#' Default=FALSE.
#' @param aalmaxbinrange The largest length bin range allowed for composition
#' data to be considered as conditional age-at-length data.  Default=4.
#' @return Many values are returned. Complete list would be quite long, but
#' should probably be created at some point in the future.
#' @author Ian Stewart, Ian Taylor
#' @keywords data manip list
#' @examples
#'
#'   \dontrun{
#'     myreplist <- SS_output(dir='c:/SS/SSv3.10b/Simple/')
#'   }
#'
rd_output <-
  function(dir, model="ss3",
           repfile="Report.sso", compfile="CompReport.sso",covarfile="covar.sso",
           forefile="Forecast-report.sso", wtfile="wtatage.ss_new",
           ncols=200, forecast=TRUE, warn=TRUE, covar=TRUE, readwt=TRUE,
           checkcor=TRUE, cormax=0.95, cormin=0.01, printhighcor=10, printlowcor=10,
           verbose=TRUE, printstats=TRUE,hidewarn=FALSE, NoCompOK=FALSE,
           aalmaxbinrange=4){
    ################################################################################
    #
    # SS_output
    # This function comes with no warranty or guarantee of accuracy
    #
    # Purpose: To import content from Stock Synthesis model run.
    # Written: Ian Stewart, NWFSC. Ian.Stewart-at-noaa.gov
    #          Ian Taylor, NWFSC/UW. Ian.Taylor-at-noaa.gov
    #          and other contributors to http://code.google.com/p/r4ss/
    # Returns: a list containing elements of Report.sso and/or covar.sso,
    #          formatted as R objects, and optional summary statistics to R console
    #
    ################################################################################

    flush.console()

    #################################################################################
    ## embedded functions: emptytest, matchfun and matchfun2
    #################################################################################

    emptytest <- function(x){ sum(!is.na(x) & x=="")/length(x) }

    matchfun <- function(string, obj=rawrep[,1], substr1=TRUE)
    {
      # return a line number from the report file (or other file)
      # sstr controls whether to compare subsets or the whole line
      match(string, if(substr1){substring(obj,1,nchar(string))}else{obj} )
    }

    matchfun2 <- function(string1,adjust1,string2,adjust2,cols="nonblank",matchcol1=1,matchcol2=1,
                          objmatch=rawrep,objsubset=rawrep,substr1=TRUE,substr2=TRUE,header=FALSE)
    {
      # return a subset of values from the report file (or other file)
      # subset is defined by character strings at the start and end, with integer
      # adjustments of the number of lines to above/below the two strings
      line1 <- match(string1,if(substr1){substring(objmatch[,matchcol1],1,nchar(string1))}else{objmatch[,matchcol1]})
      line2 <- match(string2,if(substr2){substring(objmatch[,matchcol2],1,nchar(string2))}else{objmatch[,matchcol2]})
      if(is.na(line1) | is.na(line2)) return("absent")

      if(is.numeric(cols))    out <- objsubset[(line1+adjust1):(line2+adjust2),cols]
      if(cols[1]=="all")      out <- objsubset[(line1+adjust1):(line2+adjust2),]
      if(cols[1]=="nonblank"){
        # returns only columns that contain at least one non-empty value
        out <- objsubset[(line1+adjust1):(line2+adjust2),]
        out <- out[,apply(out,2,emptytest) < 1]
      }
      if(header && nrow(out)>0){
        out[1,out[1,]==""] <- "NoName"
        names(out) <- out[1,]
        out <- out[-1,]
      }
      return(out)
    }

    # get info on output files created by Stock Synthesis
    shortrepfile <- repfile
    repfile <- file.path(dir,repfile)

    parfile <- dir(dir,pattern=".par$")
    if(length(parfile)>1){
      filetimes <- file.info(file.path(dir,parfile))$mtime
      parfile <- parfile[filetimes==max(filetimes)][1]
      if(verbose) cat("Multiple files in directory match pattern *.par\n",
                      "choosing most recently modified:",parfile,"\n")
    }
    if(length(parfile)==0){
      if(!hidewarn) cat("Some stats skipped because the .par file not found:\n  ",parfile,"\n")
      parfile <- NA
    }else{
      parfile <- file.path(dir,parfile)
    }

    # read three rows to get start time and version number from rep file
    if(file.exists(repfile)){
      if(file.info(repfile)$size>0){
        if(verbose) cat("Getting header info from:\n  ",repfile,"\n")
      }else{
        stop("report file is empty: ",repfile)
      }
    }else{
      stop("can't find report file: ",repfile)
    }
    rephead <- readLines(con=repfile,n=15)

    # warn if SS version used to create rep file is too old or too new for this code
    # note: SS_versionCode is new with V3.20
    # perhaps in the future we will use it to replace SS_versionshort throughout r4ss?
    SS_versionCode <- rephead[grep("#V",rephead)]
    SS_version <- rephead[grep("Stock_Synthesis",rephead)]
    SS_version <- SS_version[substring(SS_version,1,2)!="#C"] # remove any version numbering in the comments
    SS_versionshort <- toupper(substr(SS_version,1,8))
    SS_versionNumeric <- as.numeric(substring(SS_versionshort,5))
    # rough limits on compatibility of this code
    SS_versionMax <- 3.30
    SS_versionMin <- 3.21 # a stab in the dark at which versions still work

    # test for version compatibility with this code
    if(SS_versionNumeric < SS_versionMin  | SS_versionNumeric > SS_versionMax){
      cat("\n! Warning, this function tested on SS-V",SS_versionMin," through SS-V",SS_versionMax,".\n",
          "  you are using ",substr(SS_version,1,9)," which MIGHT NOT WORK with this R code.\n\n",sep="")
    }else{
      if(verbose)
        cat("! Warning, this function tested on SS-V",SS_versionMin," through SS-V",SS_versionMax,".\n",
            "  you are using ",substr(SS_version,1,9)," which SHOULD work with this R code.\n",sep="")
    }

    findtime <- function(lines){
      # quick function to get model start time from SS output files
      time <- strsplit(lines[grep("ime",lines)],"ime: ")[[1]]
      if(length(time)<2) return() else return(time[2])
    }
    repfiletime <- findtime(rephead)
    if(verbose) cat("Report file time:",repfiletime,"\n")

    corfile <- NA
    if(covar){
      # .cor file
      if(!is.na(parfile)){
        corfile <- sub(".par",".cor",parfile,fixed=TRUE)
        if(!file.exists(corfile)){
          cat("Some stats skipped because the .cor file not found:",corfile,"\n")
          corfile <- NA
        }
      }
      # CoVar.sso file
      covarfile <- file.path(dir,covarfile)
      if(!file.exists(covarfile)){
        stop("covar file not found. Change input to covar=FALSE, or modify 'covarfile' input.\n")
      }

      # time check for CoVar file
      covarhead <- readLines(con=covarfile,n=10)
      covarskip <- grep("active-i",covarhead)-1
      covartime <- findtime(covarhead)
      # the conversion to R time class below may no longer be necessary as strings should match
      if(is.null(covartime) || is.null(repfiletime)){
        cat("problem comparing the file creation times:\n")
        cat("  Report.sso:",repfiletime,"\n")
        cat("  covar.sso:",covartime,"\n")
      }else{
        if( covartime != repfiletime){
          cat("covar time:",covartime,"\n")
          stop(shortrepfile," and ",covarfile," were from different model runs. Change input to covar=FALSE")
        }
      }
      # covar file exists, but has problems
      nowrite <- grep("do not write",covarhead)
      if(length(nowrite)>0){
        stop("problem with covar file: file contains the warning\n",
             "'",covarhead[nowrite],"'\n",
             "Change input to covar=FALSE, or modify 'covarfile' input.\n",sep="")
      }
    }

    # time check for CompReport file
    compfile <- file.path(dir,compfile)
    if(file.exists(compfile)){
      comphead <- readLines(con=compfile,n=30)
      compskip <- grep("Composition_Database",comphead)
      # compend value helps diagnose when no comp data exists in CompReport.sso file.
      compend <- grep(" end ",comphead)
      if(length(compend)==0) compend <- 999
      comptime <- findtime(comphead)
      if(is.null(comptime) || is.null(repfiletime)){
        cat("problem comparing the file creation times:\n")
        cat("  Report.sso:",repfiletime,"\n")
        cat("  CompReport.sso:",comptime,"\n")
      }else{
        if(comptime != repfiletime){
          cat("CompReport time:",comptime,"\n")
          stop(shortrepfile," and ",compfile," were from different model runs.")
        }
      }
      comp <- TRUE
    }else{
      if(!NoCompOK) stop("Missing ",compfile,
                         ". Change the compfile input or rerun model to get the file.\n",sep="")
      else comp <- FALSE
    }

    # read report file
    if(verbose) cat("Reading full report file\n")
    flush.console()
    rawrep <- read.table(file=repfile,col.names=1:ncols,fill=TRUE,quote="",
                         colClasses="character",nrows=-1,comment.char="")

    # Ian T.: if the read.table command above had "blank.lines.skip=TRUE" then blank lines could play a role in parsing the report file

    # check empty columns
    nonblanks <- apply(rawrep,2,emptytest) < 1
    maxnonblank = max(0,(1:ncols)[nonblanks==TRUE])
    if(maxnonblank==ncols){
      stop("all columns are used and some data may been missed,\n",
           "  increase 'ncols' input above current value (ncols=",ncols,")")
    }
    if(verbose){
      if((maxnonblank+1)==ncols) cat("Got all columns.\n")
      if((maxnonblank+1)<ncols) cat("Got all columns. To speed code, use ncols=",maxnonblank+1," in the future.\n",sep="")
      cat("Got Report file\n")
    }
    flush.console()

    # read forecast report file
    if(forecast){
      forecastname <- file.path(dir,forefile)
      temp <- file.info(forecastname)$size
      if(is.na(temp) | temp==0){
        stop("Forecase-report.sso file is empty.\n",
             "Change input to 'forecast=FALSE' or rerun model with forecast turned on.")
      }
      # read the file
      rawforecast1 <- read.table(file=forecastname,col.names=1:ncols,fill=TRUE,quote="",colClasses="character",nrows=-1)
      sprtarg <- as.numeric(rawforecast1[matchfun("SPR_target",rawforecast1[,1]),2])
      btarg   <- as.numeric(rawforecast1[matchfun("Btarget",rawforecast1[,1]),2])
      endyield <- matchfun("MSY_not_calculated",rawforecast1[,1])
      if(is.na(endyield)) yesMSY <- TRUE else yesMSY <- FALSE
      if(yesMSY) endyield <- matchfun("findFmsy",rawforecast1[,10])
      if(verbose) cat("Got Forecast-report file\n")

      # this section on equilibrium yield moved to Report.sso on Jan 6
      startline <- matchfun("profile",rawforecast1[,11])
      if(!is.na(startline)){ # before the Jan 6 fix to benchmarks
        yieldraw <- rawforecast1[(startline+1):endyield,]
      }else{
        yieldraw <- matchfun2("SPR/YPR_Profile",1,"Dynamic_Bzero",-2)
        # note: section with "Dynamic_Bzero" is missing before Hessian is run or skipped
      }
      if(yieldraw[[1]][1]=="absent"){
        cat("!warning: Report.sso appears to be early version from before Hessian was estimated.\n",
            "         equilibrium yield estimates not included in output.\n")
        yieldraw <- NA
      }
      if(is.na(yieldraw[[1]][1])){
        yielddat <- NA
      }else{
        if(SS_versionshort=="SS-V3.11"){
          yielddat <- yieldraw[c(2:(as.numeric(length(yieldraw[,1])-1))),c(4,7)]
          colnames(yielddat) <- c("Catch","Depletion")
        }else{
          names <- yieldraw[1,]
          names[names=="SSB/Bzero"] <- "Depletion"
          yielddat <- yieldraw[c(2:(as.numeric(length(yieldraw[,1])-1))),]
          names(yielddat) <- names #colnames(yielddat) <- c("Catch","Depletion","YPR")
        }
        for(icol in 1:ncol(yielddat)){
          yielddat[,icol] <- as.numeric(yielddat[,icol])
        }
        yielddat <- yielddat[order(yielddat$Depletion,decreasing = FALSE),]
      }
    }else{
      if(verbose)
        cat("You skipped the forecast file\n",
            "  setting SPR target and Biomass target to -999\n",
            "  lines won't be drawn for these targets\n",
            "  (can replace or override in SS_plots by setting 'sprtarg' and 'btarg')\n")
      sprtarg <- -999
      btarg <- -999
    }
    minbthresh <- -999
    if(!is.na(btarg) & btarg==0.4){
      if(verbose)
        cat("Setting minimum biomass threshhold to 0.25\n",
            "  based on US west coast assumption associated with biomass target of 0.4.\n",
            "  (can replace or override in SS_plots by setting 'minbthresh')\n")
      minbthresh <- 0.25 # west coast assumption for non flatfish
    }
    if(!is.na(btarg) & btarg==0.25){
      if(verbose)
        cat("Setting minimum biomass threshhold to 0.25\n",
            "  based on US west coast assumption associated with flatfish target of 0.25.\n",
            "  (can replace or override in SS_plots by setting 'minbthresh')\n")
      minbthresh <- 0.125 # west coast assumption for flatfish
    }

    flush.console()

    # check for use of temporary files
    logfile <- dir(dir,pattern=".log$")
    logfile <- logfile[logfile != "fmin.log"]
    if(length(logfile)>1){
      filetimes <- file.info(file.path(dir,logfile))$mtime
      logfile <- logfile[filetimes==max(filetimes)]
      if(verbose) cat("Multiple files in directory match pattern *.log\n",
                      "choosing most recently modified file:",logfile,"\n")
    }
    if(length(logfile)==1 && file.info(file.path(dir,logfile))$size>0){
      logfile <- read.table(file.path(dir,logfile))[,c(4,6)]
      names(logfile) <- c("TempFile","Size")
      maxtemp <- max(logfile$Size)
      if(maxtemp==0){
        if(verbose) cat("Got log file. There were NO temporary files were written in this run.\n")
      }else{
        if(verbose){
          cat("!warning: temporary files were written in this run:\n")
          print(logfile)
        }
      }
    }else{
      logfile <- NA
      if(verbose) cat("No non-empty log file in directory or too many files matching pattern *.log\n")
    }

    # read warnings file
    if(warn){
      warnname <- file.path(dir,"warning.sso")
      if(!file.exists(warnname)){
        cat("warning.sso file not found\n")
        nwarn <- NA
        warn <- NA
      }else{
        warn <- readLines(warnname,warn=FALSE)
        warnstring <- warn[grep("N warnings: ",warn)]
        if(length(warnstring)>0){
          nwarn <- as.numeric(strsplit(warnstring,"N warnings: ")[[1]][2])
          textblock <- c(paste("were", nwarn, "warnings"),paste("was", nwarn, "warning"))[1+(nwarn==1)]
          if(verbose) cat("Got warning file.\n",
                          " There", textblock, "in", warnname,"\n")
        }else{
          cat("warning.sso file is missing the string 'N warnings'!\n")
          nwarn <- NA
        }
      }
    }else{
      if(verbose) cat("You skipped the warnings file\n")
      nwarn <- NA
    }
    if(verbose) cat("Finished reading files\n")
    flush.console()

    # positions of timeseries section
    begin <- matchfun("TIME_SERIES")+2
    end  <- matchfun("SPR_series")-1

    # selectivity read first because it was used to get fleet info
    # this can be moved to join rest of selex stuff after SSv3.11 not supported any more
    selex <- matchfun2("LEN_SELEX",6,"AGE_SELEX",-1,header=TRUE)
    for(icol in (1:ncol(selex))[!(names(selex) %in% c("Factor","label"))]) selex[,icol] <- as.numeric(selex[,icol])

    ## DEFINITIONS section (new in SSv3.20)
    rawdefs <- matchfun2("DEFINITIONS",1,"LIKELIHOOD",-1)
    # get season stuff
    nseasons <- as.numeric(rawdefs[1,2])
    seasdurations <- as.numeric(rawdefs[3,1+1:nseasons])
    seasfracs <- round(12*cumsum(seasdurations))/12
    seasfracs <- seasfracs - seasdurations/2 # should be mid-point of each season as a fraction of the year
    if(SS_versionNumeric >= 3.3){
      # version 3.3 (fleet info switched from columns to rows starting with 3.3)
      # get fleet info
      defs <- rawdefs[-(1:3),apply(rawdefs[-(1:3),],2,emptytest)<1]
      defs[defs==""] <- NA
      FleetNames <- as.character(defs[grep("fleet_names",defs$X1),-1])
      FleetNames <- FleetNames[!is.na(FleetNames)]
      nfleets <- length(FleetNames)
      fleet_ID    <- 1:nfleets
      defs <- defs[-(1:3),1:8] # hardwiring dimensions, this may change in future versions
      names(defs) <- c("fleet_type", "timing", "area", "units",
                       "equ_catch_se", "catch_se", "survey_units", "survey_error")
      for(icol in 1:ncol(defs)){
        defs[,icol] <- as.numeric(defs[,icol])
      }
      # fleet_type definitions from TPL:
      # 1=fleet with catch; 2=discard only fleet with F;
      # 3=survey(ignore catch); 4=ignore completely
      fleet_type   <- defs$fleet_type
      fleet_timing <- defs$timing
      fleet_area   <- defs$area
      catch_units  <- defs$units
      equ_catch_se <- defs$equ_catch_se
      catch_se     <- defs$catch_se
      survey_units <- defs$survey_units
      survey_error <- defs$survey_error
      IsFishFleet  <- fleet_type <= 2 # based on definitions above
    }else{
      # version 3.20-3.24
      # get fleet info
      defs <- rawdefs[-(1:3),apply(rawdefs[-(1:3),],2,emptytest)<1]
      defs[defs==""] <- NA
      lab <- defs$X1
      fleet_ID     <- as.numeric(defs[grep("fleet_ID",lab),-1])
      names(defs)  <- c("Label",paste("Fleet",fleet_ID,sep=""))
      FleetNames   <- as.character(defs[grep("fleet_names",lab),-1])
      fleet_area   <- as.numeric(defs[grep("fleet_area",lab),-1])
      catch_units  <- as.numeric(defs[grep("Catch_units",lab),-1])
      catch_error  <- as.numeric(defs[grep("Catch_error",lab),-1])
      survey_units <- as.numeric(defs[grep("Survey_units",lab),-1])
      survey_error <- as.numeric(defs[grep("Survey_error",lab),-1])
      IsFishFleet  <- !is.na(catch_units)
      nfleets      <- length(FleetNames)
    }

    # more dimensions
    nfishfleets  <- sum(IsFishFleet)
    nsexes <- length(unique(as.numeric(selex$gender)))
    nareas <- max(as.numeric(rawrep[begin:end,1]))
    startyr <- min(as.numeric(rawrep[begin:end,2]))+2  # this is the 'initial' year not including
    temptime <- rawrep[begin:end,2:3]
    endyr <- max(as.numeric(temptime[temptime[,2]=="TIME",1])) # this is the beginning of the last year of the normal timeseries
    tempaccu <- as.character(rawrep[matchfun("Natural_Mortality")+1,-(1:5)])
    accuage <- max(as.numeric(tempaccu[tempaccu!=""]))
    # which column of INDEX_1 has number of CPUE values (used in reading INDEX_2)
    if(SS_versionNumeric >= 3.3){
      ncpue_column <- 13
      # IAN T.: this may need updating in revised version
    }else{
      ncpue_column <- 11
    }
    ncpue <- sum(as.numeric(rawrep[matchfun("INDEX_1")+1+1:nfleets,ncpue_column]))


    # compositions
    if(comp){   # skip this stuff if no CompReport.sso file
      allbins <- read.table(file=compfile, col.names=1:ncols, fill=TRUE, colClasses="character", skip=3, nrows=15)
      #lbins is data length bins
      lbins <- as.numeric(allbins[7,-1])
      lbins <- lbins[!is.na(lbins)]
      nlbins <- length(lbins)
      #lbinspop is Pop_len_mid used for selex and bio quantities
      lbinspop <- as.numeric(allbins[3,-1])
      lbinspop <- lbinspop[!is.na(lbinspop)]
      nlbinspop <- length(lbinspop)
      Lbin_method <- as.numeric(allbins[matchfun("Method_for_Lbin_definition",allbins[,1]),2])
      if(compend==compskip+2){
        cat("It appears that there is no composition data in CompReport.sso\n")
        comp <- FALSE # turning off switch to function doesn't look for comp data later on
        agebins <- NA
        sizebinlist <- NA
        nagebins <- length(agebins)
      }else{
        # read composition database
        if(SS_versionshort=="SS-V3.11") col.names=1:21 else col.names=1:22
        #if(SS_versionshort=="SS-V3.24") col.names=1:23
        if(SS_versionNumeric >= 3.24) col.names=1:23
        rawcompdbase <- read.table(file=compfile, col.names=col.names, fill=TRUE, colClasses="character", skip=compskip, nrows=-1)
        names(rawcompdbase) <- rawcompdbase[1,]
        names(rawcompdbase)[names(rawcompdbase)=="Used?"] <- "Used"
        endfile <- grep("End_comp_data",rawcompdbase[,1])
        compdbase <- rawcompdbase[2:(endfile-2),] # subtract header line and last 2 lines
        # split Pick_gender=3 into males and females to get a value for sex = 0 (unknown), 1 (female), or 2 (male)
        compdbase$sex <- compdbase$Pick_gender
        compdbase$sex[compdbase$Pick_gender==3] <- compdbase$Gender[compdbase$Pick_gender==3]

        # make correction to tag output associated with 3.24f (fixed in later versions)
        if(substr(SS_version,1,9)=="SS-V3.24f"){
          cat('Correcting for bug in tag data output associated with SSv3.24f\n')
          tag1rows <- compdbase$Pick_gender=="TAG1"
          if(any(tag1rows)){
            tag1 <- compdbase[tag1rows,]
            tag1new <- tag1
            tag1new[,4:23] <- tag1new[,3:22] # shift columns over
            tag1new$Yr.S <- tag1new$Yr # move Yr.S
            tag1new$Yr <- floor(as.numeric(tag1new$Yr)) # turn Yr.S into Yr
            compdbase[tag1rows,] <- tag1new
          }
        }

        compdbase <- compdbase[compdbase$Obs!="",]
        compdbase[compdbase=="_"] <- NA
        compdbase$Used[is.na(compdbase$Used)] <- "yes"
        if(!("SuprPer" %in% names(compdbase))) compdbase$SuprPer <- "No"
        compdbase$SuprPer[is.na(compdbase$SuprPer)] <- "No"

        n <- sum(is.na(compdbase$N) & compdbase$Used!="skip" & compdbase$Kind!="TAG2")
        if(n>0){
          cat("Warning:",n,"rows from composition database have NA sample size\n  but are not part of a super-period. (Maybe input as N=0?)\n")
        }
        for(i in (1:ncol(compdbase))[!(names(compdbase) %in% c("Kind","SuprPer","Used"))]) compdbase[,i] <- as.numeric(compdbase[,i])

        # configure seasons
        if(nseasons>1) compdbase$YrSeasName <- paste(floor(compdbase$Yr),"s",compdbase$Seas,sep="") else compdbase$YrSeasName <- compdbase$Yr

        # starting with SSv3.24a, the Yr.S column is already in the output, otherwise fill it in
        if(!"Yr.S" %in% names(compdbase)){
          if(any(floor(compdbase$Yr)!=compdbase$Yr)){
            # in some cases, year is already a decimal number
            compdbase$Yr.S <- compdbase$Yr
            compdbase$Yr <- floor(compdbase$Yr)
          }else{
            # add fraction of season to distinguish between samples
            compdbase$Yr.S <- compdbase$Yr + (0.5/nseasons)*compdbase$Seas
          }
        }

        # deal with Lbins
        compdbase$Lbin_range <- compdbase$Lbin_hi - compdbase$Lbin_lo
        compdbase$Lbin_mid <- 0.5*(compdbase$Lbin_lo + compdbase$Lbin_hi)

        # divide into objects by kind
        Lbin_range <- compdbase$Lbin_range
        if(is.null(Lbin_range)){ # if/else required to avoid warning if no comp data at all
          notconditional <- TRUE
          conditional <- FALSE
        }else{
          notconditional <- !is.na(Lbin_range) & Lbin_range >  aalmaxbinrange
          conditional    <- !is.na(Lbin_range) & Lbin_range <= aalmaxbinrange
        }

        if(SS_versionNumeric >= 3.22){
          # new designation of ghost fleets from negative samp size to negative fleet
          lendbase         <- compdbase[compdbase$Kind=="LEN"  & compdbase$Used!="skip",]
          sizedbase        <- compdbase[compdbase$Kind=="SIZE" & compdbase$Used!="skip",]
          agedbase         <- compdbase[compdbase$Kind=="AGE"  & compdbase$Used!="skip" & notconditional,]
          condbase         <- compdbase[compdbase$Kind=="AGE"  & compdbase$Used!="skip" & conditional,]
        }else{
          # older designation of ghost fleets from negative samp size to negative fleet
          lendbase         <- compdbase[compdbase$Kind=="LEN"  & (compdbase$SuprPer=="Sup" | (!is.na(compdbase$N) & compdbase$N > 0)),]
          sizedbase        <- compdbase[compdbase$Kind=="SIZE" & (compdbase$SuprPer=="Sup" | (!is.na(compdbase$N) & compdbase$N > 0)),]
          agedbase         <- compdbase[compdbase$Kind=="AGE"  & (compdbase$SuprPer=="Sup" | (!is.na(compdbase$N) & compdbase$N > 0)) & notconditional,]
          condbase         <- compdbase[compdbase$Kind=="AGE"  & (compdbase$SuprPer=="Sup" | (!is.na(compdbase$N) & compdbase$N > 0)) & conditional,]
        }
        ghostagedbase    <- compdbase[compdbase$Kind=="AGE"  & compdbase$Used=="skip" & compdbase$SuprPer=="No" & notconditional,]
        ghostcondbase    <- compdbase[compdbase$Kind=="AGE"  & compdbase$Used=="skip" & compdbase$SuprPer=="No" & conditional,]
        ghostlendbase    <- compdbase[compdbase$Kind=="LEN"  & compdbase$Used=="skip" & compdbase$SuprPer=="No",]
        compdbase$Kind[compdbase$Kind=="L@A" & compdbase$Ageerr < 0] <- "W@A"

        # extra processing for sizedbase
        if(!is.null(sizedbase) && nrow(sizedbase)>0){
          sizedbase$bio.or.num=c("bio","num")[sizedbase$Lbin_lo]
          sizedbase$units=c("kg","lb","cm","in")[sizedbase$Lbin_hi]
          sizedbase$method=sizedbase$Ageerr

          if(any(sizedbase$units %in% c("lb","in"))){
            if(verbose)
              cat("Note: converting bins in generalized size comp data in sizedbase\n",
                  " back to the original units of lbs or inches.\n")
          }
          # convert bins from kg to lbs when that was the original unit
          sizedbase$Bin[sizedbase$units=="lb"] <-
            sizedbase$Bin[sizedbase$units=="lb"]/0.4536
          # convert bins from cm to inches when that was the original unit
          sizedbase$Bin[sizedbase$units=="in"] <-
            sizedbase$Bin[sizedbase$units=="in"]/2.54

          sizebinlist <- list()
          for(imethod in 1:max(sizedbase$method)){
            tmp <- sort(unique(sizedbase$Bin[sizedbase$method==imethod]))
            if(length(tmp)==0) tmp <- NULL
            sizebinlist[[paste("size_method_",imethod,sep="")]] <- tmp
          }
        }else{
          sizebinlist <- NA
        }

        if(is.null(compdbase$N)){
          good <- TRUE
        }else{
          good <- !is.na(compdbase$N)
        }
        ladbase          <- compdbase[compdbase$Kind=="L@A" & good,]
        wadbase          <- compdbase[compdbase$Kind=="W@A" & good,]
        tagdbase1        <- compdbase[compdbase$Kind=="TAG1",]
        tagdbase2        <- compdbase[compdbase$Kind=="TAG2",]
        # consider range of bins for conditional age at length data
        if(verbose){
          cat("CompReport file separated by this code as follows (rows = Ncomps*Nbins):\n",
              "  ",nrow(lendbase), "rows of length comp data,\n",
              "  ",nrow(sizedbase),"rows of generalized size comp data,\n",
              "  ",nrow(agedbase), "rows of age comp data,\n",
              "  ",nrow(condbase), "rows of conditional age-at-length data,\n",
              "  ",nrow(ghostagedbase),"rows of ghost fleet age comp data,\n",
              "  ",nrow(ghostcondbase),"rows of ghost fleet conditional age-at-length data,\n",
              "  ",nrow(ghostlendbase),"rows of ghost fleet length comp data,\n",
              "  ",nrow(ladbase),  "rows of mean length at age data,\n",
              "  ",nrow(wadbase),  "rows of mean weight at age data,\n",
              "  ",nrow(tagdbase1),"rows of 'TAG1' comp data, and\n",
              "  ",nrow(tagdbase2),"rows of 'TAG2' comp data.\n")
        }
        # convert bin indices to true lengths
        if(nrow(agedbase)>0){
          Lbin_ranges <- as.data.frame(table(agedbase$Lbin_range))
          names(Lbin_ranges)[1] <- "Lbin_hi-Lbin_lo"
          if(length(unique(agedbase$Lbin_range)) > 1){
            cat("Warning!: different ranges of Lbin_lo to Lbin_hi found in age comps.\n")
            print(Lbin_ranges)
            cat("  consider increasing 'aalmaxbinrange' to designate\n")
            cat("  some of these data as conditional age-at-length\n")
          }
          agebins <- sort(unique(agedbase$Bin[!is.na(agedbase$Bin)]))
        }else{
          agebins <- NA
        }
        nagebins <- length(agebins)
      }
    }else{
      # if comp option is turned off
      lbins <- NA
      nlbins <- NA

      #### need to get length bins from somewhere
      ## temp <- rawrep[grep("NUMBERS_AT_LENGTH",rawrep[,1])+1,]
      ## lbinspop <- as.numeric(temp[temp!=""][-(1:11)])
      ## nlbinspop <- length(lbinspop)
      lbinspop <- NA
      nlbinspop <- ncol(selex)-5 # hopefully this works alright
      agebins <- NA
      nagebins <- NA
      Lbin_method <- 2
      sizebinlist <- NA
    }

    # info on growth morphs (see also section setting mainmorphs below)
    endcode <- "SIZEFREQ_TRANSLATION" #(this section heading not present in all models)
    #if(SS_versionshort=="SS-V3.11") shift <- -1 else shift <- -2
    shift <- -1
    if(is.na(matchfun(endcode))){
      endcode <- "MOVEMENT"
      shift <- -2
    }
    morph_indexing <- matchfun2("MORPH_INDEXING",1,endcode,shift,cols=1:9,header=TRUE)
    for(i in 1:ncol(morph_indexing)) morph_indexing[,i] <- as.numeric(morph_indexing[,i])
    if(SS_versionNumeric < 3.3){
      ngpatterns <- max(morph_indexing$Gpattern)
    }else{
      ngpatterns <- max(morph_indexing$GP)
    }

    # forecast
    if(forecast){
      grab  <- rawforecast1[,1]
      nforecastyears <- as.numeric(rawforecast1[grab %in% c("N_forecast_yrs:"),2])
      nforecastyears <- nforecastyears[1]
    }else{
      nforecastyears <- NA
    }
    if(verbose) cat("Finished dimensioning\n")
    flush.console()

    # stats list: items that are output to the GUI (if printstats==T) for a quick summary of results
    stats <- list()
    stats$SS_version <- SS_version
    stats$SS_versionshort <- SS_versionshort
    stats$SS_versionNumeric <- SS_versionNumeric

    stats$StartTime <- paste(as.character(matchfun2("StartTime",0,"StartTime",0,cols=1:6)),collapse=" ")
    stats$RunTime <- paste(as.character(matchfun2("StartTime",2,"StartTime",2,cols=4:9)),collapse=" ")

    tempfiles  <- as.data.frame(rawrep[4:5,1:2],row.names = NULL)
    tempfiles <- matchfun2("Data_File",0,"Control_File",0,cols=1:2)
    stats$Files_used <- paste(c(tempfiles[1,],tempfiles[2,]),collapse=" ")

    # check warnings
    stats$Nwarnings <- nwarn
    if(length(warn)>20) warn <- c(warn[1:20],paste("Note:",length(warn)-20,"additional lines truncated. Look in warning.sso file to see full list."))
    stats$warnings <- warn

    # likelihoods
    rawlike <- matchfun2("LIKELIHOOD",2,"Fleet:",-2)
    like <- data.frame(signif(as.numeric(rawlike[,2]),digits=7))
    names(like) <- "values"
    rownames(like) <- rawlike[,1]
    lambdas <- rawlike[,3]
    lambdas[lambdas==""] <- NA
    lambdas <- as.numeric(lambdas)
    like$lambdas <- lambdas
    stats$likelihoods_used <- like
    stats$likelihoods_raw_by_fleet <-
      likelihoods_by_fleet <-
      matchfun2("Fleet:",0,"Input_Variance_Adjustment",-1,header=TRUE)
    likelihoods_by_fleet[likelihoods_by_fleet=="_"] <- NA
    for(icol in 2:ncol(likelihoods_by_fleet)) likelihoods_by_fleet[,icol] <- as.numeric(likelihoods_by_fleet[,icol])
    names(likelihoods_by_fleet) <- c("Label","ALL",FleetNames)
    labs <- likelihoods_by_fleet$Label
    # removing ":" at the end of likelihood components
    for(irow in 1:length(labs)) labs[irow] <- substr(labs[irow],1,nchar(labs[irow])-1)
    likelihoods_by_fleet$Label <- labs
    stats$likelihoods_by_fleet <- likelihoods_by_fleet


    # parameters
    if(SS_versionNumeric>= 3.23) shift <- -1
    if(SS_versionNumeric== 3.22) shift <- -2
    if(SS_versionNumeric < 3.22) shift <- -1
    parameters <- matchfun2("PARAMETERS",1,"DERIVED_QUANTITIES",shift,header=TRUE)

    if(SS_versionNumeric >= 3.23){
      temp <- tail(parameters,2)[,1:3]
      parameters <- parameters[1:(nrow(parameters)-2),]
    }

    parameters[parameters=="_"] <- NA
    parameters[parameters==" "] <- NA

    if(SS_versionNumeric >= 3.22){ # current approach to parameter section
      for(i in (1:ncol(parameters))[!(names(parameters)%in%c("Label","PR_type","Status"))])
        parameters[,i] <- as.numeric(parameters[,i])
    }
    if(SS_versionNumeric==3.21){
      # revised section in SS-V3.21 where text description of PR_type instead of number
      for(i in (1:ncol(parameters))[!(names(parameters)%in%c("Label","PR_type","Status"))])
        parameters[,i] <- as.numeric(parameters[,i])
      temp <- names(parameters)
      cat("Note: inserting new 13th column heading in parameters section due to error in Report.sso in SSv3.21f\n")
      temp <- c(temp[1:12],"PR_type_code",temp[-(1:12)])
      temp <- temp[-length(temp)]
      names(parameters) <- temp
    }
    if(SS_versionNumeric <= 3.20){
      # really old parameters section
      for(i in (1:ncol(parameters))[!(names(parameters)%in%c("Label","Status"))])
        parameters[,i] <- as.numeric(parameters[,i])
    }
    rownames(parameters) <- parameters$Label

    activepars <- parameters$Label[!is.na(parameters$Active_Cnt)]

    if(!is.na(parfile)){
      parline <- read.table(parfile,fill=TRUE,comment.char="",nrows=1)
    }else{
      parline <- matrix(NA,1,16)

    }
    stats$N_estimated_parameters <- parline[1,6]

    pars <- parameters[!is.na(parameters$Phase) & parameters$Phase>0,]

    if(nrow(pars)>0){
      pars$Afterbound <- ""
      pars$checkdiff <- pars$Value - pars$Min
      pars$checkdiff2 <- pars$Max - pars$Value
      pars$checkdiff3 <- abs(pars$Value-(pars$Max-(pars$Max-pars$Min)/2))
      pars$Afterbound[pars$checkdiff < 0.001 | pars$checkdiff2 < 0.001 | pars$checkdiff2 < 0.001] <- "CHECK"
      pars$Afterbound[!pars$Afterbound %in% "CHECK"] <- "OK"
    }
    stats$table_of_phases <- table(parameters$Phase)
    #pars <- pars[pars$Phase %in% 0:100,]
    #stats$estimated_non_rec_devparameters <- pars[,c(2,3,5:14,17)]
    stats$estimated_non_rec_devparameters <- pars[,names(pars) %in%
                                                    c("Label","Value","Phase","Min","Max","Init","Prior","PR_type",
                                                      "Pr_SD","Prior_Like","Parm_StDev","Status","Afterbound")]

    # read covar.sso file
    if(covar){
      CoVar <- read.table(covarfile,header=TRUE,colClasses=c(rep("numeric",4),rep("character",4),"numeric"),skip=covarskip)
      if(verbose) cat("Got covar file.\n")
      stdtable <- CoVar[CoVar$Par..j=="Std",c(7,9,5)]
      names(stdtable) = c("name","std","type")
      N_estimated_parameters2 <- sum(stdtable$type=="Par")

      # this section was muddling Derived Quants with Parameters in early version of SSv3.20
      # got work-around pending fix from Rick to use of "Par" vs. "Der" in covar file.
      if(is.na(stats$N_estimated_parameters)){
        stats$N_estimated_parameters <- N_estimated_parameters2
      }else{
        if(stats$N_estimated_parameters!=N_estimated_parameters2){
          cat("!warning:\n")
          cat(" ",stats$N_estimated_parameters,"estimated parameters indicated by",parfile,"\n")
          cat(" ",N_estimated_parameters2,"estimated parameters shown in",covarfile,"\n")
          cat("  returning the first value,",stats$N_estimated_parameters,"\n")
          stats$N_estimated_parameters <- stats$N_estimated_parameters
        }
      }
      Nstd <- sum(stdtable$std>0)
      checkbadrun <- unique(stdtable$std)
      if (length(checkbadrun) == 1) {
        if (checkbadrun %in% c(NA, "NaN", "na")) {
          stop(paste0("No quantities were estimated in the covar file \nand all",
                      "estimates of standard deviation are ", checkbadrun, ". \nTry re-running",
                      "stock synthesis."))
        }
      }

      if(Nstd<=1){
        stop("Too few estimated quantities in covar file (n=",Nstd,"). Change input to covar=FALSE.")
      }
      if(checkcor==TRUE & stats$N_estimated_parameters > 1)
      {
        corfilter <- CoVar[CoVar$all.i!=CoVar$all.j &
                             CoVar$Par..i=="Par" &
                             CoVar$Par..j=="Par" &
                             CoVar$label.i %in% activepars &
                             CoVar$label.j %in% activepars &
                             !substr(CoVar$label.i,1,8)=="ForeRecr" &
                             !substr(CoVar$label.j,1,8)=="ForeRecr",]
        rangecor <- range(abs(corfilter$corr))
        corstats <- list()
        corstats$cormessage1 <- paste("Range of abs(parameter correlations) is",min(rangecor),"to",max(rangecor))
        # search for high or low correlations in covar file
        highcor <- corfilter[abs(corfilter$corr) >= cormax, names(CoVar)%in%c("label.i", "label.j", "corr")]
        lowcorcandidates <- corfilter[abs(corfilter$corr) <= cormin, names(CoVar)%in%c("label.i", "label.j", "corr")]
        lowcortestlist <- data.frame(unique(c(lowcorcandidates$label.i,lowcorcandidates$label.j)))
        lowcortestlist$name <- as.character(lowcortestlist[,1])
        nlowcor <- 0
        lowcor <- 0
        if(nrow(lowcortestlist)>0)
        {
          lowcortestlist$max <- NA
          for(i in 1:length(lowcortestlist[,1]))
          {
            lowcortestlist$max[i] <- max(corfilter$corr[corfilter$label.i == lowcortestlist$name[i]],corfilter$corr[corfilter$label.j == lowcortestlist$name[i]])
          }
          lowcor <- lowcortestlist[abs(lowcortestlist$max) <= cormin,2:3]
          nlowcor <- nrow(lowcor)
        }
        nhighcor <- nrow(highcor)
        if(printhighcor>0){
          if(nhighcor==0) textblock <- "No correlations"
          if(nhighcor==1) textblock <- "1 correlation"
          if(nhighcor>1)  textblock <- paste(nhighcor,"correlations")
          corstats$cormessage2 <-paste(textblock, " above threshold (cormax=", cormax,")",sep="")
          if(nhighcor>0 & nhighcor<=printhighcor){
            row.names(highcor) = paste("   ",1:nhighcor)
            corstats$cormessage3 <- highcor
          }
          if(nhighcor>0 & nhighcor>printhighcor){
            highcorsub <- highcor[order(-abs(highcor$corr)),]
            highcorsub <- highcorsub[1:printhighcor,]
            row.names(highcorsub) <- paste("   ",1:printhighcor)
            corstats$cormessage4 <- paste("Highest",printhighcor,
                                          "parameter correlations above threshold (to print more, increase 'printhighcor' input):")
            corstats$cormessage5 <- highcorsub
          }
        }else{
          corstats$cormessage6 <- "High correlations not reported. To report, change 'printhighcor' input to a positive value."
        }

        if(printlowcor>0){
          if(nlowcor==0) textblock <- "No uncorrelated parameters"
          if(nlowcor==1) textblock <- "1 uncorrelation"
          if(nlowcor>1)  textblock <- paste(nlowcor,"uncorrelated parameters")
          corstats$cormessage7 <- paste(textblock, " below threshold (cormin=", cormin,")",sep="")
          if(nlowcor>0 & nlowcor<=printlowcor){
            corstats$cormessage8 <-lowcor
          }
          if(nlowcor>0 & nlowcor>printlowcor){
            lowcorsub <- lowcor[order(abs(lowcor$max)),]
            lowcorsub <- lowcorsub[1:printlowcor,]
            corstats$cormessage9 <- paste("Lowest",printlowcor,
                                          "parameters uncorrelations below threshold (to print more, increase 'printlowcor' input):")
            corstats$cormessage10 <-lowcorsub
          }
        }else{
          corstats$cormessage11 <-"Uncorrelated parameters not reported. To report, change 'printlowcor' input to a positive value."
        }
      }else{
        corstats <- NA
        if(verbose){
          cat("You skipped the correlation check (or have only 1 parameter)\n")
        }
      }
    }else{
      if(verbose){
        cat("You skipped the covar file\n")
      }
    }
    flush.console()

    # read weight-at-age file
    wtatage <- NULL
    if(readwt){
      wtfile <- file.path(dir,wtfile)
      if(!file.exists(wtfile) | file.info(wtfile)$size==0){
        if(verbose) cat("Skipping weight-at-age file. File missing or empty:",wtfile,"\n")
      }else{
        # read top few lines to figure out how many to skip
        wtatagelines <- readLines(wtfile,n=20)
        # read full file
        wtatage <- read.table(wtfile,header=TRUE,comment.char="",
                              skip=(grep("yr seas gender",wtatagelines)-1))
        names(wtatage)[1] <- "yr" # replacing "X.yr" created by presence of #
      }
    }

    # derived quantities
    if(SS_versionNumeric < 3.3){
      der <- matchfun2("DERIVED_QUANTITIES",4,"MGparm_By_Year_after_adjustments",-1,
                       header=TRUE)
      MGParm_dev_details <- NA
    }else{
      der <- matchfun2("DERIVED_QUANTITIES",4,"MGParm_dev_details",0,
                       header=TRUE)
      MGParm_dev_details <- matchfun2("MGParm_dev_details",1,
                                      "MGparm_By_Year_after_adjustments",-1,
                                      header=TRUE)
    }
    der <- der[der$LABEL!="Bzero_again",]
    der[der=="_"] <- NA
    for(i in 2:3) der[,i] = as.numeric(der[,i])
    rownames(der) <- der$LABEL

    managementratiolabels <- matchfun2("DERIVED_QUANTITIES",1,"DERIVED_QUANTITIES",3,cols=1:2)
    names(managementratiolabels) <- c("Ratio","Label")

    # time-varying parameters
    MGparmAdj <- matchfun2("MGparm_By_Year_after_adjustments",1,
                           "selparm(Size)_By_Year_after_adjustments",-1,header=TRUE)
    if(nrow(MGparmAdj)>0){
      for(icol in 1:ncol(MGparmAdj)) MGparmAdj[,icol] <- as.numeric(MGparmAdj[,icol])
    }else{
      MGparmAdj <- NA
    }

    # time-varying size-selectivity parameters
    SelSizeAdj <- matchfun2("selparm(Size)_By_Year_after_adjustments",2,"selparm(Age)_By_Year_after_adjustments",-1)
    if(nrow(SelSizeAdj)>2){
      SelSizeAdj <- SelSizeAdj[,apply(SelSizeAdj,2,emptytest)<1]
      SelSizeAdj[SelSizeAdj==""] <- NA
      for(icol in 1:ncol(SelSizeAdj)) SelSizeAdj[,icol] <- as.numeric(SelSizeAdj[,icol])
      names(SelSizeAdj) <- c("FleetSvy","Yr",paste("Par",1:(ncol(SelSizeAdj)-2),sep=""))
    }else{
      SelSizeAdj <- NA
    }

    # time-varying age-selectivity parameters
    SelAgeAdj <- matchfun2("selparm(Age)_By_Year_after_adjustments",2,"RECRUITMENT_DIST",-1)
    if(nrow(SelAgeAdj)>2){
      SelAgeAdj <- SelAgeAdj[,apply(SelAgeAdj,2,emptytest)<1]
      SelAgeAdj[SelAgeAdj==""] <- NA
      if(SelAgeAdj[1,1]=="RECRUITMENT_DIST"){
        SelAgeAdj <- NA
      }else{
        for(icol in 1:ncol(SelAgeAdj)) SelAgeAdj[,icol] <- as.numeric(SelAgeAdj[,icol])
        names(SelAgeAdj) <- c("FleetSvy","Yr",paste("Par",1:(ncol(SelAgeAdj)-2),sep=""))
      }
    }else{
      SelAgeAdj <- NA
    }

    # recruitment distribution
    recruitment_dist <- matchfun2("RECRUITMENT_DIST",1,"MORPH_INDEXING",-1,header=TRUE)
    # starting in SSv3.24Q there are additional outputs that get combined as a list
    if(length(grep("RECRUITMENT_DIST_BENCHMARK",recruitment_dist[,1]))==0){
      for(i in 1:6) recruitment_dist[,i] <- as.numeric(recruitment_dist[,i])
    }else{
      recruitment_dist <- matchfun2("RECRUITMENT_DIST",0,"MORPH_INDEXING",-1,header=FALSE)
      # start empty list
      rd <- list()
      # find break points in table
      rd.line.top   <- 1
      rd.line.bench <- grep("RECRUITMENT_DIST_BENCHMARK", recruitment_dist[,1])
      rd.line.fore  <- grep("RECRUITMENT_DIST_FORECAST", recruitment_dist[,1])
      rd.line.end   <- nrow(recruitment_dist)
      # split apart table
      rd$recruit_dist_endyr      <- recruitment_dist[(rd.line.top+1):(rd.line.bench-1),]
      rd$recruit_dist_benchmarks <- recruitment_dist[(rd.line.bench+1):(rd.line.fore-1),]
      rd$recruit_dist_forecast   <- recruitment_dist[(rd.line.fore+1):(rd.line.end),]
      for(i in 1:length(rd)){
        # convert first row to header
        tmp <- rd[[i]]
        names(tmp) <- tmp[1,]
        tmp <- tmp[-1,]
        for(icol in 1:6) tmp[,icol] <- as.numeric(tmp[,icol])
        rd[[i]] <- tmp
      }
      # provide as same name
      recruitment_dist <- rd
    }

    # gradient
    if(covar & !is.na(corfile)) stats$log_det_hessian <- read.table(corfile,nrows=1)[1,10]
    stats$maximum_gradient_component <- as.numeric(matchfun2("Convergence_Level",0,"Convergence_Level",0,cols=2))

    # sigma_R
    if(SS_versionNumeric >= 3.3 |
       substring(SS_version,1,9) %in% c("SS-V3.24U", "SS-V3.24V",
                                        "SS-V3.24W", "SS-V3.24X",
                                        "SS-V3.24Y", "SS-V3.24Z")){
      # accounting for additional line introduced in 3.24U
      last_row_index <- 11
    }else{
      last_row_index <- 10
    }

    srhead <- matchfun2("SPAWN_RECRUIT",0,"SPAWN_RECRUIT",last_row_index,cols=1:6)
    rmse_table <- as.data.frame(srhead[-(1:(last_row_index-1)),1:5])
    for(icol in 2:5){
      rmse_table[,icol] <- as.numeric(rmse_table[,icol])
    }
    names(rmse_table) <- srhead[last_row_index-1,1:5]
    names(rmse_table)[4] <- "RMSE_over_sigmaR"
    stats$sigma_R_in <- as.numeric(srhead[last_row_index-6,1])
    stats$rmse_table <- rmse_table

    # Bias adjustment ramp
    biascol <- grep("breakpoints_for_bias", srhead)
    breakpoints_for_bias_adjustment_ramp <- srhead[
      grep("breakpoints_for_bias", srhead[, biascol]), 1:5]
    colnames(breakpoints_for_bias_adjustment_ramp) <- c("last_yr_early",
                                                        "first_yr_full", "last_yr_full", "first_yr_recent", "max_bias_adj")
    rownames(breakpoints_for_bias_adjustment_ramp) <- NULL

    # Spawner-recruit curve
    rawsr <- matchfun2("SPAWN_RECRUIT",last_row_index+1,"INDEX_2",-1,cols=1:9)
    names(rawsr) <- rawsr[1,]
    rawsr[rawsr=="_"] <- NA
    rawsr <- rawsr[-(1:2),] # remove header rows
    sr <- rawsr[-(1:2),] # remove rows for Virg and Init
    for(i in 1:(ncol(sr)-1)) sr[,i] <- as.numeric(sr[,i])

    # variance and sample size tuning information
    vartune <- matchfun2("INDEX_1",1,"INDEX_1",(nfleets+1),cols=1:21,header=TRUE)
    vartune <- vartune[vartune$N > 0,]
    vartune[,1] <- vartune[,21]
    vartune <- vartune[,c(1,8,11,13,16,18)]
    stats$index_variance_tuning_check <- vartune

    # Length comp effective N tuning check
    lenntune <- matchfun2("FIT_AGE_COMPS",-(nfleets+1),"FIT_AGE_COMPS",-1,cols=1:10,header=TRUE)
    names(lenntune)[10] <- "FleetName"
    lenntune <- lenntune[lenntune$N>0, c(10,1,4:9)]
    # avoid NA warnings by removing #IND values
    lenntune$"MeaneffN/MeaninputN"[lenntune$"MeaneffN/MeaninputN"=="-1.#IND"] <- NA
    for(icol in 2:ncol(lenntune)) lenntune[,icol] <- as.numeric(lenntune[,icol])
    lenntune$"HarEffN/MeanInputN" <- lenntune$"HarMean(effN)"/lenntune$"mean(inputN*Adj)"
    stats$Length_comp_Eff_N_tuning_check <- lenntune

    ## # FIT_AGE_COMPS
    fit_age_comps <- matchfun2("FIT_AGE_COMPS",1,"FIT_SIZE_COMPS",-(nfleets+2),header=TRUE)
    if(nrow(fit_age_comps)>0){
      # replace underscores with NA
      fit_age_comps[fit_age_comps=="_"] <- NA
      # make columns numeric (except "Used", which may contain "skip")
      for(icol in which(!names(fit_age_comps) %in% "Use")){
        fit_age_comps[,icol] <- as.numeric(fit_age_comps[,icol])
      }
    }else{
      fit_age_comps <- NA
    }

    # Age comp effective N tuning check
    agentune <- matchfun2("FIT_SIZE_COMPS",-(nfleets+1),"FIT_SIZE_COMPS",-1,cols=1:10,header=TRUE)
    names(agentune)[10] <- "FleetName"
    agentune <- agentune[agentune$N>0, c(10,1,4:9)]
    # avoid NA warnings by removing #IND values
    agentune$"MeaneffN/MeaninputN"[agentune$"MeaneffN/MeaninputN"=="-1.#IND"] <- NA
    for(i in 2:ncol(agentune)) agentune[,i] <- as.numeric(agentune[,i])
    agentune$"HarEffN/MeanInputN" <- agentune$"HarMean(effN)"/agentune$"mean(inputN*Adj)"
    stats$Age_comp_Eff_N_tuning_check <- agentune

    if(FALSE){
      # !! Ian T., fix this to read tuning for generalized size comp data
      #            this can be done with a shift in strategy of using blank.lines.skip=TRUE
      #            in read.table, but that will require additional revisions throughout
      sizentune <- matchfun2("LEN_SELEX",-(nfleets+1),"LEN_SELEX",-1,cols=1:10,header=TRUE)
      sizentune[,1] <- sizentune[,10]
      sizentune <- sizentune[sizentune$Npos>0, c(1,3,4,5,6,8,9)]
      stats$Size_comp_Eff_N_tuning_check <- sizentune
    }


    if(verbose) cat("Finished primary run statistics list\n")
    flush.console()

    # data return object
    returndat <- list()

    if(SS_versionNumeric <= 3.24){
      returndat$definitions  <- defs
      returndat$fleet_ID     <- fleet_ID
      returndat$fleet_area   <- fleet_area
      returndat$catch_units  <- catch_units
      returndat$catch_error  <- catch_error
    }
    if(SS_versionNumeric >= 3.3){
      returndat$definitions  <- defs
      returndat$fleet_ID     <- fleet_ID
      returndat$fleet_type   <- fleet_area
      returndat$fleet_timing <- fleet_area
      returndat$fleet_area   <- fleet_area
      returndat$catch_units  <- catch_units
      returndat$catch_se     <- catch_se
      returndat$equ_catch_se <- equ_catch_se
    }
    returndat$survey_units <- survey_units
    returndat$survey_error <- survey_error
    returndat$IsFishFleet  <- IsFishFleet
    returndat$nfishfleets  <- nfishfleets

    returndat$nfleets     <- nfleets
    returndat$nsexes      <- nsexes
    returndat$ngpatterns  <- ngpatterns
    returndat$lbins       <- lbins
    returndat$Lbin_method <- Lbin_method
    returndat$nlbins      <- nlbins
    returndat$lbinspop    <- lbinspop
    returndat$nlbinspop   <- nlbinspop
    returndat$sizebinlist <- sizebinlist
    returndat$agebins     <- agebins
    returndat$nagebins    <- nagebins
    returndat$accuage     <- accuage
    returndat$nareas      <- nareas
    returndat$startyr     <- startyr
    returndat$endyr       <- endyr
    returndat$nseasons    <- nseasons
    returndat$seasfracs   <- seasfracs
    returndat$seasdurations  <- seasdurations
    returndat$nforecastyears <- nforecastyears
    returndat$morph_indexing <- morph_indexing
    returndat$MGParm_dev_details <- MGParm_dev_details
    returndat$MGparmAdj   <- MGparmAdj
    returndat$SelSizeAdj  <- SelSizeAdj
    returndat$SelAgeAdj   <- SelAgeAdj
    returndat$recruitment_dist <- recruitment_dist
    returndat$recruit     <- sr
    returndat$breakpoints_for_bias_adjustment_ramp <- breakpoints_for_bias_adjustment_ramp


    # Static growth
    begin <- matchfun("N_Used_morphs",rawrep[,6])+1 # keyword "BIOLOGY" not unique enough
    rawbio <- rawrep[begin:(begin+nlbinspop),1:10]
    rawbio <- rawbio[,apply(rawbio,2,emptytest) < 1]
    names(rawbio) <- rawbio[1,]
    biology <- rawbio[-1,]
    for(i in 1:ncol(biology)) biology[,i] <- as.numeric(biology[,i])

    # determine fecundity type
    FecType <- 0
    if("Eggs/kg_slope_wt_Fem" %in% parameters$Label){
      FecType <- 1
      FecPar1name <- "Eggs/kg_inter_Fem"
      FecPar2name <- "Eggs/kg_slope_wt_Fem"
    }
    if("Eggs_exp_len_Fem" %in% parameters$Label){
      FecType <- 2
      FecPar1name <- "Eggs_scalar_Fem"
      FecPar2name <- "Eggs_exp_len_Fem"
    }
    if("Eggs_exp_wt_Fem" %in% parameters$Label){
      FecType <- 3
      FecPar1name <- "Eggs_scalar_Fem"
      FecPar2name <- "Eggs_exp_wt_Fem"
    }
    if("Eggs_slope_len_Fem" %in% parameters$Label){
      FecType <- 4
      FecPar1name <- "Eggs_intercept_Fem"
      FecPar2name <- "Eggs_slope_len_Fem"
    }
    if("Eggs_slope_Wt_Fem" %in% parameters$Label){
      FecType <- 5
      FecPar1name <- "Eggs_intercept_Fem"
      FecPar2name <- "Eggs_slope_Wt_Fem"
    }
    returndat$biology <- biology
    returndat$FecType <- FecType
    returndat$FecPar1name <- FecPar1name
    returndat$FecPar2name <- FecPar2name

    returndat$FecPar1 <- parameters$Value[parameters$Label==FecPar1name]
    returndat$FecPar2 <- parameters$Value[parameters$Label==FecPar2name]

    # simple test to figure out if fecundity is proportional to spawning biomass:
    returndat$SpawnOutputUnits <- ifelse(!is.na(biology$Fecundity[1]) &&
                                           all(biology$Wt_len_F==biology$Fecundity),
                                         "biomass", "numbers")

    Growth_Parameters <- matchfun2("Growth_Parameters",1,
                                   "Growth_Parameters",1+nrow(morph_indexing),
                                   header=TRUE)
    for(icol in 1:ncol(Growth_Parameters)){
      Growth_Parameters[,icol] <- as.numeric(Growth_Parameters[,icol])
    }
    returndat$Growth_Parameters <- Growth_Parameters

    Seas_Effects <- matchfun2("Seas_Effects",1,"Biology_at_age_in_endyr",-1,header=TRUE)
    if(Seas_Effects[[1]][1]!="absent"){
      for(i in 1:ncol(Seas_Effects)) Seas_Effects[,i] <- as.numeric(Seas_Effects[,i])
    }else{
      Seas_Effects <- NA
    }
    returndat$Seas_Effects <- Seas_Effects

    # ending year growth, including pattern for the CV (added in SSv3.22b_Aug3)
    growthCVtype <- matchfun2("Biology_at_age",0,"Biology_at_age",0,header=FALSE)
    if(nchar(growthCVtype)>31){
      returndat$growthCVtype <- substring(growthCVtype,30)
    }else{
      returndat$growthCVtype <- "unknown"
    }
    growdat <- matchfun2("Biology_at_age",1,"MEAN_BODY_WT(begin)",-1,header=TRUE)
    for(i in 1:ncol(growdat)) growdat[,i] <- as.numeric(growdat[,i])
    nmorphs <- max(growdat$Morph)
    midmorphs <- c(c(0,nmorphs/nsexes)+ceiling(nmorphs/nsexes/2))
    returndat$endgrowth <- growdat


    # test for use of empirical weight-at-age input file (wtatage.ss)
    test <- matchfun2("MEAN_BODY_WT(begin)",0,"MEAN_BODY_WT(begin)",0,header=FALSE)
    wtatage_switch <- length(grep("wtatage.ss",test))>0
    returndat$wtatage_switch <- wtatage_switch

    # mean body weight
    mean_body_wt <- matchfun2("MEAN_BODY_WT(begin)",1,"MEAN_SIZE_TIMESERIES",-1,header=TRUE)
    for(i in 1:ncol(mean_body_wt)) mean_body_wt[,i] <- as.numeric(mean_body_wt[,i])
    returndat$mean_body_wt <- mean_body_wt

    # Time-varying growth
    rawgrow <- matchfun2("MEAN_SIZE_TIMESERIES",1,"mean_size_Jan_1_for_gender",-1,cols=1:(4+accuage+1))
    growthvaries <- FALSE
    if(length(rawgrow)>1){
      names(rawgrow) <- rawgrow[1,]
      growdat <- rawgrow[-1,]
      for(i in 1:ncol(growdat)) growdat[,i] <- as.numeric(growdat[,i])
      growdat <- growdat[growdat$Beg==1 & growdat$Yr >= startyr & growdat$Yr < endyr,]
      if(nseasons > 1) growdat <- growdat[growdat$Seas==1,]
      if(length(unique(growdat$Yr))>1) growthvaries <- TRUE
      returndat$growthseries <- growdat
      returndat$growthvaries <- growthvaries
    }

    # Length selex and retention
    if(!forecast) selex <- selex[selex$year <= endyr,]
    returndat$sizeselex <- selex

    # Age based selex
    ageselex <- matchfun2("AGE_SELEX",4,"ENVIRONMENTAL_DATA",-1,header=TRUE)
    if(!forecast) ageselex <- ageselex[ageselex$year <= endyr,]
    for(icol in (1:ncol(ageselex))[!(names(ageselex) %in% c("factor","label"))]) ageselex[,icol] <- as.numeric(ageselex[,icol])
    returndat$ageselex <- ageselex

    # time series
    timeseries <- matchfun2("TIME_SERIES",1,"SPR_series",-1,header=TRUE)
    timeseries[timeseries=="_"] <- NA
    for(i in (1:ncol(timeseries))[names(timeseries)!="Era"]) timeseries[,i] = as.numeric(timeseries[,i])

    ## # sum catches and other quantities across fleets
    ## # commented out pending additional test for more than one fleet with catch,
    ## # without which the apply function has errors
    ## timeseries$dead_B_sum <- apply(timeseries[,grep("dead(B)",names(timeseries),
    ##                                                 fixed=TRUE)], 1, sum)
    ## timeseries$dead_N_sum <- apply(timeseries[,grep("dead(N)",names(timeseries),
    ##                                                 fixed=TRUE)], 1, sum)
    ## timeseries$retain_B_sum <- apply(timeseries[,grep("retain(B)",names(timeseries),
    ##                                                   fixed=TRUE)], 1, sum)
    ## timeseries$retain_N_sum <- apply(timeseries[,grep("retain(N)",names(timeseries),
    ##                                                   fixed=TRUE)], 1, sum)
    ## timeseries$sel_B_sum <- apply(timeseries[,grep("sel(B)",names(timeseries),
    ##                                                fixed=TRUE)], 1, sum)
    ## timeseries$sel_N_sum <- apply(timeseries[,grep("sel(N)",names(timeseries),
    ##                                                fixed=TRUE)], 1, sum)
    ## timeseries$obs_cat_sum <- apply(timeseries[,grep("obs_cat",names(timeseries),
    ##                                                  fixed=TRUE)], 1, sum)

    returndat$timeseries <- timeseries

    # get spawning season
    # currently (v3.20b), Spawning Biomass is only calculated in a unique spawning season within the year
    spawnseas <- unique(timeseries$Seas[!is.na(timeseries$SpawnBio)])
    # probablem with spawning season calculation when NA values in SpawnBio
    if(length(spawnseas)==0){
      spawnseas <- NA
    }
    returndat$spawnseas <- spawnseas
    # get birth seasons as vector of seasons with non-zero recruitment
    returndat$birthseas <- sort(unique(timeseries$Seas[timeseries$Recruit_0 > 0]))

    # set mainmorphs as those morphs born in the spawning season
    # and the largest fraction of the submorphs (should equal middle morph when using sub-morphs)
    if(SS_versionNumeric >= 3.3){
      # new "platoon" label
      temp <- morph_indexing[morph_indexing$Bseas==min(spawnseas) &
                               morph_indexing$Platoon_Dist==max(morph_indexing$Platoon_Dist),]
    }else{
      # old "sub_morph" label
      temp <- morph_indexing[morph_indexing$Bseas==min(spawnseas) &
                               morph_indexing$Sub_Morph_Dist==max(morph_indexing$Sub_Morph_Dist),]
    }
    # however, if there are no fish born in the spawning season, then it should be the first birth season
    if("recruit_dist_endyr" %in% names(recruitment_dist)){
      rd <- recruitment_dist$recruit_dist_endyr
    }else{
      rd <- recruitment_dist
    }
    # this work around needed for 12/2/2013 version of 3.3
    # which has simpler recruitment_dist than 3.24S
    if(is.null(rd$Used)){
      rd$Used <- 1
    }
    if(!is.na(spawnseas) & rd$Used[spawnseas]==0){
      if(SS_versionNumeric >= 3.3){
        # new "platoon" label
        temp <- morph_indexing[morph_indexing$Bseas==min(rd$Seas[rd$Used==1]) &
                                 morph_indexing$Platoon_Dist==max(morph_indexing$Platoon_Dist),]
      }else{
        # old "sub_morph" label
        temp <- morph_indexing[morph_indexing$Bseas==min(rd$Seas[rd$Used==1]) &
                                 morph_indexing$Sub_Morph_Dist==max(morph_indexing$Sub_Morph_Dist),]
      }
    }
    # filter in case multiple growth patterns (would cause problems)
    if(SS_versionNumeric >= 3.3){
      column_label <- "Sex"
    }else{
      column_label <- "Gender"
    }
    mainmorphs <- min(temp$Index[temp[[column_label]]==1])
    if(nsexes==2){
      mainmorphs <- c(mainmorphs, min(temp$Index[temp[[column_label]]==2]))
    }
    if(length(mainmorphs)==0){
      cat("!Error with morph indexing in SS_output function.\n")
    }
    returndat$mainmorphs  <- mainmorphs

    # stats and dimensions
    timeseries$Yr <- timeseries$Yr + (timeseries$Seas-1)/nseasons
    ts <- timeseries[timeseries$Yr <= endyr+1,]
    tsyears <- ts$Yr[ts$Seas==1]

    # Depletion
    tsspaw_bio <- ts$SpawnBio[ts$Seas==spawnseas & ts$Area==1]
    if(nareas > 1) # loop over areas if necessary to sum spawning biomass
    {
      for(a in 2:nareas){
        tsspaw_bio <- tsspaw_bio + ts$SpawnBio[ts$Seas==spawnseas & ts$Area==a]
      }
    }
    if(nsexes==1){
      tsspaw_bio <- tsspaw_bio/2
    }
    depletionseries <- tsspaw_bio/tsspaw_bio[1]
    stats$SBzero <- tsspaw_bio[1]
    stats$current_depletion <- depletionseries[length(depletionseries)]

    # total landings (in the future, this section should be cleaned up to take advantage of
    # new columns that are in process of being added above, such as $dead_B_sum
    ls <- nrow(ts)-1
    totretainedmat <- as.matrix(ts[,substr(names(ts),1,nchar("retain(B)"))=="retain(B)"])
    ts$totretained <- 0
    ts$totretained[3:ls] <- rowSums(totretainedmat)[3:ls]

    # total catch
    totcatchmat <- as.matrix(ts[,substr(names(ts),1,nchar("enc(B)"))=="enc(B)"])
    ts$totcatch <- 0
    ts$totcatch[3:ls] <- rowSums(totcatchmat)[3:ls]

    # harvest rates
    F_method <- as.numeric(rawrep[matchfun("F_Method"),2])
    returndat$F_method <- F_method
    if(F_method==1){
      stringmatch <- "Hrate:_"
    }else{stringmatch <- "F:_"}
    Hrates <- as.matrix(ts[,substr(names(ts),1,nchar(stringmatch))==stringmatch])
    fmax <- max(Hrates)
    #stats$fmax <- fmax
    #stats$endyrcatch <- ts$totcatch[ls]
    #stats$endyrlandings <- ts$totretained[ls]

    # depletion
    depletion_method <- as.numeric(rawrep[matchfun("Depletion_method"),2])
    depletion_basis <- rawrep[matchfun("B_ratio_denominator"),2]
    if(depletion_basis=="no_depletion_basis"){
      depletion_basis <- "none"
    }else{
      depletion_basis <- as.numeric(strsplit(depletion_basis,"%*",fixed=TRUE)[[1]][1])/100
    }
    returndat$depletion_method <- depletion_method
    returndat$depletion_basis <- depletion_basis

    ## discard fractions ###

    # degrees of freedom for T-distribution (or indicator 0, -1, -2 for other distributions)
    if(SS_versionNumeric < 3.20){
      # old header from 3.11
      DF_discard <- rawrep[matchfun("DISCARD_OUTPUT"),3]
      if(length(grep("T_distribution",DF_discard))>0)
        DF_discard <- as.numeric(strsplit(DF_discard,"=_")[[1]][2])
      if(length(grep("_normal_with_Std_in_as_CV",DF_discard))>0)     DF_discard <- 0
      if(length(grep("_normal_with_Std_in_as_stddev",DF_discard))>0) DF_discard <- -1
      if(length(grep("_lognormal",DF_discard))>0)                    DF_discard <- -2
      shift <- 2
      discard_spec <- NULL
    }else{ # newer header in 3.20 and beyond
      DF_discard <- NA
      shift <- 1
      discard_spec <- matchfun2("DISCARD_SPECIFICATION",9,"DISCARD_OUTPUT",-2,
                                cols=1:3,header=TRUE)
      # test for Robbie Emmet's experimental new discard option
      if(length(grep("trunc_normal", names(discard_spec)))>0){
        discard_spec <- matchfun2("DISCARD_SPECIFICATION",10,"DISCARD_OUTPUT",-2,
                                  cols=1:3,header=TRUE)
      }
      for(icol in 1:3){
        discard_spec[,icol] <- as.numeric(discard_spec[,icol])
      }
      names(discard_spec)[1] <- "Fleet"
    }
    discard <- matchfun2("DISCARD_OUTPUT",shift,"MEAN_BODY_WT_OUTPUT",-1,header=TRUE)
    if(names(discard)[1]=="MEAN_BODY_WT_OUTPUT"){
      discard <- NA
    }
    if(!is.na(discard) && names(discard)[1]!="Fleet"){
      # rerun read of discard if in SSv3.20b which had missing line break
      discard <- matchfun2("DISCARD_OUTPUT",shift,"MEAN_BODY_WT_OUTPUT",-1,header=FALSE)
      names(discard) <- c("Fleet","Yr","Seas","Obs","Exp","Std_in","Std_use","Dev")
    }

    discard_type <- NA
    if(!is.na(discard) && nrow(discard)>1){
      discard[discard=="_"] <- NA
      if(SS_versionNumeric <= 3.23){ # v3.23 and before had things combined under "name"
        for(icol in (1:ncol(discard))[!(names(discard) %in% c("Fleet"))])
          discard[,icol] <- as.numeric(discard[,icol])
        discard$FleetNum <- NA
        if(!"Name"%in%names(discard)) discard$Name <- discard$Fleet
        for(i in 1:nrow(discard)){
          discard$FleetNum[i] <- strsplit(discard$Name[i],"_")[[1]][1]
          discard$FleetName[i] <- substring(discard$Name[i],nchar(discard$FleetNum[i])+2)
        }
      }else{ # v3.24 and beyond has separate columns for fleet number and fleet name
        for(icol in (1:ncol(discard))[!(names(discard) %in% c("Name","SuprPer"))])
          discard[,icol] <- as.numeric(discard[,icol])
        # redundant columns are holdovers from earlier SS versions
        discard$FleetNum <- discard$Fleet
        discard$FleetName <- discard$Name
      }
    }else{
      discard <- NA
    }
    returndat$discard <- discard
    returndat$discard_type <- discard_type
    returndat$DF_discard <- DF_discard
    returndat$discard_spec <- discard_spec

    ## Average body weight observations
    # degrees of freedom for T-distribution
    # old way: DF_mnwgt <- rawrep[matchfun("MEAN_BODY_WT_OUTPUT")+1,1]
    DF_mnwgt <- rawrep[matchfun("log(L)_based_on_T_distribution"),1]
    if(!is.na(DF_mnwgt)){
      DF_mnwgt <- as.numeric(strsplit(DF_mnwgt,"=_")[[1]][2])
      mnwgt <- matchfun2("MEAN_BODY_WT_OUTPUT",2,"FIT_LEN_COMPS",-1,header=TRUE)

      mnwgt[mnwgt=="_"] <- NA
      if(SS_versionNumeric <= 3.23){ # v3.23 and before had things combined under "name"
        for(icol in (1:ncol(mnwgt))[!(names(mnwgt) %in% c("Fleet"))])
          mnwgt[,icol] <- as.numeric(mnwgt[,icol])
        mnwgt$FleetNum <- NA
        for(i in 1:nrow(mnwgt)){
          mnwgt$FleetNum[i] <- strsplit(mnwgt$Fleet[i],"_")[[1]][1]
          mnwgt$FleetName[i] <- substring(mnwgt$Fleet[i],nchar(mnwgt$FleetNum[i])+2)
        }
      }else{ # v3.24 and beyond has separate columns for fleet number and fleet name
        for(icol in (1:ncol(mnwgt))[!(names(mnwgt) %in% c("Name"))])
          mnwgt[,icol] <- as.numeric(mnwgt[,icol])
        # redundant columns are holdovers from earlier SS versions
        mnwgt$FleetNum <- mnwgt$Fleet
        mnwgt$FleetName <- mnwgt$Name
      }
    }else{
      DF_mnwgt <- NA
      mnwgt <- NA
    }
    returndat$mnwgt <- mnwgt
    returndat$DF_mnwgt <- DF_mnwgt

    # Yield and SPR time-series
    spr <- matchfun2("SPR_series",5,"SPAWN_RECRUIT",-1,header=TRUE)
    if(length(grep("Kobe_Plot",rawrep[,1]))!=0){
      shift <- -3
      if(SS_versionNumeric < 3.23) shift <- -1
      spr <- matchfun2("SPR_series",5,"Kobe_Plot",shift,header=TRUE)
      Kobe_head <- matchfun2("Kobe_Plot",0,"Kobe_Plot",3,header=TRUE)
      if(length(grep("F_report_basis_is_not",Kobe_head[1,1]))>0){
        shift <- 2
        Kobe_warn <- Kobe_head[1,1]
      }else{
        shift <- 1
        Kobe_warn <- NA
      }
      Kobe <- matchfun2("Kobe_Plot",shift,"SPAWN_RECRUIT",-1,header=TRUE)
      Kobe_MSY_basis <- names(Kobe)[1]
      names(Kobe) <- Kobe[1,]
      Kobe <- Kobe[-1,]
      Kobe[Kobe=="_"] <- NA
      for(icol in 1:3){
        names(Kobe)[icol] <- sub("/",".",names(Kobe)[icol],fixed=TRUE)
        Kobe[,icol] <- as.numeric(Kobe[,icol])
      }
    }else{
      Kobe <- NA
      Kobe_warn <- NA
      Kobe_MSY_basis <- NA
    }
    returndat$Kobe_warn <- Kobe_warn
    returndat$Kobe_MSY_basis <- Kobe_MSY_basis
    returndat$Kobe <- Kobe
    spr[spr=="_"] <- NA
    spr[spr=="&"] <- NA
    for(i in (1:ncol(spr))[!(names(spr)%in%c("Actual:","More_F(by_morph):"))]) spr[,i] <- as.numeric(spr[,i])
    #spr <- spr[spr$Year <= endyr,]
    spr$spr <- spr$SPR
    returndat$sprseries <- spr
    stats$last_years_SPR <- spr$spr[nrow(spr)]
    stats$SPRratioLabel <- managementratiolabels[1,2]
    stats$last_years_SPRratio <- spr$SPR_std[nrow(spr)]

    returndat$managementratiolabels <- managementratiolabels
    returndat$F_report_basis <- managementratiolabels$Label[2]
    returndat$B_ratio_denominator <- as.numeric(strsplit(managementratiolabels$Label[3],"%")[[1]][1])/100
    returndat$sprtarg <- sprtarg
    returndat$btarg <- btarg

    # override minbthresh = 0.25 if it looks like hake
    if(!is.na(btarg) & btarg==0.4 & startyr==1966 & sprtarg==0.4 &
       accuage==20 & wtatage_switch){
      if(verbose)
        cat("Setting minimum biomass threshhold to 0.10 because this looks like hake\n",
            "  (can replace or override in SS_plots by setting 'minbthresh')\n")
      minbthresh <- 0.1 # treaty value for hake
    }
    returndat$minbthresh <- minbthresh

    if(forecast){
      returndat$equil_yield <- yielddat
      # stats$spr_at_msy <- as.numeric(rawforecast[33,2])
      # stats$exploit_at_msy <- as.numeric(rawforecast[35,2])
      # stats$bmsy_over_VLHbzero <- as.numeric(rawforecast[38,3])
      # stats$retained_msy <- as.numeric(rawforecast[43,5])
    }else{if(verbose) cat("You skipped the equilibrium yield data\n")}
    flush.console()

    if(ncpue>0)
    {
      # CPUE/Survey series
      cpue <- matchfun2("INDEX_2",1,"INDEX_2",ncpue+1,header=TRUE)
      cpue[cpue=="_"] <- NA
      cpue$FleetName <- NA
      cpue$FleetNum <- NA
      if(SS_versionNumeric < 3.24){
        for(i in (1:ncol(cpue))[!names(cpue) %in% c("Fleet","Supr_Per")]) cpue[,i] <- as.numeric(cpue[,i])
        for(i in 1:nrow(cpue)){
          cpue$FleetNum[i] <- strsplit(cpue$Fleet[i],"_")[[1]][1]
          cpue$FleetName[i] <- substring(cpue$Fleet[i],nchar(cpue$FleetNum[i])+2)
        }
      }else{
        for(i in (1:ncol(cpue))[!names(cpue) %in% c("Name","Supr_Per")]) cpue[,i] <- as.numeric(cpue[,i])
        # redundant columns are used to easily maintain backwards compatibility of plotting code
        cpue$FleetNum <- cpue$Fleet
        cpue$FleetName <- cpue$Name
      }
    }else{
      cpue <- NA
    }
    returndat$cpue <- cpue

    # Numbers at age
    rawnatage <- matchfun2("NUMBERS_AT_AGE",1,"NUMBERS_AT_LENGTH",-1,cols=1:(12+accuage),substr1=FALSE)
    if(length(rawnatage)>1){
      names(rawnatage) <- rawnatage[1,]
      rawnatage <- rawnatage[-1,]
      for(i in (1:ncol(rawnatage))[!(names(rawnatage) %in% c("Beg/Mid","Era"))]) rawnatage[,i] = as.numeric(rawnatage[,i])
      returndat$natage <- rawnatage
    }

    # Numbers at length
    if(length(grep("BIOMASS_AT_LENGTH",rawrep[,1]))==0){
      rawnatlen <- matchfun2("NUMBERS_AT_LENGTH",1,"CATCH_AT_AGE",-1,cols=1:(11+nlbinspop),substr1=FALSE)
    }else{
      rawnatlen <- matchfun2("NUMBERS_AT_LENGTH",1,"BIOMASS_AT_LENGTH",-1,cols=1:(11+nlbinspop),substr1=FALSE)
    }
    if(length(rawnatlen)>1){
      names(rawnatlen) <- rawnatlen[1,]
      rawnatlen <- rawnatlen[-1,]
      for(i in (1:ncol(rawnatlen))[!(names(rawnatlen) %in% c("Beg/Mid","Era"))]) rawnatlen[,i] = as.numeric(rawnatlen[,i])
      returndat$natlen <- rawnatlen
    }

    # Biomass at length (first appeared in version 3.24l, 12-5-2012)
    if(length(grep("BIOMASS_AT_LENGTH",rawrep[,1]))>0){
      rawbatlen <- matchfun2("BIOMASS_AT_LENGTH",1,"CATCH_AT_AGE",-1,cols=1:(11+nlbinspop),substr1=FALSE)
      if(length(rawbatlen)>1){
        names(rawbatlen) <- rawbatlen[1,]
        rawbatlen <- rawbatlen[-1,]
        for(i in (1:ncol(rawbatlen))[!(names(rawbatlen) %in% c("Beg/Mid","Era"))]) rawbatlen[,i] = as.numeric(rawbatlen[,i])
        returndat$batlen <- rawbatlen
      }
    }


    # Movement
    movement <- matchfun2("MOVEMENT",1,"EXPLOITATION",-1,cols=1:(7+accuage),substr1=FALSE)
    names(movement) <- c(movement[1,1:6],paste("age",movement[1,-(1:6)],sep=""))
    movement <- movement[-1,]
    for(i in 1:ncol(movement)) movement[,i] <- as.numeric(movement[,i])
    returndat$movement <- movement

    # reporting rates
    tagreportrates <- matchfun2("Reporting_Rates_by_Fishery",1,
                                "See_composition_data_output_for_tag_recapture_details",-1,
                                cols=1:3)
    if(tagreportrates[[1]][1]!="absent"){
      names(tagreportrates) <- tagreportrates[1,]
      tagreportrates <- tagreportrates[-1,]
      for(i in 1:ncol(tagreportrates)) tagreportrates[,i] <- as.numeric(tagreportrates[,i])
      returndat$tagreportrates <- tagreportrates
    }else{
      returndat$tagreportrates <- NA
    }

    # tag recapture table
    tagrecap <- matchfun2("TAG_Recapture",1,
                          "Tags_Alive",-1,
                          cols=1:10)
    if(tagrecap[[1]][1]!="absent"){
      tagfirstperiod <- as.numeric(tagrecap[1,1])
      tagaccumperiod <- as.numeric(tagrecap[2,1])
      names(tagrecap) <- tagrecap[4,]
      tagrecap <- tagrecap[-(1:4),]
      for(i in 1:ncol(tagrecap)) tagrecap[,i] <- as.numeric(tagrecap[,i])
      returndat$tagrecap <- tagrecap
      returndat$tagfirstperiod <- tagfirstperiod
      returndat$tagaccumperiod <- tagaccumperiod
    }else{
      returndat$tagrecap <- NA
      returndat$tagfirstperiod <- NA
      returndat$tagaccumperiod <- NA
    }

    # tags alive
    tagsalive <- matchfun2("Tags_Alive",1,
                           "Total_recaptures",-1,
                           cols=1:ncols)
    if(tagsalive[[1]][1]!="absent"){
      tagcols <- max((1:ncols)[apply(tagsalive,2,function(x){any(x!="")})])
      tagsalive <- tagsalive[,1:tagcols]
      names(tagsalive) <- c("TG",paste("period",0:(tagcols-2),sep=""))
      for(i in 1:ncol(tagsalive)) tagsalive[,i] <- as.numeric(tagsalive[,i])
      returndat$tagsalive <- tagsalive
    }else{
      returndat$tagsalive <- NA
    }

    # total recaptures
    tagtotrecap <- matchfun2("Total_recaptures",1,
                             "Reporting_Rates_by_Fishery",-1,
                             cols=1:ncols)
    if(tagtotrecap[[1]][1]!="absent"){
      tagcols <- max((1:ncols)[apply(tagtotrecap,2,function(x){any(x!="")})])
      tagtotrecap <- tagtotrecap[,1:tagcols]
      names(tagtotrecap) <- c("TG",paste("period",0:(tagcols-2),sep=""))
      for(i in 1:ncol(tagtotrecap)) tagtotrecap[,i] <- as.numeric(tagtotrecap[,i])
      returndat$tagtotrecap <- tagtotrecap
    }else{
      returndat$tagtotrecap <- NA
    }

    # age-length matrix
    rawALK <- matchfun2("AGE_LENGTH_KEY",4,"AGE_AGE_KEY",-1,cols=1:(accuage+2))
    if(length(rawALK)>1){
      morph_col <- 5
      if(SS_versionNumeric < 3.3 &
         length(grep("Sub_Seas", rawALK[,3]))==0){
        morph_col <- 3
      }
      starts <- grep("Morph:",rawALK[,morph_col])+2
      ends <- grep("mean",rawALK[,1])-1
      N_ALKs <- length(starts)
      # 3rd dimension should be either nmorphs or nmorphs*(number of Sub_Seas)
      ALK = array(NA,c(nlbinspop,accuage+1,length(starts)))
      for(i in 1:N_ALKs){
        ALKtemp <- rawALK[starts[i]:ends[i],-1]
        for(icol in 1:(accuage+1)) ALKtemp[,icol] <- as.numeric(ALKtemp[,icol])
        ALK[,,i] <- as.matrix(ALKtemp)
      }
      returndat$ALK <- ALK
    }

    # ageing error matrices
    rawAAK <- matchfun2("AGE_AGE_KEY",1,"SELEX_database",-1,cols=1:(accuage+2))
    if(length(rawAAK)>1){
      starts <- grep("KEY:",rawAAK[,1])
      returndat$N_ageerror_defs <- N_ageerror_defs <- length(starts)
      if(N_ageerror_defs > 0)
      {
        # loop over ageing error types to get definitions
        nrowsAAK <- nrow(rawAAK)/N_ageerror_defs - 3
        AAK = array(NA,c(N_ageerror_defs,nrowsAAK,accuage+1))
        age_error_mean <- age_error_sd <- data.frame(age=0:accuage)
        for(i in 1:N_ageerror_defs){
          AAKtemp <- rawAAK[starts[i] + 2 + 1:nrowsAAK,-1]
          rownames.tmp <- rawAAK[starts[i] + 2 + 1:nrowsAAK,1]
          for(icol in 1:(accuage+1)) AAKtemp[,icol] <- as.numeric(AAKtemp[,icol])
          AAK[i,,] <- as.matrix(AAKtemp)
          age_error_mean[[paste("type",i,sep="")]] <- as.numeric((rawAAK[starts[i] + 1,-1]))
          age_error_sd[[paste("type",i,sep="")]] <- as.numeric((rawAAK[starts[i] + 2,-1]))
        }
        # add names to 3 dimensions of age-age-key
        if(!is.null(AAK)){
          dimnames(AAK) <- list(AgeingErrorType=1:N_ageerror_defs,
                                ObsAgeBin=rownames.tmp,
                                TrueAge=0:accuage)
        }
        returndat$AAK <- AAK
        returndat$age_error_mean <- age_error_mean
        returndat$age_error_sd <- age_error_sd
      }
    }

    # catch at age
    catage <- matchfun2("CATCH_AT_AGE",1,"BIOLOGY",-1)
    if(catage[[1]][1]=="absent"){
      catage <- NA
      cat("! Warning: no catch-at-age numbers because 'detailed age-structured reports'\n",
          "          is turned off in starter file.\n")
    }else{
      catage <- catage[,apply(catage,2,emptytest)<1]
      names(catage) <- catage[1,]
      catage <- catage[-1,]
      for(icol in (1:ncol(catage))[substr(names(catage),1,2)!="XX" & names(catage)!="Era"]){
        catage[,icol] <- as.numeric(catage[,icol])
      }
    }
    returndat$catage <- catage

    if(!is.na(matchfun("Z_AT_AGE"))){
      # Z at age
      #With_fishery
      #No_fishery_for_Z=M_and_dynamic_Bzero
      Z_at_age <- matchfun2("Z_AT_AGE_Annual_2",1,"Spawning_Biomass_Report_1",-2,header=TRUE)
      M_at_age <- matchfun2("Z_AT_AGE_Annual_1",1,"-ln(Nt+1",-1,matchcol2=5, header=TRUE)
      if(nrow(Z_at_age)>0){
        Z_at_age[Z_at_age=="_"] <- NA
        M_at_age[M_at_age=="_"] <- NA
        # if birth season is not season 1, you can get infinite values
        Z_at_age[Z_at_age=="-1.#INF"] <- NA
        M_at_age[M_at_age=="-1.#INF"] <- NA
        if(Z_at_age[[1]][1]!="absent" && nrow(Z_at_age>0)){
          for(i in 1:ncol(Z_at_age)) Z_at_age[,i] <- as.numeric(Z_at_age[,i])
          for(i in 1:ncol(M_at_age)) M_at_age[,i] <- as.numeric(M_at_age[,i])
        }else{
          Z_at_age <- NA
          M_at_age <- NA
        }
      }
    }else{
      # this could be cleaned up
      Z_at_age <- NA
      M_at_age <- NA
    }
    returndat$Z_at_age <- Z_at_age
    returndat$M_at_age <- M_at_age

    # Dynamic_Bzero output "with fishery"
    Dynamic_Bzero1 <- matchfun2("Spawning_Biomass_Report_2",1,"NUMBERS_AT_AGE_Annual_2",-1)
    # Dynamic_Bzero output "no fishery"
    Dynamic_Bzero2 <- matchfun2("Spawning_Biomass_Report_1",1,"NUMBERS_AT_AGE_Annual_1",-1)
    if(Dynamic_Bzero1[[1]][1]=="absent"){
      Dynamic_Bzero <- NA
    }else{
      Dynamic_Bzero <- cbind(Dynamic_Bzero1,Dynamic_Bzero2[,3])
      names(Dynamic_Bzero) <- c("Yr","Era","SPB","SPB_nofishing")
      if(nareas==1 & ngpatterns==1){ # for simpler models, do some cleanup
        Dynamic_Bzero <- Dynamic_Bzero[-(1:2),]
        for(icol in c(1,3,4)) Dynamic_Bzero[,icol] <- as.numeric(as.character(Dynamic_Bzero[,icol]))
        names(Dynamic_Bzero) <- c("Yr","Era","SPB","SPB_nofishing")
      }
    }
    returndat$Dynamic_Bzero <- Dynamic_Bzero

    # adding stuff to list which gets returned by function
    if(comp){
      returndat$comp_data_exists <- TRUE
      returndat$lendbase      <- lendbase
      returndat$sizedbase     <- sizedbase
      returndat$agedbase      <- agedbase
      returndat$condbase      <- condbase
      returndat$ghostagedbase <- ghostagedbase
      returndat$ghostcondbase <- ghostcondbase
      returndat$ghostlendbase <- ghostlendbase
      returndat$ladbase       <- ladbase
      returndat$wadbase       <- wadbase
      returndat$tagdbase1     <- tagdbase1
      returndat$tagdbase2     <- tagdbase2
    }else{
      returndat$comp_data_exists <- FALSE
    }
    # tables on fit to comps and mean age stuff from within Report.sso
    returndat$age_comp_fit_table <- fit_age_comps

    returndat$derived_quants <- der
    returndat$parameters <- parameters
    returndat$FleetNames <- FleetNames
    returndat$repfiletime <- repfiletime
    returndat$SRRtype <- as.numeric(rawrep[matchfun("SPAWN_RECRUIT"),3]) # type of stock recruit relationship

    # get "sigma" used by Pacific Council in P-star calculations
    SPB_final_Label <- paste0("SPB_",endyr+1)
    if(SPB_final_Label %in% der$LABEL){
      SPB_final_EST <- der$Value[der$LABEL==SPB_final_Label]
      SPB_final_SD <- der$StdDev[der$LABEL==SPB_final_Label]
      returndat$Pstar_sigma <- sqrt(log((SPB_final_SD/SPB_final_EST)^2+1))
    }else{
      returndat$Pstar_sigma <- NULL
    }

    if(covar){
      returndat$CoVar    <- CoVar
      if(stats$N_estimated_parameters > 1){
        returndat$highcor  <- highcor
        returndat$lowcor   <- lowcor
        returndat$corstats <- corstats
      }
      returndat$stdtable <- stdtable
    }
    returndat <- c(returndat,stats)
    returndat$logfile <- logfile

    # process annual recruit devs
    recdevEarly   <- parameters[substring(parameters$Label,1,13)=="Early_RecrDev",]
    early_initage <- parameters[substring(parameters$Label,1,13)=="Early_InitAge",]
    main_initage  <- parameters[substring(parameters$Label,1,12)=="Main_InitAge",]
    recdev        <- parameters[substring(parameters$Label,1,12)=="Main_RecrDev",]
    recdevFore    <- parameters[substring(parameters$Label,1, 8)=="ForeRecr",]
    recdevLate    <- parameters[substring(parameters$Label,1,12)=="Late_RecrDev",]

    if(nrow(recdev)>0){
      recdev$Yr        <- as.numeric(substring(recdev$Label,14))
    }
    if(nrow(recdevEarly)>0){
      recdevEarly$Yr   <- as.numeric(substring(recdevEarly$Label,15))
    }
    if(nrow(early_initage)>0){
      early_initage$Yr <- startyr - as.numeric(substring(early_initage$Label,15))
      recdevEarly <- rbind(early_initage,recdevEarly)
    }
    if(nrow(main_initage)>0){
      main_initage$Yr  <- startyr - as.numeric(substring(main_initage$Label,14))
      recdev <- rbind(main_initage,recdev)
    }
    if(nrow(recdevFore)>0)
      recdevFore$Yr <- as.numeric(substring(recdevFore$Label,10))
    if(nrow(recdevLate)>0)
      recdevLate$Yr <- as.numeric(substring(recdevLate$Label,14))
    if(nrow(recdevFore)>0 & nrow(recdevLate)>0)
      recdevFore <- rbind(recdevLate,recdevFore)

    Yr <- c(recdevEarly$Yr,recdev$Yr,recdevFore$Yr)
    recruitpars <- rbind(if(nrow(recdevEarly)>0){recdevEarly}else{NULL},
                         if(nrow(recdevEarly)>0){recdev}else{NULL},
                         if(nrow(recdevEarly)>0){recdevFore}else{NULL})
    returndat$recruitpars <- recruitpars
    # process adjustments to recruit devs
    RecrDistpars <- parameters[substring(parameters$Label,1,8)=="RecrDist",]
    returndat$RecrDistpars <- RecrDistpars

    # adding read of wtatage file
    returndat$wtatage <- wtatage

    # print list of statistics
    if(printstats){
      cat("Statistics shown below (to turn off, change input to printstats=FALSE)\n")

      # remove scientific notation (only for display, not returned values, which were added to returndat already)
      stats$likelihoods_used <- format(stats$likelihoods_used,scientific=20)
      stats$estimated_non_rec_devparameters <- format(stats$estimated_non_rec_devparameters,scientific=20)
      print(stats)
      if(covar){
        if(stats$N_estimated_parameters > 1){
          print(corstats, quote=FALSE)
        }else{
          cat("Too few estimated parameters to report correlations.\n")
        }
      }
    }


    # return the inputs to this function so they can be used by SSplots or other functions
    inputs <- list()
    inputs$dir      <- dir
    inputs$model    <- model
    inputs$repfile  <- repfile
    inputs$forecast <- forecast
    inputs$warn     <- warn
    inputs$covar    <- covar
    inputs$verbose  <- verbose

    returndat$inputs <- inputs

    if(verbose) cat("completed SS_output\n")
    invisible(returndat)

  } # end function

#' Read catch values
#'
#' Return a timeseries of fleet catches and important
#' population status metrics.
#'
#'
#' @param output SS ouput list created by rd_output.
#' @param yrs the years of data to be returned.
#'
#' @author Nathan Vaughan
#' @keywords auxillary functions
ReadCatch <- function(output,yrs){
  CatchMatrix<-matrix(NA,nrow=(length(output$timeseries[,1])*output$nfishfleets),ncol=17)
  std_F_Mat<-matrix(0,nrow=output$nseasons,ncol=(length(output$sprseries[,1])+2))
  std_F_Mat[1,]<-c(0,0,output$sprseries[,12])
  zero_Mat<-matrix(0,nrow=output$nseasons,ncol=(length(output$sprseries[,1])+2))
  for(i in 1:output$nfishfleets){
    CatchMatrix[((i-1)*length(output$timeseries[,1])+1):(i*length(output$timeseries[,1])),1:3]<-as.matrix(output$timeseries[,c(2,4,1)])
    CatchMatrix[((i-1)*length(output$timeseries[,1])+1):(i*length(output$timeseries[,1])),4]<-rep(i,length(output$timeseries[,1]))
    CatchMatrix[((i-1)*length(output$timeseries[,1])+1):(i*length(output$timeseries[,1])),5:12]<-as.matrix(output$timeseries[,(c(1:8)+(8+output$ngpatterns+2*output$ngpatterns*output$nsexes+8*(i-1)))])
    CatchMatrix[((i-1)*length(output$timeseries[,1])+1):(i*length(output$timeseries[,1])),13]<-rep(c(output$sprseries[1:2,4],output$sprseries[,5]),output$nseasons*output$nareas)
    if(i==1){CatchMatrix[((i-1)*length(output$timeseries[,1])+1):(i*length(output$timeseries[,1])),17]<-c(c(std_F_Mat),rep(c(zero_Mat),(output$nareas-1)))
    }else{CatchMatrix[((i-1)*length(output$timeseries[,1])+1):(i*length(output$timeseries[,1])),17]<-rep(c(zero_Mat),output$nareas)}
    CatchMatrix[((i-1)*length(output$timeseries[,1])+1):(i*length(output$timeseries[,1])),14:16]<-as.matrix(output$timeseries[,c(7,8,5)])
  }
  #incProgress(0.00, detail = paste0("reading catch projections cleaning SPB (This can take a few minutes)"))
  CatchMatrix[,13]<-ifelse((CatchMatrix[,2]==1 & CatchMatrix[,4]==1),CatchMatrix[,13],0)
  #incProgress(0.00, detail = paste0("reading catch projections cleaning SSB (This can take a few minutes)"))
  CatchMatrix[,14]<-ifelse((CatchMatrix[,2]==1 & CatchMatrix[,4]==1),CatchMatrix[,14],0)
  #incProgress(0.00, detail = paste0("reading catch projections cleaning Total Biomass (This can take a few minutes)"))
  CatchMatrix[,16]<-ifelse((CatchMatrix[,2]==1 & CatchMatrix[,4]==1),CatchMatrix[,16],0)
  #incProgress(0.00, detail = paste0("reading catch projections cleaning Recruits (This can take a few minutes)"))
  CatchMatrix[,15]<-ifelse((CatchMatrix[,4]==1),CatchMatrix[,15],0)
  CatchMatrix[]
  CatchMatrix<-data.frame(CatchMatrix)
  CatchAll<-CatchMatrix
  CatchC<-cbind(CatchAll[,1:4],CatchAll[,7],(CatchAll[,6]-CatchAll[,7]),CatchAll[,10],(CatchAll[,9]-CatchAll[,10]),CatchAll[,11:17])
  names(CatchC)<-c("Year","Season","Area","Fleet","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPB","SSB","Recruits","PopBiomass","F(Std)")
  CatchC<-CatchC[CatchC[,1]>=yrs[1] & CatchC[,1]<=yrs[length(yrs)],]
  return(CatchC)
}
