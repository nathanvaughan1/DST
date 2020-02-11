#' Build base quota
#'
#' Reads the assessment outputs and builds a GUI
#' plotting interface to display the important
#' results of the base assessment.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
buildBaseQuota<-function(input, output, session){
  incProgress(0.1, detail = paste0("Building output results display"))
  incProgress(0.2, detail="Building selection interface")
  removeUI("#currOutput>*",multiple=TRUE,immediate=TRUE)
  removeUI("#currOutput",multiple=TRUE,immediate=TRUE)
  newDiv("QuotaForecast","currOutput")
  newFluidRow("currOutput","BaseQuota")

  #newFluidRow("currOutput","forecastResults")

  newColumn("BaseQuota","BaseQuotaInputs",width = 4)
  newFluidRow("BaseQuotaInputs","BaseDataChoiceInputs")
  newColumn("BaseDataChoiceInputs","BaseDataChoiceInputs2",width = 12)
  newFluidRow("BaseQuotaInputs","BaseTimeSeriesInputs")
  newColumn("BaseTimeSeriesInputs","BaseTimeSeriesInputs2",width = 12)
  newFluidRow("BaseQuotaInputs","BaseKobePlotInputs")
  newColumn("BaseKobePlotInputs","BaseKobePlotInputs2",width = 12)
  newFluidRow("BaseQuotaInputs","BaseyearRangeInputs")
  newColumn("BaseyearRangeInputs","BaseyearRangeInputs2",width = 12)
  newColumn("BaseQuota","BaseQuotaGraph",width = 8)
  newFluidRow("BaseTimeSeriesInputs2","displayRadioBase")
  newRadio(location="BaseDataChoiceInputs2",id="displayRadioButtonBase5",label="Projection dispay data:",choices=c("Landings","Dead Discards","Harvest Rate","SSB","Population Biomass","Recruits"),selected="Landings",inline=TRUE)
  #newRadio(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase1",label="Forecast results source:",choices=c("Base","Target","Implemented"),selected="Implemented",inline=TRUE)
  #newRadio(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase7",label="Source relative to:",choices=c("Raw","Base","Target"),selected="Raw",inline=TRUE)
  #newRadio(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase8",label="Difference format:",choices=c("Absolute","Percentage","Proportion"),selected="Absolute",inline=TRUE)
  newRadio(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase2",label="Units:",choices=c("Weight","Number"),selected="Weight",inline=TRUE)
  if(data.orig$nseas == 1 & data.orig$N_areas ==1){
    newtext(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase6",inputText="")
    hideElement(id="displayRadioButtonBase6")
  }else if(data.orig$nseas == 1){
    newCheckBoxGroup(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase6",label="Aggregate by:",choices=c("Area"),selected=c("Area"),inline=TRUE)
  }else if(data.orig$N_areas == 1){
    newCheckBoxGroup(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase6",label="Aggregate by:",choices=c("Season"),selected=c("Season"),inline=TRUE)
  }else{
    newCheckBoxGroup(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase6",label="Aggregate by:",choices=c("Area","Season"),selected=c("Area","Season"),inline=TRUE)
  }

  if(data.orig$Nfleet==1)
  {
    newRadio(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase",label="Partition fishery by:",choices=c("Total"),selected="Total",inline=TRUE)
  }else if(length(unique(forecast.orig$fleet_assignment_to_allocation_group))==1)
  {
    newRadio(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase",label="Partition fishery by:",choices=c("Total","Fleets"),selected="Total",inline=TRUE)
  }else
  {
    newRadio(location="BaseTimeSeriesInputs2",id="displayRadioButtonBase",label="Partition fishery by:",choices=c("Total","Groups","Fleets"),selected="Total",inline=TRUE)
  }

  color <- colors()[grep('gr(a|e)y', colors(), invert = T)]
  color <- color[8:433]
  color <- sample(color,length(color))
  color <- c("red","dark blue","dark green", "gold", "pink", "purple","black","cyan","brown",color)
  color <- unique(color)
  colorChoicesBase <- color

  newFluidRow("BaseQuotaGraph","timeSeriesBase")
  newColumn("timeSeriesBase","timeSeriesBasePlots",width=8)
  newColumn("timeSeriesBase","timeSeriesBaseLegendCol",width=4)

  runthis=TRUE
  incProgress(0.1, detail = paste0("Compliling catch data"))

  CatchBase1<-baseCatch
  CatchBase1<-cbind(CatchBase1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchBase1[,4]],CatchBase1[,5:15])
  names(CatchBase1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","HarvestRate(F)")
  CatchBase1<-CatchBase1[order(CatchBase1[,3],CatchBase1[,1],CatchBase1[,2],CatchBase1[,4]),]

  newCheckBoxGroup(location='BaseTimeSeriesInputs2',id="displayRadioButtonBase4",label="Allocation Groups to Display",selected=as.numeric(disp.Desc$GroupNames[,1]),choiceNames=disp.Desc$GroupNames[,2],choiceValues=as.numeric(disp.Desc$GroupNames[,1]))
  newCheckBoxGroup(location='BaseTimeSeriesInputs2',id="displayRadioButtonBase3",label="Fleets to Display",selected=as.numeric(disp.Desc$FleetNames[,1]),choiceNames=disp.Desc$FleetNames[,2],choiceValues=as.numeric(disp.Desc$FleetNames[,1]))
  newSlider(location='BaseyearRangeInputs2',id="displayYearsBase",titleText = "Years to display", min=min(CatchBase1[,1]),max=max(CatchBase1[,1]),value = c((data.orig$endyr+1),data.orig$endyr+7),step=1,sep="")
  #newActionButton(location='BaseyearRangeInputs2',id="saveOutput",label = "Save displayed figure/data")
  #newtextInput(location='BaseyearRangeInputs2',id="saveName",value=paste0(disp.Desc$Title),label = "Saved figure/data name")

  idBase1<-"timeSeriesPlotsBase"
  locationBase1<-"timeSeriesBasePlots"
  idBase4<-"timeSeriesLegendBase"
  locationBase4<-"timeSeriesBaseLegendCol"
  xlabBase="Year"
  newPlot(location=locationBase1,id=idBase1)
  newPlot(location=locationBase4,id=idBase4)
  incProgress(0.1, detail = paste0("Building output figures"))

  observerPool[[3]]<<-observe(priority=1,{
    #print("running observer 3")
    withProgress(message="Creating Base results figures",value=0.5,{
      if(!is.null(input$displayRadioButtonBase) &
         #!is.null(input$displayRadioButtonBase1) &
         !is.null(input$displayRadioButtonBase2) &
         !is.null(input$displayRadioButtonBase3) &
         !is.null(input$displayRadioButtonBase4) &
         !is.null(input$displayRadioButtonBase5) &
         #!is.null(input$displayRadioButtonBase7) &
         #!is.null(input$displayRadioButtonBase8) &
         !is.null(input$displayYearsBase))
      {
        xlimBase=c((input$displayYearsBase[1]-1),(input$displayYearsBase[2]+3))
        xlimTargBase<-xlimBase
        if(!is.null(disp.Desc$TargetYears))
        {
          xlimTargBase<-disp.Desc$TargetYears
        }
        labelPointBase<-seq(input$displayYearsBase[1],input$displayYearsBase[2],max(1,floor((input$displayYearsBase[2]-input$displayYearsBase[1])/4)))
        if((input$displayYearsBase[2]-input$displayYearsBase[1])>=10){
          labelPointBase<-labelPointBase[-length(labelPointBase)]
        }

        {
          if(input$displayRadioButtonBase5=="Harvest Rate")
          {
            unitModBase<-"F"
            catModBase<-"Harvest Rate"
            catMod2Base<-"Harvest Rate"
            dataColOutBase<-c(1:5,16)
          }else if(input$displayRadioButtonBase5!="Landings" & input$displayRadioButtonBase5!="Dead Discards")
          {
            unitModBase<-weightNames[disp.Desc$Units[1]]
            if(input$displayRadioButtonBase5=="SPB"){
              catModBase<-"Equilibrium Spawning Potential Biomass"
              catMod2Base<-"Equilibrium Spawning Potential Biomass"
              dataColOutBase<-c(1:5,12)
            }else if(input$displayRadioButtonBase5=="SSB"){
              catModBase<-"Spawning Stock Biomass"
              catMod2Base<-"Spawning Stock Biomass"
              dataColOutBase<-c(1:5,13)
            }else if(input$displayRadioButtonBase5=="Recruits"){
              catModBase<-"Recruitment"
              catMod2Base<-"Recruitment"
              dataColOutBase<-c(1:5,14)
            }else if(input$displayRadioButtonBase5=="Population Biomass"){
              catModBase<-"Total Population Biomass"
              catMod2Base<-"Total Population Biomass"
              dataColOutBase<-c(1:5,15)
            }
          }else if(input$displayRadioButtonBase5=="Landings")
          {
            if(input$displayRadioButtonBase2=="Weight")
            {
              dataColOutBase<-c(1:5,6)
              unitModBase<-weightNames[disp.Desc$Units[1]]
            }else if(input$displayRadioButtonBase2=="Number")
            {
              dataColOutBase<-c(1:5,8)
              unitModBase<-"1000's"
            }
            catModBase<-"Catch"
            catMod2Base<-"Catch"
          }else if(input$displayRadioButtonBase5=="Dead Discards")
          {
            if(input$displayRadioButtonBase2=="Weight")
            {
              dataColOutBase<-c(1:5,7)
              unitModBase<-weightNames[disp.Desc$Units[1]]
            }else if(input$displayRadioButtonBase2=="Number")
            {
              dataColOutBase<-c(1:5,9)
              unitModBase<-"1000's"
            }
            catModBase<-"Dead Discards"
            catMod2Base<-"Dead Discards"
          }
          CatchDispBase<-CatchBase1
          CatchDisp2Base<-CatchBase1
          sourceModBase<-"Base"


          {
            potNamesCatchBase<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","HarvestRate(F)")
            CatchDispBase<-CatchDispBase[,dataColOutBase]
            CatchDisp2Base<-CatchDisp2Base[,dataColOutBase]
            if("Area"%in%input$displayRadioButtonBase6 | length(unique(CatchDispBase[,3]))==1)
            {
              CatchDispBase[,3]<-rep(-99,length(CatchDispBase[,3]))
              CatchDisp2Base[,3]<-rep(-99,length(CatchDisp2Base[,3]))
            }
            if("Season"%in%input$displayRadioButtonBase6 | length(unique(CatchDispBase[,2]))==1)
            {
              CatchDispBase[,2]<-rep(-99,length(CatchDispBase[,2]))
              CatchDisp2Base[,2]<-rep(-99,length(CatchDisp2Base[,2]))
            }
            if(input$displayRadioButtonBase=="Total")
            {
              clustModBase<-"Total"
              CatchDispBase[,4]<-rep(-99,length(CatchDispBase[,4]))
              CatchDispBase[,5]<-rep(-99,length(CatchDispBase[,5]))
              CatchDisp2Base[,4]<-rep(-99,length(CatchDisp2Base[,4]))
              CatchDisp2Base[,5]<-rep(-99,length(CatchDisp2Base[,5]))
            }else if(input$displayRadioButtonBase=="Groups"){
              CatchDispBase[,4]<-rep(-99,length(CatchDispBase[,4]))
              CatchDispBase<-CatchDispBase[CatchDispBase[,5]%in%as.numeric(input$displayRadioButtonBase4),]
              CatchDisp2Base[,4]<-rep(-99,length(CatchDisp2Base[,4]))
              CatchDisp2Base<-CatchDisp2Base[CatchDisp2Base[,5]%in%as.numeric(input$displayRadioButtonBase4),]
              clustModBase<-"Group"
            }else if(input$displayRadioButtonBase=="Fleets"){
              CatchDispBase[,5]<-rep(-99,length(CatchDispBase[,5]))
              CatchDispBase<-CatchDispBase[CatchDispBase[,4]%in%as.numeric(input$displayRadioButtonBase3),]
              CatchDisp2Base[,5]<-rep(-99,length(CatchDisp2Base[,5]))
              CatchDisp2Base<-CatchDisp2Base[CatchDisp2Base[,4]%in%as.numeric(input$displayRadioButtonBase3),]
              clustModBase<-"Fleet"
            }
            if(length(unique(CatchDispBase[,5]))==1){
              CatchDispBase[,5]<-rep(-99,length(CatchDispBase[,5]))
              CatchDisp2Base[,5]<-rep(-99,length(CatchDisp2Base[,5]))
            }

            CatchDispBase<-aggregate(CatchDispBase,list(year=CatchDispBase[,1],season=CatchDispBase[,2],area=CatchDispBase[,3],fleet=CatchDispBase[,4],group=CatchDispBase[,5]),sum)
            CatchDispBase<-cbind(CatchDispBase[,1:5],CatchDispBase[,11])
            names(CatchDispBase)<-potNamesCatchBase[dataColOutBase]
            CatchDispBase<-CatchDispBase[order(CatchDispBase[,3],CatchDispBase[,1],CatchDispBase[,2],CatchDispBase[,4]),]



            LegendValsBase<-aggregate(CatchDispBase[,2:5],list(season=CatchDispBase[,2],area=CatchDispBase[,3],fleet=CatchDispBase[,4],group=CatchDispBase[,5]),sum)
            LegendValsBase<-LegendValsBase[,1:4]
            LegendNamesBase<-LegendValsBase
            LegendNamesBase[,1]<-ifelse(LegendValsBase[,1]==-99,"", paste0(disp.Desc$SeasonNames[LegendValsBase[,1],2],", "))
            LegendNamesBase[,2]<-ifelse(LegendValsBase[,2]==-99,"", paste0(disp.Desc$AreaNames[LegendValsBase[,2],2],", "))
            LegendNamesBase[,3]<-ifelse(LegendValsBase[,3]==-99,"", disp.Desc$FleetNames[LegendValsBase[,3],2])
            LegendNamesBase[,4]<-ifelse(LegendValsBase[,4]==-99,"", paste0(disp.Desc$GroupNames[LegendValsBase[,4],2],", "))

            CatchTempBase<-CatchDispBase[(CatchDispBase[,1]>=xlimBase[1] & CatchDispBase[,1]<=xlimBase[2]) | (CatchDispBase[,1]>=xlimTargBase[1] & CatchDispBase[,1]<=xlimTargBase[2]),]
            displayValsBase<-unique(paste0(LegendNamesBase[,2],LegendNamesBase[,1],LegendNamesBase[,4],LegendNamesBase[,3]))
            if(length(displayValsBase)==1){
              if(displayValsBase==""){displayValsBase<-"Total"}}
            if(length(displayValsBase)>15){
              heightBase<-400*(length(displayValsBase)/15)
            }else{heightBase<-'auto'}

            eval(parse(text=paste0("output$",idBase4,"<-renderPlot({
                                   par(mar=c(0,0,0,0),oma=c(0,0,0,0))
                                   plot(NA,main='',xlab='',ylab='',xlim=c(0,1),ylim=c(0,1),axes=FALSE)
                                   legend(x=0,y=1,legend=displayValsBase,lty=1,lwd=2,col=colorChoicesBase[1:length(displayValsBase)],title='Display Categories')
          },height=heightBase)",collapse = "")))

            CatchTempBase<-CatchTempBase[is.finite(CatchTempBase[,6]),]
            ylimBase=c((min(CatchTempBase[,6],na.rm=TRUE)-0.05*max(abs(CatchTempBase[,6]),na.rm=TRUE)),(max(CatchTempBase[,6],na.rm=TRUE)+0.05*max(abs(CatchTempBase[,6]),na.rm=TRUE)))
            ylabBase<-paste0(catMod2Base," (",unitModBase,")")
            mainBase<-paste0(sourceModBase," Projected ",clustModBase," ",catModBase)


            eval(parse(text=paste0("output$",idBase1,"<-renderPlot({
                                   par(mar=c(5,4,4,0),oma=c(0,0,0,0))
                                   plot(NA,main=mainBase,xlab='',ylab='',xlim=xlimBase,ylim=ylimBase,axes=FALSE)
                                   axis(1,at=c(input$displayYearsBase[1]:input$displayYearsBase[2],(input$displayYearsBase[2]+2)),labels=FALSE,cex=0.5,pos=ylimBase[1])
                                   axis(1,at=c(labelPointBase,(input$displayYearsBase[2]+2)),labels=FALSE,cex=0.5,pos=ylimBase[1],lwd.ticks=2)
                                   axis(1,at=c(labelPointBase,(input$displayYearsBase[2]+2)),labels=c(labelPointBase,'Target'),cex=0.5,pos=(ylimBase[1]+0.02*(ylimBase[2]-ylimBase[1])),tick=FALSE)
                                   axis(2,pos=xlimBase[1],labels=FALSE)
                                   axis(2,pos=(xlimBase[1]+0.01*(xlimBase[2]-xlimBase[1])),tick=FALSE)
                                   title(xlab=xlabBase,ylab=ylabBase,line=1)
                                   polygon(x=c(xlimBase[1],xlimBase[1],xlimBase[2],xlimBase[2]),y=c(ylimBase[1],1.0*ylimBase[2],1.0*ylimBase[2],ylimBase[1]))
                                   for(i in 1:length(LegendValsBase[,1])){
                                   CatchTempBase2<-CatchTempBase[CatchTempBase[,2]==LegendValsBase[i,1] & CatchTempBase[,3]==LegendValsBase[i,2] & CatchTempBase[,4]==LegendValsBase[i,3] & CatchTempBase[,5]==LegendValsBase[i,4],]
                                   lines(x=CatchTempBase2[CatchTempBase2[,1]>=input$displayYearsBase[1] & CatchTempBase2[,1]<=input$displayYearsBase[2],1],y=CatchTempBase2[CatchTempBase2[,1]>=input$displayYearsBase[1] & CatchTempBase2[,1]<=input$displayYearsBase[2],6],lwd=2,col=colorChoicesBase[i])
                                   points(x=(input$displayYearsBase[2]+2),y=mean(CatchTempBase2[CatchTempBase2[,1]>= disp.Desc$TargetYears[1] & CatchTempBase2[,1]<=disp.Desc$TargetYears[2],6]),pch=16,col=colorChoicesBase[i])
                                   }
        })",collapse = "")))
          }
      }

      }
      })
      })
}

#' Build update quota
#'
#' Reads the assessment outputs and builds a GUI
#' plotting interface to display the important
#' results of the new updated assessment.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords interface management
buildUpdateQuota<-function(input, output, session){
  incProgress(0.1, detail = paste0("Building output results display"))


  if(input$ForecastTarget==6){
  get_Cost_Benefit<-function(Point_value=NULL,Val_series=NULL,Quant_Series=NULL){
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
  }
  removeUI("#currOutput>*",multiple=TRUE,immediate=TRUE)
  removeUI("#currOutput",multiple=TRUE,immediate=TRUE)
  newDiv("QuotaForecast","currOutput")
  newFluidRow("currOutput","forecastResults")
  #newFluidRow("currOutput","KobePlot")
  newColumn("forecastResults","NewQuotaInputs",width = 4)
  newFluidRow("NewQuotaInputs","DataChoiceInputs")
  newColumn("DataChoiceInputs","DataChoiceInputs2",width = 12)
  newFluidRow("NewQuotaInputs","TimeSeriesInputs")
  newColumn("TimeSeriesInputs","TimeSeriesInputs2",width = 12)
  newFluidRow("NewQuotaInputs","KobePlotInputs")
  newColumn("KobePlotInputs","KobePlotInputs2",width = 12)
  newFluidRow("NewQuotaInputs","yearRangeInputs")
  newColumn("yearRangeInputs","yearRangeInputs2",width = 12)
  newColumn("forecastResults","NewQuotaGraph",width = 8)
  newFluidRow("TimeSeriesInputs2","displayRadioOut")
  if(input$ForecastTarget==6){
    newRadio(location="DataChoiceInputs2",id="displayRadioButtonOut5",label="Projection dispay data:",choices=c("Landings","Dead Discards","Harvest Rate","Economics","SSB","Population Biomass","Recruits","Kobe"),selected="Landings",inline=TRUE)
  }else{
    newRadio(location="DataChoiceInputs2",id="displayRadioButtonOut5",label="Projection dispay data:",choices=c("Landings","Dead Discards","Harvest Rate","SSB","Population Biomass","Recruits","Kobe"),selected="Landings",inline=TRUE)
  }
  newRadio(location="TimeSeriesInputs2",id="displayRadioButtonOut1",label="Forecast results source:",choices=c("Base","Target","Implemented"),selected="Implemented",inline=TRUE)
  newRadio(location="TimeSeriesInputs2",id="displayRadioButtonOut7",label="Source relative to:",choices=c("Raw","Base","Target"),selected="Raw",inline=TRUE)
  newRadio(location="TimeSeriesInputs2",id="displayRadioButtonOut8",label="Difference format:",choices=c("Absolute","Percentage","Proportion"),selected="Absolute",inline=TRUE)
  newRadio(location="TimeSeriesInputs2",id="displayRadioButtonOut2",label="Units:",choices=c("Weight","Number"),selected="Weight",inline=TRUE)
  if(data.orig$nseas == 1 & data.orig$N_areas ==1){
    newtext(location="TimeSeriesInputs2",id="displayRadioButtonOut6",inputText="")
    hideElement(id="displayRadioButtonOut6")
  }else if(data.orig$nseas == 1){
    newCheckBoxGroup(location="TimeSeriesInputs2",id="displayRadioButtonOut6",label="Aggregate by:",choices=c("Area"),selected=c("Area"),inline=TRUE)
  }else if(data.orig$N_areas == 1){
    newCheckBoxGroup(location="TimeSeriesInputs2",id="displayRadioButtonOut6",label="Aggregate by:",choices=c("Season"),selected=c("Season"),inline=TRUE)
  }else{
    newCheckBoxGroup(location="TimeSeriesInputs2",id="displayRadioButtonOut6",label="Aggregate by:",choices=c("Area","Season"),selected=c("Area","Season"),inline=TRUE)
  }

  if(data.orig$Nfleet==1)
  {
    newRadio(location="TimeSeriesInputs2",id="displayRadioButtonOut",label="Partition fishery by:",choices=c("Total"),selected="Total",inline=TRUE)
  }else if(length(unique(forecast.orig$fleet_assignment_to_allocation_group))==1)
  {
    newRadio(location="TimeSeriesInputs2",id="displayRadioButtonOut",label="Partition fishery by:",choices=c("Total","Fleets"),selected="Total",inline=TRUE)
  }else
  {
    newRadio(location="TimeSeriesInputs2",id="displayRadioButtonOut",label="Partition fishery by:",choices=c("Total","Groups","Fleets"),selected="Total",inline=TRUE)
  }

  
  newRadio(location="TimeSeriesInputs2",id="displayRadioButtonOut9",label="Economic data to display:",choices=c("Costs","Benefits","Profits"),selected="Profits",inline=TRUE)
  
  
  color <- colors()[grep('gr(a|e)y', colors(), invert = T)]
  color <- color[8:433]
  color <- sample(color,length(color))
  color <- c("red","dark blue","dark green", "gold", "pink", "purple","black","cyan","brown",color)
  color <- unique(color)
  colorChoicesOut <- color

  newFluidRow("NewQuotaGraph","timeSeriesOut")
  newColumn("timeSeriesOut","timeSeriesOutPlots",width=8)
  newColumn("timeSeriesOut","timeSeriesOutLegendCol",width=4)

  runthis=TRUE
  incProgress(0.1, detail = paste0("Compliling catch data"))

  CatchOut1<-baseCatch
  CatchOut1<-cbind(CatchOut1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchOut1[,4]],CatchOut1[,5:15])
  names(CatchOut1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)")
  CatchOut1<-CatchOut1[order(CatchOut1[,3],CatchOut1[,1],CatchOut1[,2],CatchOut1[,4]),]

  CatchOutMSY1<-msyCatch
  CatchOutMSY1[,4]<-ifelse(CatchOutMSY1[,4]>data.orig$Nfleet,(CatchOutMSY1[,4]-data.orig$Nfleet),CatchOutMSY1[,4])
  CatchOutMSY1<-aggregate(CatchOutMSY1,list(year=CatchOutMSY1[,1],season=CatchOutMSY1[,2],area=CatchOutMSY1[,3],fleet=CatchOutMSY1[,4]),sum)
  CatchOutMSY1<-cbind(CatchOutMSY1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchOutMSY1[,4]],CatchOutMSY1[,9:19])
  names(CatchOutMSY1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)")
  CatchOutMSY1<-CatchOutMSY1[order(CatchOutMSY1[,3],CatchOutMSY1[,1],CatchOutMSY1[,2],CatchOutMSY1[,4]),]

  CatchOutRun1<-newCatch
  CatchOutRun1[,4]<-ifelse(CatchOutRun1[,4]>data.orig$Nfleet,(CatchOutRun1[,4]-data.orig$Nfleet),CatchOutRun1[,4])
  CatchOutRun1<-aggregate(CatchOutRun1,list(year=CatchOutRun1[,1],season=CatchOutRun1[,2],area=CatchOutRun1[,3],fleet=CatchOutRun1[,4]),sum)
  CatchOutRun1<-cbind(CatchOutRun1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchOutRun1[,4]],CatchOutRun1[,9:19])
  names(CatchOutRun1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)")
  CatchOutRun1<-CatchOutRun1[order(CatchOutRun1[,3],CatchOutRun1[,1],CatchOutRun1[,2],CatchOutRun1[,4]),]

  if(input$ForecastTarget==6){
    CatchOut1Cost<-CatchOut1[,1]
    CatchOut1Benefit<-CatchOut1[,1]
    CatchOutMSY1Cost<-CatchOut1[,1]
    CatchOutMSY1Benefit<-CatchOut1[,1]
    CatchOutRun1Cost<-CatchOut1[,1]
    CatchOutRun1Benefit<-CatchOut1[,1]
    
    for(i in unique(CatchOut1[,4])){
      if(data.run$units_of_catch[i]==1){
        catch_Col_temp<-6
        Catch_scale<-2204.62
      }else{
        catch_Col_temp<-8
        Catch_scale<-1000
      }
      CatchOut1Cost[which(CatchOut1[,4]==i)]<-vapply(CatchOut1[which(CatchOut1[,4]==i),11],get_Cost_Benefit,Val_series=CostMatrix[i,],Quant_Series=FSeq,FUN.VALUE = 1)
      CatchOut1Benefit[which(CatchOut1[,4]==i)]<-vapply(CatchOut1[which(CatchOut1[,4]==i),catch_Col_temp],get_Cost_Benefit,Val_series=Catch_scale*BenefitMatrix[i,],Quant_Series=Catch_scale*CatchSeq,FUN.VALUE = 1)
      CatchOutMSY1Cost[which(CatchOutMSY1[,4]==i)]<-vapply(CatchOutMSY1[which(CatchOutMSY1[,4]==i),11],get_Cost_Benefit,Val_series=CostMatrix[i,],Quant_Series=FSeq,FUN.VALUE = 1)
      CatchOutMSY1Benefit[which(CatchOutMSY1[,4]==i)]<-vapply(CatchOutMSY1[which(CatchOutMSY1[,4]==i),catch_Col_temp],get_Cost_Benefit,Val_series=Catch_scale*BenefitMatrix[i,],Quant_Series=Catch_scale*CatchSeq,FUN.VALUE = 1)
      CatchOutRun1Cost[which(CatchOutRun1[,4]==i)]<-vapply(CatchOutRun1[which(CatchOutRun1[,4]==i),11],get_Cost_Benefit,Val_series=CostMatrix[i,],Quant_Series=FSeq,FUN.VALUE = 1)
      CatchOutRun1Benefit[which(CatchOutRun1[,4]==i)]<-vapply(CatchOutRun1[which(CatchOutRun1[,4]==i),catch_Col_temp],get_Cost_Benefit,Val_series=Catch_scale*BenefitMatrix[i,],Quant_Series=Catch_scale*CatchSeq,FUN.VALUE = 1)
    }
    
    CatchOut1<-cbind(CatchOut1[,1:16],CatchOut1[,14:16])
    CatchOut1[,17]<-CatchOut1Cost
    CatchOut1[,18]<-CatchOut1Benefit
    CatchOut1[,19]<-CatchOut1Benefit-CatchOut1Cost
    
    CatchOutMSY1<-cbind(CatchOutMSY1[,1:16],CatchOutMSY1[,14:16])
    CatchOutMSY1[,17]<-CatchOutMSY1Cost
    CatchOutMSY1[,18]<-CatchOutMSY1Benefit
    CatchOutMSY1[,19]<-CatchOutMSY1Benefit-CatchOutMSY1Cost
    
    CatchOutRun1<-cbind(CatchOutRun1[,1:16],CatchOutRun1[,14:16])
    CatchOutRun1[,17]<-CatchOutRun1Cost
    CatchOutRun1[,18]<-CatchOutRun1Benefit
    CatchOutRun1[,19]<-CatchOutRun1Benefit-CatchOutRun1Cost
    
    names(CatchOut1)<-names(CatchOutMSY1)<-names(CatchOutRun1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)","Cost","Benefit","Profit")
  }
  
  lcatch<-length(CatchOutRun1[1,])
  CatchOutDiffMB1<-CatchOutRun1
  CatchOutDiffMB1[,6:lcatch]<-(CatchOutMSY1[,6:lcatch]-CatchOut1[,6:lcatch])

  CatchOutDiffRB1<-CatchOutRun1
  CatchOutDiffRB1[,6:lcatch]<-(CatchOutRun1[,6:lcatch]-CatchOut1[,6:lcatch])

  CatchOutDiffRM1<-CatchOutRun1
  CatchOutDiffRM1[,6:lcatch]<-(CatchOutRun1[,6:lcatch]-CatchOutMSY1[,6:lcatch])

  CatchOutPercMB1<-CatchOutRun1
  CatchOutPercMB1[,6:lcatch]<-100*(CatchOutMSY1[,6:lcatch]-CatchOut1[,6:lcatch])/CatchOut1[,6:lcatch]

  CatchOutPercRB1<-CatchOutRun1
  CatchOutPercRB1[,6:lcatch]<-100*(CatchOutRun1[,6:lcatch]-CatchOut1[,6:lcatch])/CatchOut1[,6:lcatch]

  CatchOutPercRM1<-CatchOutRun1
  CatchOutPercRM1[,6:lcatch]<-100*(CatchOutRun1[,6:lcatch]-CatchOutMSY1[,6:lcatch])/CatchOutMSY1[,6:lcatch]

  CatchOutPropMB1<-CatchOutRun1
  CatchOutPropMB1[,6:lcatch]<-CatchOutMSY1[,6:lcatch]/CatchOut1[,6:lcatch]

  CatchOutPropRB1<-CatchOutRun1
  CatchOutPropRB1[,6:lcatch]<-CatchOutRun1[,6:lcatch]/CatchOut1[,6:lcatch]

  CatchOutPropRM1<-CatchOutRun1
  CatchOutPropRM1[,6:lcatch]<-CatchOutRun1[,6:lcatch]/CatchOutMSY1[,6:lcatch]

  for(i in 6:lcatch)
  {
    CatchOutPercMB1[,i]<-ifelse(!is.finite(CatchOutPercMB1[,i]),0,CatchOutPercMB1[,i])
    CatchOutPercRB1[,i]<-ifelse(!is.finite(CatchOutPercRB1[,i]),0,CatchOutPercRB1[,i])
    CatchOutPercRM1[,i]<-ifelse(!is.finite(CatchOutPercRM1[,i]),0,CatchOutPercRM1[,i])
    CatchOutPropMB1[,i]<-ifelse(!is.finite(CatchOutPropMB1[,i]),1,CatchOutPropMB1[,i])
    CatchOutPropRB1[,i]<-ifelse(!is.finite(CatchOutPropRB1[,i]),1,CatchOutPropRB1[,i])
    CatchOutPropRM1[,i]<-ifelse(!is.finite(CatchOutPropRM1[,i]),1,CatchOutPropRM1[,i])
  }

  newCheckBoxGroup(location='TimeSeriesInputs2',id="displayRadioButtonOut4",label="Allocation Groups to Display",selected=as.numeric(disp.Desc$GroupNames[,1]),choiceNames=disp.Desc$GroupNames[,2],choiceValues=as.numeric(disp.Desc$GroupNames[,1]))
  newCheckBoxGroup(location='TimeSeriesInputs2',id="displayRadioButtonOut3",label="Fleets to Display",selected=as.numeric(disp.Desc$FleetNames[,1]),choiceNames=disp.Desc$FleetNames[,2],choiceValues=as.numeric(disp.Desc$FleetNames[,1]))
  newSlider(location='yearRangeInputs2',id="displayYearsOut",titleText = "Years to display", min=min(CatchOut1[,1]),max=max(CatchOut1[,1]),value = c((data.orig$endyr+1),data.orig$endyr+7),step=1,sep="")
  #newActionButton(location='yearRangeInputs2',id="saveOutput",label = "Save displayed figure/data")
  newRadio(location='yearRangeInputs2',id="downloadOptions",label="Item to download",selected=1,choiceNames = c("ICCAT","Displayed Figure with Data","SS3 Report file"),choiceValues = c(1,2,3))
  newtextInput(location='yearRangeInputs2',id="saveName",value=paste0(disp.Desc$Title),label = "Saved figure/data name")
  newDownload(location='yearRangeInputs2',id="downloadOutput1",label="Download ICCAT")
  newDownload(location='yearRangeInputs2',id="downloadOutput2",label="Download Figure/Data")
  newDownload(location='yearRangeInputs2',id="downloadOutput3",label="Download SS3 Report")

  observerPool[[21]]<<-observe({
    if(input$ForecastTarget==6){
      if(!is.null(input$displayRadioButtonOut5)){
        if(input$displayRadioButtonOut5=="Economics"){
          showElement(id="displayRadioButtonOut9")
        }else{
          hideElement(id="displayRadioButtonOut9")
        }
      }else{
        hideElement(id="displayRadioButtonOut9")
      }
    }else{
      hideElement(id="displayRadioButtonOut9")
    }
    if(is.null(input$downloadOptions)){
      hideElement(id="downloadOutput1")
      hideElement(id="downloadOutput2")
      hideElement(id="downloadOutput3")
      hideElement(id="saveName")
    }else if(input$downloadOptions==1){
      showElement(id="downloadOutput1")
      hideElement(id="downloadOutput2")
      hideElement(id="downloadOutput3")
      hideElement(id="saveName")
    }else if(input$downloadOptions==2){
      hideElement(id="downloadOutput1")
      showElement(id="downloadOutput2")
      hideElement(id="downloadOutput3")
      showElement(id="saveName")
    }else if(input$downloadOptions==3){
      hideElement(id="downloadOutput1")
      hideElement(id="downloadOutput2")
      showElement(id="downloadOutput3")
      hideElement(id="saveName")
    }
  })

  CatchMSYKobe<-CatchOutMSY1
  KobeMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],6])/(input$TargetYears[2]-input$TargetYears[1]+1)
  KobeSSBMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],13])/(input$TargetYears[2]-input$TargetYears[1]+1)
  KobeBMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],15])/(input$TargetYears[2]-input$TargetYears[1]+1)
  KobeFMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],16])/(input$TargetYears[2]-input$TargetYears[1]+1)
  KobeY1<-sum(CatchMSYKobe[CatchMSYKobe[,1]==input$ManagementYearInput,6])
  KobeSSB1<-sum(CatchMSYKobe[CatchMSYKobe[,1]==input$ManagementYearInput,13])
  KobeB1<-sum(CatchMSYKobe[CatchMSYKobe[,1]==input$ManagementYearInput,15])
  KobeF1<-sum(CatchMSYKobe[CatchMSYKobe[,1]==input$ManagementYearInput,16])
  KobeY5<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+4),6])
  KobeSSB5<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+4),13])
  KobeB5<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+4),15])
  KobeF5<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+4),16])
  KobeY10<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+9),6])
  KobeSSB10<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+9),13])
  KobeB10<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+9),15])
  KobeF10<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+9),16])
  KobeY20<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+19),6])
  KobeSSB20<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+19),13])
  KobeB20<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+19),15])
  KobeF20<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+19),16])
  KobeY2070<-sum(CatchMSYKobe[CatchMSYKobe[,1]==2070,6])
  KobeSSB2070<-sum(CatchMSYKobe[CatchMSYKobe[,1]==2070,13])
  KobeB2070<-sum(CatchMSYKobe[CatchMSYKobe[,1]==2070,15])
  KobeF2070<-sum(CatchMSYKobe[CatchMSYKobe[,1]==2070,16])

  CatchNewKobe<-CatchOutRun1
  CatchNewKobe<-aggregate(CatchNewKobe,list(year=CatchNewKobe[,1]),sum)
  CatchNewKobe<-CatchNewKobe[,c(1,17,16)]
  CatchNewKobe[,2]<-CatchNewKobe[,2]/KobeFMSY
  CatchNewKobe[,3]<-CatchNewKobe[,3]/KobeBMSY

  AppliedKobeY1<-sum(CatchOutRun1[CatchOutRun1[,1]==input$ManagementYearInput,6])
  AppliedKobeSSB1<-sum(CatchOutRun1[CatchOutRun1[,1]==input$ManagementYearInput,13])
  AppliedKobeB1<-sum(CatchOutRun1[CatchOutRun1[,1]==input$ManagementYearInput,15])
  AppliedKobeF1<-sum(CatchOutRun1[CatchOutRun1[,1]==input$ManagementYearInput,16])
  AppliedKobeY5<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+4),6])
  AppliedKobeSSB5<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+4),13])
  AppliedKobeB5<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+4),15])
  AppliedKobeF5<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+4),16])
  AppliedKobeY10<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+9),6])
  AppliedKobeSSB10<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+9),13])
  AppliedKobeB10<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+9),15])
  AppliedKobeF10<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+9),16])
  AppliedKobeY20<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+19),6])
  AppliedKobeSSB20<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+19),13])
  AppliedKobeB20<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+19),15])
  AppliedKobeF20<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+19),16])
  AppliedKobeY2070<-sum(CatchOutRun1[CatchOutRun1[,1]==2070,6])
  AppliedKobeSSB2070<-sum(CatchOutRun1[CatchOutRun1[,1]==2070,13])
  AppliedKobeB2070<-sum(CatchOutRun1[CatchOutRun1[,1]==2070,15])
  AppliedKobeF2070<-sum(CatchOutRun1[CatchOutRun1[,1]==2070,16])


  idOut1<-"timeSeriesPlotsOut"
  locationOut1<-"timeSeriesOutPlots"
  idOut4<-"timeSeriesLegendOut"
  locationOut4<-"timeSeriesOutLegendCol"
  xlabOut="Year"
  newPlot(location=locationOut1,id=idOut1)
  newPlot(location=locationOut4,id=idOut4)
  incProgress(0.1, detail = paste0("Building output figures"))


  output$downloadOutput1<-downloadHandler(
    filename = "ICCAT_values.csv",
    content =function(file){
      outVect1<-c(KobeMSY,KobeSSBMSY,KobeBMSY,KobeFMSY,KobeY1,KobeSSB1,KobeB1,KobeF1,KobeY5,KobeSSB5,KobeB5,KobeF5,KobeY10,KobeSSB10,KobeB10,KobeF10,KobeY20,KobeSSB20,KobeB20,KobeF20,KobeY2070,KobeSSB2070,KobeB2070,KobeF2070,KobeY1/KobeMSY,KobeSSB1/KobeSSBMSY,KobeB1/KobeBMSY,KobeF1/KobeFMSY,KobeY5/KobeMSY,KobeSSB5/KobeSSBMSY,KobeB5/KobeBMSY,KobeF5/KobeFMSY,KobeY10/KobeMSY,KobeSSB10/KobeSSBMSY,KobeB10/KobeBMSY,KobeF10/KobeFMSY,KobeY20/KobeMSY,KobeSSB20/KobeSSBMSY,KobeB20/KobeBMSY,KobeF20/KobeFMSY)
      outVect2<-c(KobeMSY,KobeSSBMSY,KobeBMSY,KobeFMSY,AppliedKobeY1,AppliedKobeSSB1,AppliedKobeB1,AppliedKobeF1,AppliedKobeY5,AppliedKobeSSB5,AppliedKobeB5,AppliedKobeF5,AppliedKobeY10,AppliedKobeSSB10,AppliedKobeB10,AppliedKobeF10,AppliedKobeY20,AppliedKobeSSB20,AppliedKobeB20,AppliedKobeF2070,AppliedKobeY2070,AppliedKobeSSB2070,AppliedKobeB2070,AppliedKobeF2070,AppliedKobeY1/KobeMSY,AppliedKobeSSB1/KobeSSBMSY,AppliedKobeB1/KobeBMSY,AppliedKobeF1/KobeFMSY,AppliedKobeY5/KobeMSY,AppliedKobeSSB5/KobeSSBMSY,AppliedKobeB5/KobeBMSY,AppliedKobeF5/KobeFMSY,AppliedKobeY10/KobeMSY,AppliedKobeSSB10/KobeSSBMSY,AppliedKobeB10/KobeBMSY,AppliedKobeF10/KobeFMSY,AppliedKobeY20/KobeMSY,AppliedKobeSSB20/KobeSSBMSY,AppliedKobeB20/KobeBMSY,AppliedKobeF20/KobeFMSY)
      outMatrix<-as.data.frame(matrix(c(outVect1,outVect2),nrow=2,ncol=40,byrow = TRUE))
      names(outMatrix)<-c("Yieldequil","SSBequi","Bequil","Fequil","Yield1yr","SSB1yr","B1yr","F1yr","Yield5yr","SSB5yr","B5yr","F5yr","Yield10yr","SSB10yr","B10yr","F10yr","Yield20yr","SSB20yr","B20yr","F20yr","Yield2070","SSB2070","B2070","F2070","Yield1yr/MSY","SSB1yr/SSBmsy","B1yr/Bmsy","F1yr/Fmsy","Yield5yr/MSY","SSB5yr/SSBmsy","B5yr/Bmsy","F5yr/Fmsy","Yield10yr/MSY","SSB10yr/SSBmsy","B10yr/Bmsy","F10yr/Fmsy","Yield20yr/MSY","SSB20yr/SSBmsy","B20yr/Bmsy","F20yr/Fmsy")
      row.names(outMatrix)<-c("MSY","Applied")
      write.csv(outMatrix,file,row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$downloadOutput2<-downloadHandler(
    filename = "Display_Figure_Data.zip",
    content =function(file){
      trueWD<-getwd()
      setwd(paste0(dir.base,"/outputs/"))

      file1<-paste0(input$saveName,"Plot.png")
      file2<-paste0(input$saveName,"Legend.png")

      file3<-paste0(input$saveName,"DataKobe.csv")

      file4<-paste0(input$saveName,"DataDetailed.csv")
      file5<-paste0(input$saveName,"DataPlot.csv")
      files<-c(file1,file2,file3,file4,file5)
      unlink(files)

      if(input$displayRadioButtonOut5=="Kobe"){
        files<-c(file1,file2,file3)
      }else{
        files<-c(file1,file2,file4,file5)
      }

      saveCurrResults(input,output,session,input$saveName)
      zip(file,files)
      setwd(trueWD)
    },
    contentType =  "application/zip"
  )

  output$downloadOutput3<-downloadHandler(
    filename = "SS3_Report.txt",
    content =function(file){
      file.copy(paste0(dir.run,"/Report.sso"),file)
    },
    contentType = "text/plain"
  )

  observerPool[[15]]<<-NULL
  observerPool[[15]]<<-observe({
    #print("running oberserver 6")
    withProgress(message="Creating output results figures",value=0.5,{
      if(!is.null(input$displayRadioButtonOut) &
         !is.null(input$displayRadioButtonOut1) &
         !is.null(input$displayRadioButtonOut2) &
         !is.null(input$displayRadioButtonOut3) &
         !is.null(input$displayRadioButtonOut4) &
         !is.null(input$displayRadioButtonOut5) &
         !is.null(input$displayRadioButtonOut7) &
         !is.null(input$displayRadioButtonOut8) &
         !is.null(input$displayRadioButtonOut9) &
         !is.null(input$displayYearsOut))
      {
        xlimOut=c((input$displayYearsOut[1]-1),(input$displayYearsOut[2]+3))
        xlimTargOut<-xlimOut
        if(!is.null(disp.Desc$TargetYears))
        {
          xlimTargOut<-disp.Desc$TargetYears
        }
        labelPointOut<-seq(input$displayYearsOut[1],input$displayYearsOut[2],max(1,floor((input$displayYearsOut[2]-input$displayYearsOut[1])/4)))
        if((input$displayYearsOut[2]-input$displayYearsOut[1])>=10){
          labelPointOut<-labelPointOut[-length(labelPointOut)]
        }
        makePlot<-TRUE
        makeKobe<-FALSE

        if(input$displayRadioButtonOut5=="Kobe"){
          makeKobe<-TRUE
          makePlot<-FALSE
          displayKobe<-CatchNewKobe[CatchNewKobe[,1]>=input$displayYearsOut[1] & CatchNewKobe[,1]<=input$displayYearsOut[2],]
          if(!is.null(input$displayYearsOut)){
            xlimKobe<-c(0,max(2,1.05*displayKobe[,3]))
            ylimKobe<-c(0,max(2,1.05*displayKobe[,2]))
            steps<-seq(0.1,0.8,length.out = length(displayKobe[,1]))
            legendDispYears<-unique(floor(seq(displayKobe[1,1],displayKobe[length(displayKobe[,1]),1],length.out = 5)))
            legendSteps<-steps[displayKobe[,1]%in%legendDispYears]
            #legendSteps<-seq(0.1,0.8,length.out = length(legendDispYears))
            eval(parse(text=paste0("output$",idOut1,"<-renderPlot({
                                   plot(NA,main='Kobe Plot new projection',xlab='B/Bmsy',ylab='F/Fmsy',xlim=xlimKobe,ylim=ylimKobe,axes=FALSE)
                                   axis(1,at=seq(0,xlimKobe[2],0.05),labels=FALSE,cex=0.5,pos=ylimKobe[1])
                                   axis(1,at=seq(0,xlimKobe[2],0.2),labels=FALSE,cex=0.5,pos=ylimKobe[1],lwd.ticks=2)
                                   axis(1,at=seq(0,xlimKobe[2],0.2),labels=TRUE,cex=0.5,pos=(ylimKobe[1]+0.02*(ylimKobe[2]-ylimKobe[1])),tick=FALSE)
                                   axis(2,at=seq(0,ylimKobe[2],0.05),pos=xlimKobe[1],labels=FALSE)
                                   axis(2,at=seq(0,ylimKobe[2],0.2),pos=(xlimKobe[1]+0.01*(xlimKobe[2]-xlimKobe[1])),tick=FALSE)
                                   polygon(x=c(xlimKobe[1],xlimKobe[1],xlimKobe[2],xlimKobe[2]),y=c(ylimKobe[1],1.0*ylimKobe[2],1.0*ylimKobe[2],ylimKobe[1]),lwd=2)
                                   polygon(x=c(xlimKobe[1],xlimKobe[1],1,1),y=c(ylimKobe[1],1,1,ylimKobe[1]),border=NA,col='yellow')
                                   polygon(x=c(1,1,xlimKobe[2],xlimKobe[2]),y=c(1,1.0*ylimKobe[2],1.0*ylimKobe[2],1),border=NA,col='yellow')
                                   polygon(x=c(xlimKobe[1],xlimKobe[1],1,1),y=c(1,1.0*ylimKobe[2],1.0*ylimKobe[2],1),border=NA,col='red')
                                   polygon(x=c(1,1,xlimKobe[2],xlimKobe[2]),y=c(ylimKobe[1],1,1,ylimKobe[1]),border=NA,col='green')
                                   lines(x=xlimKobe,y=c(1,1),lwd=2,col='black')
                                   lines(x=c(1,1),y=ylimKobe,lwd=2,col='black')
                                   for(i in 1:length(displayKobe[,3])){
                                   lines(x=displayKobe[i:length(displayKobe[,3]),3],y=displayKobe[i:length(displayKobe[,3]),2],lwd=2,col=gray(level=steps[i]))
                                   }
                                   points(x=displayKobe[,3],y=displayKobe[,2],cex=1,pch=16,col=gray(level=steps))
                                   points(x=displayKobe[displayKobe[,1]%in%legendDispYears,3],y=displayKobe[displayKobe[,1]%in%legendDispYears,2],cex=2,pch=16,col=gray(level=legendSteps))
          })",collapse = "")))

            if(length(displayKobe[,1])>1){
              legendDispYears<-unique(floor(seq(displayKobe[1,1],displayKobe[length(displayKobe[,1]),1],length.out = 5)))
              legendSteps<-seq(0.1,0.8,length.out = length(legendDispYears))
              eval(parse(text=paste0("output$",idOut4,"<-renderPlot({
                                     plot(NA,main='',xlab='',ylab='',xlim=c(0,1),ylim=c(0,1),axes=FALSE)
                                     legend(x='left',legend=legendDispYears,pch=16,cex=2,col=gray(level=legendSteps),title='Years')
            })",collapse = "")))}}

        }else{
          if(input$displayRadioButtonOut5=="Harvest Rate")
          {
            unitMod<-"F"
            catMod<-"Harvest Rate"
            catMod2<-"Harvest Rate"
            dataColOut<-c(1:5,16)
          }else if(input$displayRadioButtonOut5=="Economics")
          {
            unitMod<-"Dollars"
            if(input$displayRadioButtonOut9=="Costs"){
              catMod<-"Gross Costs"
              catMod2<-"Gross Costs"
              dataColOut<-c(1:5,17)
            }else if(input$displayRadioButtonOut9=="Benefits"){
              catMod<-"Gross Revenue"
              catMod2<-"Gross Revenue"
              dataColOut<-c(1:5,18)
            }else if(input$displayRadioButtonOut9=="Profits"){
              catMod<-"Net Profits"
              catMod2<-"Net Profits"
              dataColOut<-c(1:5,19)
            }
            
          }else if(input$displayRadioButtonOut5!="Landings" & input$displayRadioButtonOut5!="Dead Discards")
          {
            unitMod<-weightNames[disp.Desc$Units[1]]
            if(input$displayRadioButtonOut5=="SPB"){
              catMod<-"Equilibrium Spawning Potential Biomass"
              catMod2<-"Equilibrium Spawning Potential Biomass"
              dataColOut<-c(1:5,12)
            }else if(input$displayRadioButtonOut5=="SSB"){
              catMod<-"Spawning Stock Biomass"
              catMod2<-"Spawning Stock Biomass"
              dataColOut<-c(1:5,13)
            }else if(input$displayRadioButtonOut5=="Recruits"){
              catMod<-"Recruitment"
              catMod2<-"Recruitment"
              dataColOut<-c(1:5,14)
            }else if(input$displayRadioButtonOut5=="Population Biomass"){
              catMod<-"Total Population Biomass"
              catMod2<-"Total Population Biomass"
              dataColOut<-c(1:5,15)
            }
          }else if(input$displayRadioButtonOut5=="Landings")
          {
            if(input$displayRadioButtonOut2=="Weight")
            {
              dataColOut<-c(1:5,6)
              unitMod<-weightNames[disp.Desc$Units[1]]
            }else if(input$displayRadioButtonOut2=="Number")
            {
              dataColOut<-c(1:5,8)
              unitMod<-"1000's"
            }
            catMod<-"Catch"
            catMod2<-"Catch"
          }else if(input$displayRadioButtonOut5=="Dead Discards")
          {
            if(input$displayRadioButtonOut2=="Weight")
            {
              dataColOut<-c(1:5,7)
              unitMod<-weightNames[disp.Desc$Units[1]]
            }else if(input$displayRadioButtonOut2=="Number")
            {
              dataColOut<-c(1:5,9)
              unitMod<-"1000's"
            }
            catMod<-"Dead Discards"
            catMod2<-"Dead Discards"
          }
          CatchDisp<-NULL
          CatchDisp2<-NULL
          if(input$displayRadioButtonOut7=="Raw"){
            if(input$displayRadioButtonOut1=="Base")
            {
              CatchDisp<-CatchOut1
              CatchDisp2<-CatchOut1
              sourceMod<-"Base"
            }else if(input$displayRadioButtonOut1=="Target")
            {
              CatchDisp<-CatchOutMSY1
              CatchDisp2<-CatchOut1
              sourceMod<-"Target"
            }else if(input$displayRadioButtonOut1=="Implemented")
            {
              CatchDisp<-CatchOutRun1
              CatchDisp2<-CatchOut1
              sourceMod<-"Implemented"
            }else
            {
              makePlot<-FALSE
            }
          }else if(input$displayRadioButtonOut7=="Base"){
            if(input$displayRadioButtonOut1=="Target")
            {
              if(input$displayRadioButtonOut8=="Absolute"){
                CatchDisp<-CatchOutDiffMB1
                CatchDisp2<-CatchOut1
                catMod2<-paste0("Change ",catMod2)
                sourceMod<-"Change from Base to Target in "
              }else if(input$displayRadioButtonOut8=="Percentage"){
                CatchDisp<-CatchOutPercMB1
                CatchDisp2<-CatchOut1
                catMod2<-paste0("Change ",catMod2)
                unitMod<-"%"
                sourceMod<-"Percent Change from Base to Target in "
              }else if(input$displayRadioButtonOut8=="Proportion"){
                CatchDisp<-CatchOutPropMB1
                CatchDisp2<-CatchOut1
                catMod2<-paste0("Relative ",catMod2)
                unitMod<-"ratio"
                sourceMod<-"Target proportion of Base for "
              }
            }else if(input$displayRadioButtonOut1=="Implemented")
            {
              if(input$displayRadioButtonOut8=="Absolute"){
                CatchDisp<-CatchOutDiffRB1
                CatchDisp2<-CatchOut1
                catMod2<-paste0("Change ",catMod2)
                sourceMod<-"Change from Base to Implemented in "
              }else if(input$displayRadioButtonOut8=="Percentage"){
                CatchDisp<-CatchOutPercRB1
                CatchDisp2<-CatchOut1
                catMod2<-paste0("Change ",catMod2)
                unitMod<-"%"
                sourceMod<-"Percent Change from Base to Implemented in "
              }else if(input$displayRadioButtonOut8=="Proportion"){
                CatchDisp<-CatchOutPropRB1
                CatchDisp2<-CatchOut1
                catMod2<-paste0("Relative ",catMod2)
                unitMod<-"ratio"
                sourceMod<-"Implemented proportion of Base for "
              }
            }else
            {
              makePlot<-FALSE
            }
          }else if(input$displayRadioButtonOut7=="Target"){
            if(input$displayRadioButtonOut1=="Implemented")
            {
              if(input$displayRadioButtonOut8=="Absolute"){
                CatchDisp<-CatchOutDiffRM1
                CatchDisp2<-CatchOut1
                catMod2<-paste0("Change ",catMod2)
                sourceMod<-"Change from Target to Implemented in "
              }else if(input$displayRadioButtonOut8=="Percentage"){
                CatchDisp<-CatchOutPercRM1
                CatchDisp2<-CatchOutMSY1
                catMod2<-paste0("Change ",catMod2)
                unitMod<-"%"
                sourceMod<-"Percent Change from Target to Implemented in "
              }else if(input$displayRadioButtonOut8=="Proportion"){
                CatchDisp<-CatchOutPropRM1
                CatchDisp2<-CatchOutMSY1
                catMod2<-paste0("Relative ",catMod2)
                unitMod<-"ratio"
                sourceMod<-"Implemented proportion of Target for "
              }
            }else
            {
              makePlot<-FALSE
            }
          }

          if(makePlot==TRUE)
          {
            if(input$ForecastTarget==6){
              potNamesCatch<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","HarvestRate(F)","Cost","Benefit","Profit")
            }else{
              potNamesCatch<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","HarvestRate(F)")
            }
            
            CatchDisp<-CatchDisp[,dataColOut]
            CatchDisp2<-CatchDisp2[,dataColOut]
            if("Area"%in%input$displayRadioButtonOut6 | length(unique(CatchDisp[,3]))==1)
            {
              CatchDisp[,3]<-rep(-99,length(CatchDisp[,3]))
              CatchDisp2[,3]<-rep(-99,length(CatchDisp2[,3]))
            }
            if("Season"%in%input$displayRadioButtonOut6 | length(unique(CatchDisp[,2]))==1)
            {
              CatchDisp[,2]<-rep(-99,length(CatchDisp[,2]))
              CatchDisp2[,2]<-rep(-99,length(CatchDisp2[,2]))
            }
            if(input$displayRadioButtonOut=="Total")
            {
              clustMod<-"Total"
              CatchDisp[,4]<-rep(-99,length(CatchDisp[,4]))
              CatchDisp[,5]<-rep(-99,length(CatchDisp[,5]))
              CatchDisp2[,4]<-rep(-99,length(CatchDisp2[,4]))
              CatchDisp2[,5]<-rep(-99,length(CatchDisp2[,5]))
            }else if(input$displayRadioButtonOut=="Groups"){
              CatchDisp[,4]<-rep(-99,length(CatchDisp[,4]))
              CatchDisp<-CatchDisp[CatchDisp[,5]%in%as.numeric(input$displayRadioButtonOut4),]
              CatchDisp2[,4]<-rep(-99,length(CatchDisp2[,4]))
              CatchDisp2<-CatchDisp2[CatchDisp2[,5]%in%as.numeric(input$displayRadioButtonOut4),]
              clustMod<-"Group"
            }else if(input$displayRadioButtonOut=="Fleets"){
              CatchDisp[,5]<-rep(-99,length(CatchDisp[,5]))
              CatchDisp<-CatchDisp[CatchDisp[,4]%in%as.numeric(input$displayRadioButtonOut3),]
              CatchDisp2[,5]<-rep(-99,length(CatchDisp2[,5]))
              CatchDisp2<-CatchDisp2[CatchDisp2[,4]%in%as.numeric(input$displayRadioButtonOut3),]
              clustMod<-"Fleet"
            }
            if(length(unique(CatchDisp[,5]))==1){
              CatchDisp[,5]<-rep(-99,length(CatchDisp[,5]))
              CatchDisp2[,5]<-rep(-99,length(CatchDisp2[,5]))
            }
            if(input$displayRadioButtonOut8=="Proportion" | input$displayRadioButtonOut8=="Percentage"){
              CatchDisp[,6]<-CatchDisp[,6]*CatchDisp2[,6]
              CatchDisp<-aggregate(CatchDisp,list(year=CatchDisp[,1],season=CatchDisp[,2],area=CatchDisp[,3],fleet=CatchDisp[,4],group=CatchDisp[,5]),sum)
              CatchDisp<-cbind(CatchDisp[,1:5],CatchDisp[,11])
              names(CatchDisp)<-potNamesCatch[dataColOut]
              CatchDisp<-CatchDisp[order(CatchDisp[,3],CatchDisp[,1],CatchDisp[,2],CatchDisp[,4]),]

              CatchDisp2<-aggregate(CatchDisp2,list(year=CatchDisp2[,1],season=CatchDisp2[,2],area=CatchDisp2[,3],fleet=CatchDisp2[,4],group=CatchDisp2[,5]),sum)
              CatchDisp2<-cbind(CatchDisp2[,1:5],CatchDisp2[,11])
              names(CatchDisp2)<-potNamesCatch[dataColOut]
              CatchDisp2<-CatchDisp2[order(CatchDisp2[,3],CatchDisp2[,1],CatchDisp2[,2],CatchDisp2[,4]),]

              CatchDisp[,6]<-CatchDisp[,6]/CatchDisp2[,6]
            }else{
              CatchDisp<-aggregate(CatchDisp,list(year=CatchDisp[,1],season=CatchDisp[,2],area=CatchDisp[,3],fleet=CatchDisp[,4],group=CatchDisp[,5]),sum)
              CatchDisp<-cbind(CatchDisp[,1:5],CatchDisp[,11])
              names(CatchDisp)<-potNamesCatch[dataColOut]
              CatchDisp<-CatchDisp[order(CatchDisp[,3],CatchDisp[,1],CatchDisp[,2],CatchDisp[,4]),]
            }


            LegendVals<-aggregate(CatchDisp[,2:5],list(season=CatchDisp[,2],area=CatchDisp[,3],fleet=CatchDisp[,4],group=CatchDisp[,5]),sum)
            LegendVals<-LegendVals[,1:4]
            LegendNames<-LegendVals
            LegendNames[,1]<-ifelse(LegendVals[,1]==-99,"", paste0(disp.Desc$SeasonNames[LegendVals[,1],2],", "))
            LegendNames[,2]<-ifelse(LegendVals[,2]==-99,"", paste0(disp.Desc$AreaNames[LegendVals[,2],2],", "))
            LegendNames[,3]<-ifelse(LegendVals[,3]==-99,"", disp.Desc$FleetNames[LegendVals[,3],2])
            LegendNames[,4]<-ifelse(LegendVals[,4]==-99,"", paste0(disp.Desc$GroupNames[LegendVals[,4],2],", "))

            CatchTempOut<-CatchDisp[(CatchDisp[,1]>=xlimOut[1] & CatchDisp[,1]<=xlimOut[2]) | (CatchDisp[,1]>=xlimTargOut[1] & CatchDisp[,1]<=xlimTargOut[2]),]
            displayValsOut<-unique(paste0(LegendNames[,2],LegendNames[,1],LegendNames[,4],LegendNames[,3]))
            if(length(displayValsOut)==1){
              if(displayValsOut==""){displayValsOut<-"Total"}}
            if(length(displayValsOut)>15){
              height<-400*(length(displayValsOut)/15)
            }else{height<-'auto'}

            eval(parse(text=paste0("output$",idOut4,"<-renderPlot({
                                   par(mar=c(0,0,0,0),oma=c(0,0,0,0))
                                   plot(NA,main='',xlab='',ylab='',xlim=c(0,1),ylim=c(0,1),axes=FALSE)
                                   legend(x=0,y=1,legend=displayValsOut,lty=1,lwd=2,col=colorChoicesOut[1:length(displayValsOut)],title='Display Categories')
          },height=height)",collapse = "")))

            CatchTempOut<-CatchTempOut[is.finite(CatchTempOut[,6]),]
            ylimOut=c((min(CatchTempOut[,6],na.rm=TRUE)-0.05*max(abs(CatchTempOut[,6]),na.rm=TRUE)),(max(CatchTempOut[,6],na.rm=TRUE)+0.05*max(abs(CatchTempOut[,6]),na.rm=TRUE)))
            ylabOut<-paste0(catMod2," (",unitMod,")")
            mainOut<-paste0(sourceMod," Projected ",clustMod," ",catMod)


            eval(parse(text=paste0("output$",idOut1,"<-renderPlot({
                                   par(mar=c(5,4,4,0),oma=c(0,0,0,0))
                                   plot(NA,main=mainOut,xlab='',ylab='',xlim=xlimOut,ylim=ylimOut,axes=FALSE)
                                   axis(1,at=c(input$displayYearsOut[1]:input$displayYearsOut[2],(input$displayYearsOut[2]+2)),labels=FALSE,cex=0.5,pos=ylimOut[1])
                                   axis(1,at=c(labelPointOut,(input$displayYearsOut[2]+2)),labels=FALSE,cex=0.5,pos=ylimOut[1],lwd.ticks=2)
                                   axis(1,at=c(labelPointOut,(input$displayYearsOut[2]+2)),labels=c(labelPointOut,'Target'),cex=0.5,pos=(ylimOut[1]+0.02*(ylimOut[2]-ylimOut[1])),tick=FALSE)
                                   axis(2,pos=xlimOut[1],labels=FALSE)
                                   axis(2,pos=(xlimOut[1]+0.01*(xlimOut[2]-xlimOut[1])),tick=FALSE)
                                   title(xlab=xlabOut,ylab=ylabOut,line=1)
                                   polygon(x=c(xlimOut[1],xlimOut[1],xlimOut[2],xlimOut[2]),y=c(ylimOut[1],1.0*ylimOut[2],1.0*ylimOut[2],ylimOut[1]))
                                   for(i in 1:length(LegendVals[,1])){
                                   CatchTempOut2<-CatchTempOut[CatchTempOut[,2]==LegendVals[i,1] & CatchTempOut[,3]==LegendVals[i,2] & CatchTempOut[,4]==LegendVals[i,3] & CatchTempOut[,5]==LegendVals[i,4],]
                                   lines(x=CatchTempOut2[CatchTempOut2[,1]>=input$displayYearsOut[1] & CatchTempOut2[,1]<=input$displayYearsOut[2],1],y=CatchTempOut2[CatchTempOut2[,1]>=input$displayYearsOut[1] & CatchTempOut2[,1]<=input$displayYearsOut[2],6],lwd=2,col=colorChoicesOut[i])
                                   points(x=(input$displayYearsOut[2]+2),y=mean(CatchTempOut2[CatchTempOut2[,1]>=input$TargetYears[1] & CatchTempOut2[,1]<=input$TargetYears[2],6]),pch=16,col=colorChoicesOut[i])
                                   }
      })",collapse = "")))
      }
  }

}
  })
  })
    }

#' Build Kobe Matrix
#'
#' Iterates over a numer of assessments and fixed catch
#' rates to calculate a probability matrix of overfishing
#' and overfished likelihoods.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @param iteration current iteration of loop.
#' @param MSYoutput Null in first iteration where MSY
#' calculated. For later iterations this is the output
#' of the first iteration.
#' @author Nathan Vaughan
#' @keywords interface management
buildKobeMatrix<-function(input,output,session,iteration,MSYoutput=NULL){
  if(iteration==1){
    CatchOutMSY1<-msyCatch
    CatchOutMSY1[,4]<-ifelse(CatchOutMSY1[,4]>data.orig$Nfleet,(CatchOutMSY1[,4]-data.orig$Nfleet),CatchOutMSY1[,4])
    CatchOutMSY1<-aggregate(CatchOutMSY1,list(year=CatchOutMSY1[,1],season=CatchOutMSY1[,2],area=CatchOutMSY1[,3],fleet=CatchOutMSY1[,4]),sum)
    CatchOutMSY1<-cbind(CatchOutMSY1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchOutMSY1[,4]],CatchOutMSY1[,9:19])
    names(CatchOutMSY1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)")
    CatchOutMSY1<-CatchOutMSY1[order(CatchOutMSY1[,3],CatchOutMSY1[,1],CatchOutMSY1[,2],CatchOutMSY1[,4]),]

    CatchMSYKobe<-CatchOutMSY1
    KobeMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],6])/(input$TargetYears[2]-input$TargetYears[1]+1)
    KobeSSBMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],13])/(input$TargetYears[2]-input$TargetYears[1]+1)
    KobeBMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],15])/(input$TargetYears[2]-input$TargetYears[1]+1)
    KobeFMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],16])/(input$TargetYears[2]-input$TargetYears[1]+1)
    KobeY1<-sum(CatchMSYKobe[CatchMSYKobe[,1]==input$ManagementYearInput,6])
    KobeSSB1<-sum(CatchMSYKobe[CatchMSYKobe[,1]==input$ManagementYearInput,13])
    KobeB1<-sum(CatchMSYKobe[CatchMSYKobe[,1]==input$ManagementYearInput,15])
    KobeF1<-sum(CatchMSYKobe[CatchMSYKobe[,1]==input$ManagementYearInput,16])
    KobeY5<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+4),6])
    KobeSSB5<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+4),13])
    KobeB5<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+4),15])
    KobeF5<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+4),16])
    KobeY10<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+9),6])
    KobeSSB10<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+9),13])
    KobeB10<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+9),15])
    KobeF10<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+9),16])
    KobeY20<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+19),6])
    KobeSSB20<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+19),13])
    KobeB20<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+19),15])
    KobeF20<-sum(CatchMSYKobe[CatchMSYKobe[,1]==(input$ManagementYearInput+19),16])

    outVect1<-c(KobeMSY,KobeSSBMSY,KobeBMSY,KobeFMSY,KobeY1,KobeSSB1,KobeB1,KobeF1,KobeY5,KobeSSB5,KobeB5,KobeF5,KobeY10,KobeSSB10,KobeB10,KobeF10,KobeY20,KobeSSB20,KobeB20,KobeF20,1,1,1,1,KobeY1/KobeMSY,KobeSSB1/KobeSSBMSY,KobeB1/KobeBMSY,KobeF1/KobeFMSY,KobeY5/KobeMSY,KobeSSB5/KobeSSBMSY,KobeB5/KobeBMSY,KobeF5/KobeFMSY,KobeY10/KobeMSY,KobeSSB10/KobeSSBMSY,KobeB10/KobeBMSY,KobeF10/KobeFMSY,KobeY20/KobeMSY,KobeSSB20/KobeSSBMSY,KobeB20/KobeBMSY,KobeF20/KobeFMSY)
    outMatrix1<-as.data.frame(matrix(c(outVect1),nrow=1,ncol=40,byrow = TRUE))
    write.table(outMatrix1,file=paste0(dir.base,"/Kobe_Matrix.csv"),row.names = FALSE, col.names = FALSE, append=TRUE, sep=",", dec=".")

    return(outVect1)
  }else{

    CatchOutRun1<-newCatch
    CatchOutRun1[,4]<-ifelse(CatchOutRun1[,4]>data.orig$Nfleet,(CatchOutRun1[,4]-data.orig$Nfleet),CatchOutRun1[,4])
    CatchOutRun1<-aggregate(CatchOutRun1,list(year=CatchOutRun1[,1],season=CatchOutRun1[,2],area=CatchOutRun1[,3],fleet=CatchOutRun1[,4]),sum)
    CatchOutRun1<-cbind(CatchOutRun1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchOutRun1[,4]],CatchOutRun1[,9:19])
    names(CatchOutRun1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)")
    CatchOutRun1<-CatchOutRun1[order(CatchOutRun1[,3],CatchOutRun1[,1],CatchOutRun1[,2],CatchOutRun1[,4]),]

    AppliedKobeYeq<-sum(CatchOutRun1[CatchOutRun1[,1]>=input$TargetYears[1] & CatchOutRun1[,1]<=input$TargetYears[2],6])/(input$TargetYears[2]-input$TargetYears[1]+1)
    AppliedKobeSSBeq<-sum(CatchOutRun1[CatchOutRun1[,1]>=input$TargetYears[1] & CatchOutRun1[,1]<=input$TargetYears[2],13])/(input$TargetYears[2]-input$TargetYears[1]+1)
    AppliedKobeBeq<-sum(CatchOutRun1[CatchOutRun1[,1]>=input$TargetYears[1] & CatchOutRun1[,1]<=input$TargetYears[2],15])/(input$TargetYears[2]-input$TargetYears[1]+1)
    AppliedKobeFeq<-sum(CatchOutRun1[CatchOutRun1[,1]>=input$TargetYears[1] & CatchOutRun1[,1]<=input$TargetYears[2],16])/(input$TargetYears[2]-input$TargetYears[1]+1)
    AppliedKobeY1<-sum(CatchOutRun1[CatchOutRun1[,1]==input$ManagementYearInput,6])
    AppliedKobeSSB1<-sum(CatchOutRun1[CatchOutRun1[,1]==input$ManagementYearInput,13])
    AppliedKobeB1<-sum(CatchOutRun1[CatchOutRun1[,1]==input$ManagementYearInput,15])
    AppliedKobeF1<-sum(CatchOutRun1[CatchOutRun1[,1]==input$ManagementYearInput,16])
    AppliedKobeY5<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+4),6])
    AppliedKobeSSB5<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+4),13])
    AppliedKobeB5<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+4),15])
    AppliedKobeF5<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+4),16])
    AppliedKobeY10<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+9),6])
    AppliedKobeSSB10<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+9),13])
    AppliedKobeB10<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+9),15])
    AppliedKobeF10<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+9),16])
    AppliedKobeY20<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+19),6])
    AppliedKobeSSB20<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+19),13])
    AppliedKobeB20<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+19),15])
    AppliedKobeF20<-sum(CatchOutRun1[CatchOutRun1[,1]==(input$ManagementYearInput+19),16])

    KobeMSY<-MSYoutput[1]
    KobeSSBMSY<-MSYoutput[2]
    KobeBMSY<-MSYoutput[3]
    KobeFMSY<-MSYoutput[4]

    outVect2<-c(AppliedKobeYeq,AppliedKobeSSBeq,AppliedKobeBeq,AppliedKobeFeq,AppliedKobeY1,AppliedKobeSSB1,AppliedKobeB1,AppliedKobeF1,AppliedKobeY5,AppliedKobeSSB5,AppliedKobeB5,AppliedKobeF5,AppliedKobeY10,AppliedKobeSSB10,AppliedKobeB10,AppliedKobeF10,AppliedKobeY20,AppliedKobeSSB20,AppliedKobeB20,AppliedKobeF20,AppliedKobeYeq/KobeMSY,AppliedKobeSSBeq/KobeSSBMSY,AppliedKobeBeq/KobeBMSY,AppliedKobeFeq/KobeFMSY,AppliedKobeY1/KobeMSY,AppliedKobeSSB1/KobeSSBMSY,AppliedKobeB1/KobeBMSY,AppliedKobeF1/KobeFMSY,AppliedKobeY5/KobeMSY,AppliedKobeSSB5/KobeSSBMSY,AppliedKobeB5/KobeBMSY,AppliedKobeF5/KobeFMSY,AppliedKobeY10/KobeMSY,AppliedKobeSSB10/KobeSSBMSY,AppliedKobeB10/KobeBMSY,AppliedKobeF10/KobeFMSY,AppliedKobeY20/KobeMSY,AppliedKobeSSB20/KobeSSBMSY,AppliedKobeB20/KobeBMSY,AppliedKobeF20/KobeFMSY)
    outMatrix2<-as.data.frame(matrix(c(outVect2),nrow=1,ncol=40,byrow = TRUE))
    write.table(outMatrix2,file=paste0(dir.base,"/Kobe_Matrix.csv"),row.names = FALSE, col.names = FALSE, append=TRUE, sep=",", dec=".")
  }
}

#' Save results
#'
#' This saves the output figures and data to a
#' user specified destination of their machine.
#'
#'
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @param baseSaveName the .
#' @author Nathan Vaughan
#' @keywords interface management
saveCurrResults<-function(input,output,session,baseSaveName){
  if(input$ForecastTarget==6){
  get_Cost_Benefit<-function(Point_value=NULL,Val_series=NULL,Quant_Series=NULL){
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
  }
  color <- colors()[grep('gr(a|e)y', colors(), invert = T)]
  color <- color[8:433]
  color <- sample(color,length(color))
  color <- c("red","dark blue","dark green", "gold", "pink", "purple","black","cyan","brown",color)
  color <- unique(color)
  colorChoicesOut <- color

  CatchOut1<-baseCatch
  CatchOut1<-cbind(CatchOut1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchOut1[,4]],CatchOut1[,5:15])
  names(CatchOut1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)")
  CatchOut1<-CatchOut1[order(CatchOut1[,3],CatchOut1[,1],CatchOut1[,2],CatchOut1[,4]),]

  CatchOutMSY1<-msyCatch
  CatchOutMSY1[,4]<-ifelse(CatchOutMSY1[,4]>data.orig$Nfleet,(CatchOutMSY1[,4]-data.orig$Nfleet),CatchOutMSY1[,4])
  CatchOutMSY1<-aggregate(CatchOutMSY1,list(year=CatchOutMSY1[,1],season=CatchOutMSY1[,2],area=CatchOutMSY1[,3],fleet=CatchOutMSY1[,4]),sum)
  CatchOutMSY1<-cbind(CatchOutMSY1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchOutMSY1[,4]],CatchOutMSY1[,9:19])
  names(CatchOutMSY1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)")
  CatchOutMSY1<-CatchOutMSY1[order(CatchOutMSY1[,3],CatchOutMSY1[,1],CatchOutMSY1[,2],CatchOutMSY1[,4]),]

  CatchOutRun1<-newCatch
  CatchOutRun1[,4]<-ifelse(CatchOutRun1[,4]>data.orig$Nfleet,(CatchOutRun1[,4]-data.orig$Nfleet),CatchOutRun1[,4])
  CatchOutRun1<-aggregate(CatchOutRun1,list(year=CatchOutRun1[,1],season=CatchOutRun1[,2],area=CatchOutRun1[,3],fleet=CatchOutRun1[,4]),sum)
  CatchOutRun1<-cbind(CatchOutRun1[,1:4],forecast.orig$fleet_assignment_to_allocation_group[CatchOutRun1[,4]],CatchOutRun1[,9:19])
  names(CatchOutRun1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)")
  CatchOutRun1<-CatchOutRun1[order(CatchOutRun1[,3],CatchOutRun1[,1],CatchOutRun1[,2],CatchOutRun1[,4]),]
 
   if(input$ForecastTarget==6){
    CatchOut1Cost<-CatchOut1[,1]
    CatchOut1Benefit<-CatchOut1[,1]
    CatchOutMSY1Cost<-CatchOut1[,1]
    CatchOutMSY1Benefit<-CatchOut1[,1]
    CatchOutRun1Cost<-CatchOut1[,1]
    CatchOutRun1Benefit<-CatchOut1[,1]
    
    for(i in unique(CatchOut1[,4])){
      
      if(data.run$units_of_catch[i]==1){
        catch_Col_temp<-6
        Catch_scale<-2204.62
      }else{
        catch_Col_temp<-8
        Catch_scale<-1000
      }
      CatchOut1Cost[which(CatchOut1[,4]==i)]<-vapply(CatchOut1[which(CatchOut1[,4]==i),11],get_Cost_Benefit,Val_series=CostMatrix[i,],Quant_Series=FSeq,FUN.VALUE = 1)
      CatchOut1Benefit[which(CatchOut1[,4]==i)]<-vapply(CatchOut1[which(CatchOut1[,4]==i),catch_Col_temp],get_Cost_Benefit,Val_series=Catch_scale*BenefitMatrix[i,],Quant_Series=Catch_scale*CatchSeq,FUN.VALUE = 1)
      CatchOutMSY1Cost[which(CatchOut1[,4]==i)]<-vapply(CatchOutMSY1[which(CatchOut1[,4]==i),11],get_Cost_Benefit,Val_series=CostMatrix[i,],Quant_Series=FSeq,FUN.VALUE = 1)
      CatchOutMSY1Benefit[which(CatchOut1[,4]==i)]<-vapply(CatchOutMSY1[which(CatchOut1[,4]==i),catch_Col_temp],get_Cost_Benefit,Val_series=Catch_scale*BenefitMatrix[i,],Quant_Series=Catch_scale*CatchSeq,FUN.VALUE = 1)
      CatchOutRun1Cost[which(CatchOut1[,4]==i)]<-vapply(CatchOutRun1[which(CatchOut1[,4]==i),11],get_Cost_Benefit,Val_series=CostMatrix[i,],Quant_Series=FSeq,FUN.VALUE = 1)
      CatchOutRun1Benefit[which(CatchOut1[,4]==i)]<-vapply(CatchOutRun1[which(CatchOut1[,4]==i),catch_Col_temp],get_Cost_Benefit,Val_series=Catch_scale*BenefitMatrix[i,],Quant_Series=Catch_scale*CatchSeq,FUN.VALUE = 1)
    }
    
    CatchOut1<-cbind(CatchOut1[,1:16],CatchOut1[,14:16])
    CatchOut1[,17]<-CatchOut1Cost
    CatchOut1[,18]<-CatchOut1Benefit
    CatchOut1[,19]<-CatchOut1Benefit-CatchOut1Cost
    
    CatchOutMSY1<-cbind(CatchOutMSY1[,1:16],CatchOutMSY1[,14:16])
    CatchOutMSY1[,17]<-CatchOutMSY1Cost
    CatchOutMSY1[,18]<-CatchOutMSY1Benefit
    CatchOutMSY1[,19]<-CatchOutMSY1Benefit-CatchOutMSY1Cost
    
    CatchOutRun1<-cbind(CatchOutRun1[,1:16],CatchOutRun1[,14:16])
    CatchOutRun1[,17]<-CatchOutRun1Cost
    CatchOutRun1[,18]<-CatchOutRun1Benefit
    CatchOutRun1[,19]<-CatchOutRun1Benefit-CatchOutRun1Cost
    
    names(CatchOut1)<-names(CatchOutMSY1)<-names(CatchOutRun1)<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","F(Std)","Cost","Benefit","Profit")
  }
  
  lcatch<-length(CatchOutRun1[1,])
  CatchOutDiffMB1<-CatchOutRun1
  CatchOutDiffMB1[,6:lcatch]<-(CatchOutMSY1[,6:lcatch]-CatchOut1[,6:lcatch])
  
  CatchOutDiffRB1<-CatchOutRun1
  CatchOutDiffRB1[,6:lcatch]<-(CatchOutRun1[,6:lcatch]-CatchOut1[,6:lcatch])
  
  CatchOutDiffRM1<-CatchOutRun1
  CatchOutDiffRM1[,6:lcatch]<-(CatchOutRun1[,6:lcatch]-CatchOutMSY1[,6:lcatch])
  
  CatchOutPercMB1<-CatchOutRun1
  CatchOutPercMB1[,6:lcatch]<-100*(CatchOutMSY1[,6:lcatch]-CatchOut1[,6:lcatch])/CatchOut1[,6:lcatch]
  
  CatchOutPercRB1<-CatchOutRun1
  CatchOutPercRB1[,6:lcatch]<-100*(CatchOutRun1[,6:lcatch]-CatchOut1[,6:lcatch])/CatchOut1[,6:lcatch]
  
  CatchOutPercRM1<-CatchOutRun1
  CatchOutPercRM1[,6:lcatch]<-100*(CatchOutRun1[,6:lcatch]-CatchOutMSY1[,6:lcatch])/CatchOutMSY1[,6:lcatch]
  
  CatchOutPropMB1<-CatchOutRun1
  CatchOutPropMB1[,6:lcatch]<-CatchOutMSY1[,6:lcatch]/CatchOut1[,6:lcatch]
  
  CatchOutPropRB1<-CatchOutRun1
  CatchOutPropRB1[,6:lcatch]<-CatchOutRun1[,6:lcatch]/CatchOut1[,6:lcatch]
  
  CatchOutPropRM1<-CatchOutRun1
  CatchOutPropRM1[,6:lcatch]<-CatchOutRun1[,6:lcatch]/CatchOutMSY1[,6:lcatch]

  CatchMSYKobe<-CatchOutMSY1
  KobeBMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],15])/(input$TargetYears[2]-input$TargetYears[1]+1)
  KobeFMSY<-sum(CatchMSYKobe[CatchMSYKobe[,1]>=input$TargetYears[1] & CatchMSYKobe[,1]<=input$TargetYears[2],16])/(input$TargetYears[2]-input$TargetYears[1]+1)
  CatchNewKobe<-CatchOutRun1
  CatchNewKobe<-aggregate(CatchNewKobe,list(year=CatchNewKobe[,1]),sum)
  CatchNewKobe<-CatchNewKobe[,c(1,17,16)]
  CatchNewKobe[,2]<-CatchNewKobe[,2]/KobeFMSY
  CatchNewKobe[,3]<-CatchNewKobe[,3]/KobeBMSY


  for(i in 6:lcatch)
  {
    CatchOutPercMB1[,i]<-ifelse(!is.finite(CatchOutPercMB1[,i]),0,CatchOutPercMB1[,i])
    CatchOutPercRB1[,i]<-ifelse(!is.finite(CatchOutPercRB1[,i]),0,CatchOutPercRB1[,i])
    CatchOutPercRM1[,i]<-ifelse(!is.finite(CatchOutPercRM1[,i]),0,CatchOutPercRM1[,i])
    CatchOutPropMB1[,i]<-ifelse(!is.finite(CatchOutPropMB1[,i]),1,CatchOutPropMB1[,i])
    CatchOutPropRB1[,i]<-ifelse(!is.finite(CatchOutPropRB1[,i]),1,CatchOutPropRB1[,i])
    CatchOutPropRM1[,i]<-ifelse(!is.finite(CatchOutPropRM1[,i]),1,CatchOutPropRM1[,i])
  }

  xlabOut="Year"

  withProgress(message="Saving output results figure and data",value=0.5,{
    if(!is.null(input$displayRadioButtonOut7) &
       !is.null(input$displayRadioButtonOut8) &
       !is.null(input$displayRadioButtonOut5) &
       !is.null(input$displayRadioButtonOut3) &
       !is.null(input$displayRadioButtonOut4) &
       !is.null(input$displayRadioButtonOut) &
       !is.null(input$displayRadioButtonOut1) &
       !is.null(input$displayRadioButtonOut2) &
       !is.null(input$displayYearsOut))
    {
      xlimOut=c((input$displayYearsOut[1]-1),(input$displayYearsOut[2]+3))
      xlimTargOut<-xlimOut
      if(!is.null(disp.Desc$TargetYears))
      {
        xlimTargOut<-disp.Desc$TargetYears
      }
      labelPointOut<-seq(input$displayYearsOut[1],input$displayYearsOut[2],max(1,floor((input$displayYearsOut[2]-input$displayYearsOut[1])/4)))
      if((input$displayYearsOut[2]-input$displayYearsOut[1])>=10){
        labelPointOut<-labelPointOut[-length(labelPointOut)]
      }
      makePlot<-TRUE
      makeKobe<-FALSE

      if(input$displayRadioButtonOut5=="Kobe"){
        makeKobe<-TRUE
        makePlot<-FALSE

        displayKobe<-CatchNewKobe[CatchNewKobe[,1]>=input$displayYearsOut[1] & CatchNewKobe[,1]<=input$displayYearsOut[2],]
        if(!is.null(input$displayYearsOut)){
          outputName<-baseSaveName
          dir.outputs<<-paste0(dir.base,"/outputs")
          if(!dir.exists(dir.outputs))
          {
            dir.create(dir.outputs)
          }
          xlimKobe<-c(0,max(2,1.05*displayKobe[,3]))
          ylimKobe<-c(0,max(2,1.05*displayKobe[,2]))
          steps<-seq(0.1,0.8,length.out = length(displayKobe[,1]))
          legendDispYears<-unique(floor(seq(displayKobe[1,1],displayKobe[length(displayKobe[,1]),1],length.out = 5)))
          legendSteps<-steps[displayKobe[,1]%in%legendDispYears]
          png(file=paste0(dir.outputs,"/",outputName,"Plot.png"),width=800,height=600)
          plot(NA,main='Kobe Plot new projection',xlab='B/Bmsy',ylab='F/Fmsy',xlim=xlimKobe,ylim=ylimKobe,axes=FALSE)
          axis(1,at=seq(0,xlimKobe[2],0.05),labels=FALSE,cex=0.5,pos=ylimKobe[1])
          axis(1,at=seq(0,xlimKobe[2],0.2),labels=FALSE,cex=0.5,pos=ylimKobe[1],lwd.ticks=2)
          axis(1,at=seq(0,xlimKobe[2],0.2),labels=TRUE,cex=0.5,pos=(ylimKobe[1]+0.02*(ylimKobe[2]-ylimKobe[1])),tick=FALSE)
          axis(2,at=seq(0,ylimKobe[2],0.05),pos=xlimKobe[1],labels=FALSE)
          axis(2,at=seq(0,ylimKobe[2],0.2),pos=(xlimKobe[1]+0.01*(xlimKobe[2]-xlimKobe[1])),tick=FALSE)
          polygon(x=c(xlimKobe[1],xlimKobe[1],xlimKobe[2],xlimKobe[2]),y=c(ylimKobe[1],1.0*ylimKobe[2],1.0*ylimKobe[2],ylimKobe[1]),lwd=2)
          polygon(x=c(xlimKobe[1],xlimKobe[1],1,1),y=c(ylimKobe[1],1,1,ylimKobe[1]),border=NA,col='yellow')
          polygon(x=c(1,1,xlimKobe[2],xlimKobe[2]),y=c(1,1.0*ylimKobe[2],1.0*ylimKobe[2],1),border=NA,col='yellow')
          polygon(x=c(xlimKobe[1],xlimKobe[1],1,1),y=c(1,1.0*ylimKobe[2],1.0*ylimKobe[2],1),border=NA,col='red')
          polygon(x=c(1,1,xlimKobe[2],xlimKobe[2]),y=c(ylimKobe[1],1,1,ylimKobe[1]),border=NA,col='green')
          lines(x=xlimKobe,y=c(1,1),lwd=2,col='black')
          lines(x=c(1,1),y=ylimKobe,lwd=2,col='black')
          for(i in 1:length(displayKobe[,3])){
            lines(x=displayKobe[i:length(displayKobe[,3]),3],y=displayKobe[i:length(displayKobe[,3]),2],lwd=2,col=gray(level=steps[i]))
          }
          points(x=displayKobe[,3],y=displayKobe[,2],cex=1,pch=16,col=gray(level=steps))
          points(x=displayKobe[displayKobe[,1]%in%legendDispYears,3],y=displayKobe[displayKobe[,1]%in%legendDispYears,2],cex=2,pch=16,col=gray(level=legendSteps))
          dev.off()
          png(file=paste0(dir.outputs,"/",outputName,"Legend.png"),width=250,height=1000)
          if(length(displayKobe[,1])>5){
            legendDispYears<-unique(floor(seq(displayKobe[1,1],displayKobe[length(displayKobe[,1]),1],length.out = 5)))
            legendSteps<-seq(0.1,0.8,length.out = length(legendDispYears))

            plot(NA,main='',xlab='',ylab='',xlim=c(0,1),ylim=c(0,1),axes=FALSE)
            legend(x='left',legend=legendDispYears,pch=16,cex=2,col=gray(level=legendSteps),title='Years')
          }else{
            legendDispYears<-displayKobe[,1]
            legendSteps<-seq(0.1,0.8,length.out = length(legendDispYears))

            plot(NA,main='',xlab='',ylab='',xlim=c(0,1),ylim=c(0,1),axes=FALSE)
            legend(x='left',legend=legendDispYears,pch=16,cex=2,col=gray(level=legendSteps),title='Years')
          }
          dev.off()
          write.csv(displayKobe,file=paste0(dir.outputs,"/",outputName,"DataKobe.csv"),row.names = FALSE)
        }

      }else{

        if(input$displayRadioButtonOut5=="Harvest Rate")
        {
          unitMod<-"F"
          catMod<-"Harvest Rate"
          catMod2<-"Harvest Rate"
          dataColOut<-c(1:5,16)
        }else if(input$displayRadioButtonOut5=="Economics")
        {
          unitMod<-"Dollars"
          if(input$displayRadioButtonOut9=="Costs"){
            catMod<-"Gross Costs"
            catMod2<-"Gross Costs"
            dataColOut<-c(1:5,17)
          }else if(input$displayRadioButtonOut9=="Benefits"){
            catMod<-"Gross Revenue"
            catMod2<-"Gross Revenue"
            dataColOut<-c(1:5,18)
          }else if(input$displayRadioButtonOut9=="Profits"){
            catMod<-"Net Profits"
            catMod2<-"Net Profits"
            dataColOut<-c(1:5,19)
          }
          
        }else if(input$displayRadioButtonOut5!="Landings" & input$displayRadioButtonOut5!="Dead Discards")
        {
          unitMod<-weightNames[disp.Desc$Units[1]]
          if(input$displayRadioButtonOut5=="SPB"){
            catMod<-"Equilibrium Spawning Potential Biomass"
            catMod2<-"Equilibrium Spawning Potential Biomass"
            dataColOut<-c(1:5,12)
          }else if(input$displayRadioButtonOut5=="SSB"){
            catMod<-"Spawning Stock Biomass"
            catMod2<-"Spawning Stock Biomass"
            dataColOut<-c(1:5,13)
          }else if(input$displayRadioButtonOut5=="Recruits"){
            catMod<-"Recruitment"
            catMod2<-"Recruitment"
            dataColOut<-c(1:5,14)
          }else if(input$displayRadioButtonOut5=="Population Biomass"){
            catMod<-"Total Population Biomass"
            catMod2<-"Total Population Biomass"
            dataColOut<-c(1:5,15)
          }
        }else if(input$displayRadioButtonOut5=="Landings")
        {
          if(input$displayRadioButtonOut2=="Weight")
          {
            dataColOut<-c(1:5,6)
            unitMod<-weightNames[disp.Desc$Units[1]]
          }else if(input$displayRadioButtonOut2=="Number")
          {
            dataColOut<-c(1:5,8)
            unitMod<-"1000's"
          }
          catMod<-"Catch"
          catMod2<-"Catch"
        }else if(input$displayRadioButtonOut5=="Dead Discards")
        {
          if(input$displayRadioButtonOut2=="Weight")
          {
            dataColOut<-c(1:5,7)
            unitMod<-weightNames[disp.Desc$Units[1]]
          }else if(input$displayRadioButtonOut2=="Number")
          {
            dataColOut<-c(1:5,9)
            unitMod<-"1000's"
          }
          catMod<-"Dead Discards"
          catMod2<-"Dead Discards"
        }

        if(input$displayRadioButtonOut7=="Raw"){
          if(input$displayRadioButtonOut1=="Base")
          {
            CatchDisp<-CatchOut1
            CatchDisp2<-CatchOut1
            sourceMod<-"Base"
          }else if(input$displayRadioButtonOut1=="Target")
          {
            CatchDisp<-CatchOutMSY1
            CatchDisp2<-CatchOut1
            sourceMod<-"MSY"
          }else if(input$displayRadioButtonOut1=="Implemented")
          {
            CatchDisp<-CatchOutRun1
            CatchDisp2<-CatchOut1
            sourceMod<-"New"
          }else
          {
            makePlot<-FALSE
          }
        }else if(input$displayRadioButtonOut7=="Base"){
          if(input$displayRadioButtonOut1=="Target")
          {
            if(input$displayRadioButtonOut8=="Absolute"){
              CatchDisp<-CatchOutDiffMB1
              CatchDisp2<-CatchOut1
              catMod2<-paste0("Change ",catMod2)
              sourceMod<-"Change from Base to MSY in "
            }else if(input$displayRadioButtonOut8=="Percentage"){
              CatchDisp<-CatchOutPercMB1
              CatchDisp2<-CatchOut1
              catMod2<-paste0("Change ",catMod2)
              unitMod<-"%"
              sourceMod<-"Percent Change from Base to MSY in "
            }else if(input$displayRadioButtonOut8=="Proportion"){
              CatchDisp<-CatchOutPropMB1
              CatchDisp2<-CatchOut1
              catMod2<-paste0("Relative ",catMod2)
              unitMod<-"ratio"
              sourceMod<-"MSY proportion of Base for "
            }
          }else if(input$displayRadioButtonOut1=="Implemented")
          {
            if(input$displayRadioButtonOut8=="Absolute"){
              CatchDisp<-CatchOutDiffRB1
              CatchDisp2<-CatchOut1
              catMod2<-paste0("Change ",catMod2)
              sourceMod<-"Change from Base to New in "
            }else if(input$displayRadioButtonOut8=="Percentage"){
              CatchDisp<-CatchOutPercRB1
              CatchDisp2<-CatchOut1
              catMod2<-paste0("Change ",catMod2)
              unitMod<-"%"
              sourceMod<-"Percent Change from Base to New in "
            }else if(input$displayRadioButtonOut8=="Proportion"){
              CatchDisp<-CatchOutPropRB1
              CatchDisp2<-CatchOut1
              catMod2<-paste0("Relative ",catMod2)
              unitMod<-"ratio"
              sourceMod<-"New proportion of Base for "
            }
          }else
          {
            makePlot<-FALSE
          }
        }else if(input$displayRadioButtonOut7=="Target"){
          if(input$displayRadioButtonOut1=="Implemented")
          {
            if(input$displayRadioButtonOut8=="Absolute"){
              CatchDisp<-CatchOutDiffRM1
              CatchDisp2<-CatchOut1
              catMod2<-paste0("Change ",catMod2)
              sourceMod<-"Change from MSY to New in "
            }else if(input$displayRadioButtonOut8=="Percentage"){
              CatchDisp<-CatchOutPercRM1
              CatchDisp2<-CatchOutMSY1
              catMod2<-paste0("Change ",catMod2)
              unitMod<-"%"
              sourceMod<-"Percent Change from MSY to New in "
            }else if(input$displayRadioButtonOut8=="Proportion"){
              CatchDisp<-CatchOutPropRM1
              CatchDisp2<-CatchOutMSY1
              catMod2<-paste0("Relative ",catMod2)
              unitMod<-"ratio"
              sourceMod<-"New proportion of MSY for "
            }
          }else
          {
            makePlot<-FALSE
          }
        }

        if(makePlot==TRUE)
        {

          if(input$ForecastTarget==6){
            potNamesCatch<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","HarvestRate(F)","Cost","Benefit","Profit")
          }else{
            potNamesCatch<-c("Year","Season","Area","Fleet","Group","Retain(B)","Discard(B)","Retain(N)","Discard(N)","Retain(Basis)","HarvestRate(F)","SPR","SSB","Recruits","PopBiomass","HarvestRate(F)")
          }
          
          CatchSave1<-CatchDisp
          names(CatchSave1)<-potNamesCatch
          CatchDisp<-CatchDisp[,dataColOut]
          CatchDisp2<-CatchDisp2[,dataColOut]
          if("Area"%in%input$displayRadioButtonOut6 | length(unique(CatchDisp[,3]))==1)
          {
            CatchDisp[,3]<-rep(-99,length(CatchDisp[,3]))
            CatchDisp2[,3]<-rep(-99,length(CatchDisp2[,3]))
          }
          if("Season"%in%input$displayRadioButtonOut6 | length(unique(CatchDisp[,2]))==1)
          {
            CatchDisp[,2]<-rep(-99,length(CatchDisp[,2]))
            CatchDisp2[,2]<-rep(-99,length(CatchDisp2[,2]))
          }
          if(input$displayRadioButtonOut=="Total")
          {
            clustMod<-"Total"
            CatchDisp[,4]<-rep(-99,length(CatchDisp[,4]))
            CatchDisp[,5]<-rep(-99,length(CatchDisp[,5]))
            CatchDisp2[,4]<-rep(-99,length(CatchDisp2[,4]))
            CatchDisp2[,5]<-rep(-99,length(CatchDisp2[,5]))
          }else if(input$displayRadioButtonOut=="Groups"){
            CatchDisp[,4]<-rep(-99,length(CatchDisp[,4]))
            CatchDisp<-CatchDisp[CatchDisp[,5]%in%as.numeric(input$displayRadioButtonOut4),]
            CatchDisp2[,4]<-rep(-99,length(CatchDisp2[,4]))
            CatchDisp2<-CatchDisp2[CatchDisp2[,5]%in%as.numeric(input$displayRadioButtonOut4),]
            clustMod<-"Group"
          }else if(input$displayRadioButtonOut=="Fleets"){
            CatchDisp[,5]<-rep(-99,length(CatchDisp[,5]))
            CatchDisp<-CatchDisp[CatchDisp[,4]%in%as.numeric(input$displayRadioButtonOut3),]
            CatchDisp2[,5]<-rep(-99,length(CatchDisp2[,5]))
            CatchDisp2<-CatchDisp2[CatchDisp2[,4]%in%as.numeric(input$displayRadioButtonOut3),]
            clustMod<-"Fleet"
          }
          if(length(unique(CatchDisp[,5]))==1){
            CatchDisp[,5]<-rep(-99,length(CatchDisp[,5]))
            CatchDisp2[,5]<-rep(-99,length(CatchDisp2[,5]))
          }
          if(input$displayRadioButtonOut8=="Proportion" | input$displayRadioButtonOut8=="Percentage"){
            CatchDisp[,6]<-CatchDisp[,6]*CatchDisp2[,6]
            CatchDisp<-aggregate(CatchDisp,list(year=CatchDisp[,1],season=CatchDisp[,2],area=CatchDisp[,3],fleet=CatchDisp[,4],group=CatchDisp[,5]),sum)
            CatchDisp<-cbind(CatchDisp[,1:5],CatchDisp[,11])
            names(CatchDisp)<-potNamesCatch[dataColOut]
            CatchDisp<-CatchDisp[order(CatchDisp[,3],CatchDisp[,1],CatchDisp[,2],CatchDisp[,4]),]

            CatchDisp2<-aggregate(CatchDisp2,list(year=CatchDisp2[,1],season=CatchDisp2[,2],area=CatchDisp2[,3],fleet=CatchDisp2[,4],group=CatchDisp2[,5]),sum)
            CatchDisp2<-cbind(CatchDisp2[,1:5],CatchDisp2[,11])
            names(CatchDisp2)<-potNamesCatch[dataColOut]
            CatchDisp2<-CatchDisp2[order(CatchDisp2[,3],CatchDisp2[,1],CatchDisp2[,2],CatchDisp2[,4]),]

            CatchDisp[,6]<-CatchDisp[,6]/CatchDisp2[,6]
          }else{
            CatchDisp<-aggregate(CatchDisp,list(year=CatchDisp[,1],season=CatchDisp[,2],area=CatchDisp[,3],fleet=CatchDisp[,4],group=CatchDisp[,5]),sum)
            CatchDisp<-cbind(CatchDisp[,1:5],CatchDisp[,11])
            names(CatchDisp)<-potNamesCatch[dataColOut]
            CatchDisp<-CatchDisp[order(CatchDisp[,3],CatchDisp[,1],CatchDisp[,2],CatchDisp[,4]),]
          }

          LegendVals<-aggregate(CatchDisp[,2:5],list(season=CatchDisp[,2],area=CatchDisp[,3],fleet=CatchDisp[,4],group=CatchDisp[,5]),sum)
          LegendVals<-LegendVals[,1:4]
          LegendNames<-LegendVals
          LegendNames[,1]<-ifelse(LegendVals[,1]==-99,"", paste0(disp.Desc$SeasonNames[LegendVals[,1],2],", "))
          LegendNames[,2]<-ifelse(LegendVals[,2]==-99,"", paste0(disp.Desc$AreaNames[LegendVals[,2],2],", "))
          LegendNames[,3]<-ifelse(LegendVals[,3]==-99,"", disp.Desc$FleetNames[LegendVals[,3],2])
          LegendNames[,4]<-ifelse(LegendVals[,4]==-99,"", paste0(disp.Desc$GroupNames[LegendVals[,4],2],", "))

          CatchTempOut<-CatchDisp[(CatchDisp[,1]>=xlimOut[1] & CatchDisp[,1]<=xlimOut[2]) | (CatchDisp[,1]>=xlimTargOut[1] & CatchDisp[,1]<=xlimTargOut[2]),]
          displayValsOut<-unique(paste0(LegendNames[,2],LegendNames[,1],LegendNames[,4],LegendNames[,3]))
          if(length(displayValsOut)==1){
            if(displayValsOut==""){displayValsOut<-"Total"}}
          if(length(displayValsOut)>15){
            height<-400*(length(displayValsOut)/15)
          }else{height<-'auto'}

          outputName<-baseSaveName
          dir.outputs<<-paste0(dir.base,"/outputs")
          if(!dir.exists(dir.outputs))
          {
            dir.create(dir.outputs)
          }

          png(file=paste0(dir.outputs,"/",outputName,"Legend.png"),width=250,height=1000)
          par(mar=c(2,2,2,2))
          plot(NA,main='',xlab='',ylab='',xlim=c(0,1),ylim=c(0,1),axes=FALSE)
          legend(x=0,y=1,legend=displayValsOut,lty=1,lwd=2,col=colorChoicesOut[1:length(displayValsOut)],title='Display Categories')
          dev.off()

          CatchTempOut<-CatchTempOut[is.finite(CatchTempOut[,6]),]
          ylimOut=c((min(CatchTempOut[,6],na.rm=TRUE)-0.05*max(abs(CatchTempOut[,6]),na.rm=TRUE)),(max(CatchTempOut[,6],na.rm=TRUE)+0.05*max(abs(CatchTempOut[,6]),na.rm=TRUE)))
          ylabOut<-paste0(catMod2," (",unitMod,")")
          mainOut<-paste0(sourceMod," Projected ",clustMod," ",catMod)

          png(file=paste0(dir.outputs,"/",outputName,"Plot.png"),width=800,height=600)
          par(mar=c(5,4,4,2))
          plot(NA,main=mainOut,xlab='',ylab='',xlim=xlimOut,ylim=ylimOut,axes=FALSE)
          axis(1,at=c(input$displayYearsOut[1]:input$displayYearsOut[2],(input$displayYearsOut[2]+2)),labels=FALSE,cex=0.5,pos=ylimOut[1])
          axis(1,at=c(labelPointOut,(input$displayYearsOut[2]+2)),labels=FALSE,cex=0.5,pos=ylimOut[1],lwd.ticks=2)
          axis(1,at=c(labelPointOut,(input$displayYearsOut[2]+2)),labels=c(labelPointOut,'Target'),cex=0.5,pos=(ylimOut[1]+0.02*(ylimOut[2]-ylimOut[1])),tick=FALSE)
          axis(2,pos=xlimOut[1],labels=FALSE)
          axis(2,pos=(xlimOut[1]+0.01*(xlimOut[2]-xlimOut[1])),tick=FALSE)
          title(xlab=xlabOut,ylab=ylabOut,line=1)
          polygon(x=c(xlimOut[1],xlimOut[1],xlimOut[2],xlimOut[2]),y=c(ylimOut[1],1.0*ylimOut[2],1.0*ylimOut[2],ylimOut[1]))
          CatchSave2<-CatchSave1[1:(input$displayYearsOut[2]-input$displayYearsOut[1]+2),1,drop=FALSE]
          CatchSave2[,1]<-c(input$displayYearsOut[1]:input$displayYearsOut[2],mean(input$TargetYears))
          for(i in 1:length(LegendVals[,1])){
            CatchTempOut2<-CatchTempOut[CatchTempOut[,2]==LegendVals[i,1] & CatchTempOut[,3]==LegendVals[i,2] & CatchTempOut[,4]==LegendVals[i,3] & CatchTempOut[,5]==LegendVals[i,4],]
            lines(x=CatchTempOut2[CatchTempOut2[,1]>=input$displayYearsOut[1] & CatchTempOut2[,1]<=input$displayYearsOut[2],1],y=CatchTempOut2[CatchTempOut2[,1]>=input$displayYearsOut[1] & CatchTempOut2[,1]<=input$displayYearsOut[2],6],lwd=2,col=colorChoicesOut[i])
            points(x=(input$displayYearsOut[2]+2),y=mean(CatchTempOut2[CatchTempOut2[,1]>=input$TargetYears[1] & CatchTempOut2[,1]<=input$TargetYears[2],6]),pch=16,col=colorChoicesOut[i])
            CatchSave2<-cbind(CatchSave2,c(CatchTempOut2[CatchTempOut2[,1]>=input$displayYearsOut[1] & CatchTempOut2[,1]<=input$displayYearsOut[2],6],mean(CatchTempOut2[CatchTempOut2[,1]>=input$TargetYears[1] & CatchTempOut2[,1]<=input$TargetYears[2],6])))
          }
          names(CatchSave2)<-c("Year",displayValsOut)
          dev.off()

          write.csv(CatchSave1,file=paste0(dir.outputs,"/",outputName,"DataDetailed.csv"),row.names = FALSE)
          write.csv(CatchSave2,file=paste0(dir.outputs,"/",outputName,"DataPlot.csv"),row.names = FALSE)
        }
      }
    }
  })
}
