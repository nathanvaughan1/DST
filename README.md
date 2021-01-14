# A decision support tool for fishery management impact projections  



##### **Nathan Vaughan Ph.D.**
##### **University of Miami - NOAA Southeast Fisheries Science Center**
##### **September 19, 2018**  

</div>



*This document describes the development of a stock assessment decision support tool (DST) designed to quantify the impact of proposed fishery management actions on projected fishery overfishing limits. The DST software automates: 1) modification of stock synthesis 3.24 ([SS3](https://vlab.ncep.noaa.gov/web/stock-synthesis)) input files based on user inputs; 2) iterative execution of SS3 within a search loop in order to achieve desired equilibrium population/catch targets under altered fishery dynamics; and 3) further reads, displays, compares, and exports of results of the updated stock assessment results for user interpretation.*  
*Document structure*  
*1) [**Introduction**](#introduction): A brief description of SS3 and its use in fisheries management, the need to extend SS3 to facilitate future fishery dynamics that vary significant from historical patterns, and the importance of a simple curated user interface to allow non-experts to interact and explore the impacts of management decisions on stock assessment results.*  
*2) [**Methods**](#methods): A description of the [fishery dynamics available for modification within the DST](#methods2) and the mechanics developed to implement new [fleet dynamics within the projection period](#methods1), [automate interaction with SS3 files](#methods3), and [explore assessment projection results](#methods4).*  
*3) [**Tutorial**](#tutorial): A step by step walkthrough of the DST interface and example application of the DST;*  


#### **Introduction**
Stock assessment scientists tasked with providing best scientific advice and managers tasked with transforming this advise into regulation of fishery resources are faced with many challenges. Depleted stocks, diverse incomplete data sets, a changing climate, competing stake holder desires, and uncertain future conditions are just some of these. A robust and frequently utilized tool available to scientists in determining best scientific advice is the stock synthesis 3.24 (SS3) assessment software package. SS3 is built upon a flexible statistical catch at age population model with parameter estimation achieved using AD Model Builder ([ADMB](http://www.admb-project.org/)).  
Key features of SS3 that make it appealing to assessment scientists are its flexible array of optional data inputs allowing estimation of a data-limited age-structured production model or a data-rich model with complex spatio-temporal variation in population and fishing fleet dynamics. These features have allowed SS3 to be implemented in over 60 formal stock assessments in the United States, Europe, and the Asia Pacific. SS3 has extensive capabilities to incorporate complexities including spatio-temporally varying annual catch limits (ACLs), minimum size limits, bag limits, closed seasons/areas, and gear restrictions into the assessment of current and historic stock status. These capabilities have significantly improved stock assessment scientists abilities to provide both absolute estimates of stock status and the uncertainty around these results. One limitation of SS3 has been its capacity for population projection under changing future conditions, where future patterns can only be set to mirror an historic reference period.  
Increasingly however, SS3 is being used in fisheries with complex and changing fleet behaviors that are being managed using dynamic regulation changes. Current stock assessment protocols using SS3 are limited in there ability to account for potential future changes in management regulations and how these may influence the annual catch limits recommended as best available science. Assessment scientists are therefore forced to provide fixed ACLs even under circumstances that regulations are likely to change. Fishery managers can likewise become frustrated when static ACLs leave them unable to realize the potential benefit or quantify the potential harm of competing regulation proposals. When three or more years can pass between stock assessments for a given species this leaves a large degree of uncertainty surrounding stock status and optimal harvest rates.  
Current attempts to bridge this gap require manual ad-hoc methods of modifying future fishery dynamics, such as inputting expected future catch statistics as data, which are time consuming to implement and not 100% accurate. This limitation allows stock assessment scientists to at best provide a small number of pre-decided alternative management action projections. These alternative projections are extremely useful however they can only be performed by trained stock assessment scientist with formal advanced request. This delay between requests and receipt of results presents a significant hurdle to interactive management and the development of an intuitive understanding of fishery dynamics. It would therefore be valuable to close this loop and allow managers and other non-expert users to interact with completed stock assessments to quantify the change in projected ACL under proposed fishery conditions.  
This capability could significantly increase users intuitive understanding of the fishery, crowd source management option ideas by allowing various users to run stock assessment projections, improve the accuracy of final ACLs by allowing them to be projected using the final regulation decisions, and accelerate turnaround times by removing the back and forward bottleneck between assessment scientists and managers. The principle requirements to achieving this goal are: 1) A method to accurately incorporate changing future fishery dynamics into SS3 projections; 2) Automated methods to read and modify all core SS3 input and output files; and 3) A simple graphical user interface that allows non-experts to modify and run SS3 projections and view the results.  
Some progress towards the automated reading and modification of SS3 files has already been accomplished in the [R](https://www.r-project.org/) software language though the [R4SS](https://github.com/r4ss/r4ss) package. Development in R is also beneficial due to its popularity among scientists, extensive statistical and graphing capabilities, and utility for development of graphical user interfaces through the [shiny](https://shiny.rstudio.com/) package. For these reasons the DST and its underlying automation logic were developed in R. This document will detail the methods developed to allow dynamic user specified future changes in fishery dynamics, automated modification of all SS3 files, and a tutorial of the graphical user interface built to facilitate these changes.      



#### **Methods** 
*Fishery projection:* The goal for interfacing with SS3 was to allow assessment scientists and managers to incorporate the impact of new management actions into ACL projections in order to maximize sustainable harvests and minimize the chances of over-fishing. The multi-year delay between stock assessments requires that the change in fleet dynamics can occur a variable number of years after the end of the assessment data series. These objectives should ideally be achieved without impacting the existing model parameterization and be able to replicate the base model results under the scenario that future fleet dynamics remain unchanged. The methodology we developed was to create a shadow fleet. Under the base scenario of no change in fleet dynamics the shadow fleet is parameterized identically to the true fleet. Under regulation/management change scenarios the pertinent fleet parameters such as retention function inflection point and discard mortality asymptotes will be modified in the shadow fleet to match the new user input values.  
The modification process proceeds as follows:  
1) An SS3 model is developed and parameterized using only the true fleet data to obtain a base model. This is the model that would be developed and used for management advise in a standard stock assessment under the assumption of no change.  
2) Run forecast with base model and a one hundred year projection under base forecast conditions.  
3) Shadow fleets identical to the original fleets are added (i.e. if the base model has five fleets then five duplicate shadow fleets are added) by modifying the SS3 control (A), data (B), parameter (C), and forecast (D) input files.  
 &nbsp; &nbsp; A) Control file modifications.  
 &nbsp; &nbsp; &nbsp; &nbsp;  i) Initial F values of 0.00001 times base fleet Initial F for all shadow fleets are added.  
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Catchability matrix rows are duplicated for shadow fleets.  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Catchability random effects are duplicated for relevant shadow fleets.  
 &nbsp; &nbsp; &nbsp; &nbsp; iv) Age and size selectivity/discard matrix rows are duplicated for shadow fleets.   
 &nbsp; &nbsp; &nbsp; &nbsp; v) Selectivity/discard parameters are duplicated for all shadow fleets.   
 &nbsp; &nbsp; &nbsp; &nbsp; vi) Variance adjustment parameters are duplicated for all shadow fleets.  
 &nbsp; &nbsp; &nbsp; &nbsp; vii) Lambdas are duplicated for all shadow fleets.  
 &nbsp; &nbsp; &nbsp; &nbsp; *Note: Time varying parameters are set as static for shadow fleets and adjusted to equal the base value in the final data year.*  
 &nbsp; &nbsp; B) Data file modifications.  
 &nbsp; &nbsp; &nbsp; &nbsp; i) Number of fishing fleets is doubled.  
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Fleet names are added for each of the shadow fleets by appending "_dummy" to each of the base fleet names.  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Survey timing values are duplicated for each shadow fleet.  
 &nbsp; &nbsp; &nbsp; &nbsp; iv) Fishing area is duplicated for each shadow fleet.  
 &nbsp; &nbsp; &nbsp; &nbsp; v) Units of catch are duplicated for each shadow fleet.  
 &nbsp; &nbsp; &nbsp; &nbsp; vi) Log standard errors are duplicated for each shadow fleet.  
 &nbsp; &nbsp; &nbsp; &nbsp; vii) equilibrium initial catches are set to base fleet values multiplied by 0.0000000001 for each shadow fleet.  
 &nbsp; &nbsp; &nbsp; &nbsp; viii) annual catches are set to 0.0000000001 for each shadow fleet.  
 &nbsp; &nbsp; C) Parameter file modifications.  
 &nbsp; &nbsp; &nbsp; &nbsp; i) Ensure that there are the correct number of forecast period recruitment deviation parameters (Default values zero).  
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Ensure that there are the correct number of forecast period implementation error parameters (Default values zero).  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Add shadow fleet initial F parameters equal to 0.0000000001 times base fleet values.  
 &nbsp; &nbsp; &nbsp; &nbsp; iv) Add shadow fleet annual F parameters equal to 0.0000000001 times base fleet values.  
 &nbsp; &nbsp; &nbsp; &nbsp; v) Add shadow fleet catchability parameters if applicable equal to base fleet values.  
 &nbsp; &nbsp; &nbsp; &nbsp; vi) Add shadow fleet selection parameters equal to base fleet values.   
 &nbsp; &nbsp; D) Forecast file modifications.  
 &nbsp; &nbsp; &nbsp; &nbsp; i) Add one to the number of allocation groups if no zero allocation group in base assessment.  
 &nbsp; &nbsp; &nbsp; &nbsp; ii) duplicate the fleet assignments to allocation groups for all shadow fleets.  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Set all the base fleets to a new allocation group zero.  
 &nbsp; &nbsp; &nbsp; &nbsp; iv) Set fleet relative F method to two.  
 &nbsp; &nbsp; &nbsp; &nbsp; v) Calculate average seasonal relative F of base fleets from the input year range.  
 &nbsp; &nbsp; &nbsp; &nbsp; vi) Duplicate and bind relative F columns to create relative F matrix for all base and shadow fleets.  
 &nbsp; &nbsp; &nbsp; &nbsp; vii) Multiply relative F by 0.0000000001 for each base fleet.  
 &nbsp; &nbsp; &nbsp; &nbsp; viii) Set first year for caps and allocation to year of management implementation (*Default: Next year from system clock*).  
 &nbsp; &nbsp; &nbsp; &nbsp; ix) Set forecast catch units to -1.  
 &nbsp; &nbsp; &nbsp; &nbsp; x) Add any years/seasons/fleets missing from forecast catch input matrix.  
 &nbsp; &nbsp; &nbsp; &nbsp; xi) Set units of catch to match the fleet units (numbers/weight) and retained/dead component.  
 &nbsp; &nbsp; &nbsp; &nbsp; xii) for all years prior to year of management implementation set fixed forecast catch equal to base model forecast results.  
 &nbsp; &nbsp; &nbsp; &nbsp; xiii) Set number of forecast years to one hundred.  
4) Update selection and discard mortality parameter values, in control and parameter files, to match user inputs from GUI.   
5) Update fleet allocations and relative F values, in forecast file, to match user input from GUI.  
6) Update population target type and value (i.e. SPR=30%, MSY), in forecast file, to match user input from GUI.  
7) Under scenarios of changing selectivity and/or fleet allocations the default results of SS3 may not achieve the desired SPR or MSY targets at equilibrium. An iterative algorithm is therefore applied to achieve target values during a user input time window (By default the time window is >50 years in the future to approximate equilibrium).  
 &nbsp; &nbsp; A) If spawning potential ratio (SPR) or spawning biomass ratio (SBR) target is chosen.  
 &nbsp; &nbsp; &nbsp; &nbsp; i) Run SS3 starting with the input target being equal to the desired target.   
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Read output of SS3 and if achieved target is not equal to the desired target adjust input target by the difference.  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Re-run SS3 and again compare results.  
 &nbsp; &nbsp; &nbsp; &nbsp; iv) Repeat steps ii and iii until difference between the achieved and desired targets is less that 0.0001 times the desired target.   
 &nbsp; &nbsp; B) If maximum sustainable yield is chosen as the target.  
 &nbsp; &nbsp; &nbsp; &nbsp; i) Set the SS3 forecast file to target a fixed SPR value.  
 &nbsp; &nbsp; &nbsp; &nbsp; *(Note: This is required to allow the target to be modified to search for the true MSY/Fmsy)*  
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Perform three runs of SS3 with SPR target values of 0.2, 0.3, and 0.4. Save the input SPR target, achieved SPR, and achieved catch.  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Fit a parabolic curve through the results with x=input SPR and y=achieved catch.  
 &nbsp; &nbsp; &nbsp; &nbsp; iv) Adjust input SPR to a weighted mean value of 0.8 times vertex of the parabola, 0.1 times the SPR estimate less than the vertex with the highest catch, and  0.1 times the SPR estimate larger than the vertex with the highest catch.  
 &nbsp; &nbsp; &nbsp; &nbsp; *(Note: This encourages the model to rapidly shrink the distance between the best spr targets above and below that achieving maximum catch)*  
 &nbsp; &nbsp; &nbsp; &nbsp; v) Re-run SS3 with new input SPR and repeat steps iii and iv until the distance between the best SPR targets above and below that achieving maximum catch are less than 0.0001 times SPR at maximum catch.  
8) The SS3 output results of the above search are then saved as the optimum fishing strategy for the input management conditions.  
9) If applied fishing is set to be different from this optimum strategy (i.e. constant F at some fraction of Foptimum or constant catch) another iteration of SS3 is run with fixed values of catch or F.  
 &nbsp; &nbsp; A) First the year for fixed caps and allocations is set to the end of the forecast period so that fixed fleet values can be input.  
 &nbsp; &nbsp; B) Projection values for either catch or F are then input for every fleet/season/year of the 100 year projection period.  
 &nbsp; &nbsp; C) Projection values are calculated from the average proportions in the user input target year range.   
10) Applied fishing results are saved to the Run folder.  
11) Results are then synthesized and presented in the outputs tab.  # {#methods2} 
*Fishery modifiers:* The DST is designed to allow simple user modification of the SS3 parameters linked to common fishery management levers. These modifications are enabled through the use of simple input boxes, sliders, and radio buttons. This allows fishery managers and regulatory scientists to make adjustments to the underlying SS3 model without any knowledge of SS3 file structure. SS3 variables available for modification include population target details (A), fishery quota measurement methods (B), fleet specific allocation methods and proportions (C), fleet specific retention function form (D), and fleet specific discard mortality rate (E).  
 &nbsp; &nbsp; (A) Population target options include.  
 &nbsp; &nbsp; &nbsp; &nbsp; i) Spawning potential ratio target value in target year range.   
 &nbsp; &nbsp; &nbsp; &nbsp; *(Note: This is the equilibrium expectation for a given F rate, not the instantaneouse ratio, and does not incorporate actual recruitment levels.)*   
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Spawning biomass ratio target value in target year range.   
 &nbsp; &nbsp; &nbsp; &nbsp; *(Note: This is the actual estimated ratio of current spawning biomass to virgin spawning biomass)*  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Maximize catch in target year range.  
 &nbsp; &nbsp; &nbsp; &nbsp; *(Note: This option simple maximizes average retained catch during the input year range, this is approximately nmaximum sustainable yeild if year range longer than a full lifespan in the future.)*  
 &nbsp; &nbsp; (B) Quota measurement methods.   
 &nbsp; &nbsp; &nbsp; &nbsp; i) Retained biomass.  
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Dead biomass.  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Retained numbers.  
 &nbsp; &nbsp; &nbsp; &nbsp; iv) Dead numbers.  
 &nbsp; &nbsp; (C) Allocation methods.    
 &nbsp; &nbsp; &nbsp; &nbsp; i) Individual fleet and/or group specific allocation.  
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Annual or seasonal allocation.  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Allocation based on catch as measured in biomass or by F.  
 &nbsp; &nbsp; (D) Retention function.  
 &nbsp; &nbsp; &nbsp; &nbsp; i) Inflection point of inflection function from zero to maximum length.  
 &nbsp; &nbsp; &nbsp; &nbsp; ii) Slope of retention function from $-inf$ to $inf$.  
 &nbsp; &nbsp; &nbsp; &nbsp; iii) Asymptote of retention function from 0 to 1.  
 &nbsp; &nbsp; (E) Discard mortality.  
 &nbsp; &nbsp; &nbsp; &nbsp; i) Asymptotic discard mortality rate.  
 &nbsp; &nbsp; &nbsp; &nbsp; *(Note: Inflection point and slope are fixed at the base models estimated values)*  

*File editing automation:* Automated iteration and editing of SS3 files is accomplished using a combination of existing package methods from R4SS and custom functions.  
1) R4SS functions are used to read, edit, and write.   
 &nbsp; &nbsp; A) Starter files.  
 &nbsp; &nbsp; B) Data files.  
 &nbsp; &nbsp; C) Output files.  
2) Custom functions were written to read, edit, and write.  
 &nbsp; &nbsp; A) Forecast files.  
 &nbsp; &nbsp; *(Note: This function is a modification of the R4SS file extended to allow a custom relative F matrix to be implemented)*  
 &nbsp; &nbsp; B) Control files.   
 &nbsp; &nbsp; *(Note: A similar function was recently added to the R4SS package and these two functions have not yet been compared)*  
 &nbsp; &nbsp; C) Parameter files.  
 &nbsp; &nbsp; D) A display data file developed to allow input of detailed names for fleets/groups/seasons/areas, display units, and other key forecast values.   
 &nbsp; &nbsp; E) Catch and stock structure time-series data.  
 &nbsp; &nbsp; F) Results summary and plotting for executive summary style documentation.  
In combination these functions facilitate extensive manipulation of all core SS3 input files and results through simple and intuitive user adjustable attributes.  

*Results exploration:* The DST was designed to facilitate simple and immediate interaction with SS3 assessment results; with the goal of enabling real-time evaluation of the impact of potential management actions. This is accomplished through an output display panel capable of plotting a wide array of fishery and stock status time-series data.  
1) Fishery data.  
 &nbsp; &nbsp; A) Retained catch in numbers or weight. Can be optionally partitioned by fleet, group, season, and area.  
 &nbsp; &nbsp; B) Dead discards in numbers or weight. Can be optionally partitioned by fleet, group, season, and area.   
 &nbsp; &nbsp; C) Harvest rate F.  Can be optionally partitioned by fleet, group, season, and area.  
2) Stock status data.  
 &nbsp; &nbsp; A) Spawning stock biomass.  Can be optionally partitioned by season and area.  
 &nbsp; &nbsp; B) Population biomass.  Can be optionally partitioned by season and area.   
 &nbsp; &nbsp; C) Recruits.   Can be optionally partitioned by season and area.  
 &nbsp; &nbsp; D) A Kobe plot of F/Fmsy vs B/Bmsy.  
 &nbsp; &nbsp; *(Note: for kobe plot results Fmsy and Bmsy are calculated as the average F and Biomass during the target year range of the optimal fishing model.)*    
 Results can be shown for the base model, optimal fishing model, or applied fishing  model. Additionally, comparisons between model results can be shown as an absolute difference, percent difference, or ratio. The display years for plotting of all time-series are also user selectable.   


#### **Tutorial** {#tutorial}
This tutorial will detail the capabilities of the decision support tool (DST) using an example case available with all distributions to allow you to follow along and confirm your results.  
*Assessment selection page:* The assessment selection page provides your first introduction to the DST.   
To get started click the select assessment drop-down menu as shown in Figure 1 below.    

![](Tutorial%20manual%20screenshots/Select%20assessment.PNG)
Figure 1: Decision Support Tool opening screen.
 
You will see a list of options: 1) the "Please select an assessment" prompt; 2) An "Upload new assessment" option; and 3) Multiple additional options for all available assessments contained in your DST assessments folder.  
1) This is simply a prompting place holder, selecting it will have no effect.   
2) The upload assessment option shown in figure 2 allows you to import a new assessment from files on you computer. A minimum of two stock synthesis files are required for this, a control file and a data file describing the fishery. The DST is designed in large part to interact with fleet retention and discard mortality values that are a common target of fishery management. However, these functions are difficult to quantify, particularly in data-limited assessments, and many not be included in the uploaded control file due to a lack of data for parameter estimation. In these cases the DST will automatically modify the required files and parameterize these functions with the required base model inferred values. A number of additional files are required to run an SS3 assessment and are also available for optional upload, it is strongly recommended that these are uploaded to ensure you understand there chosen values. However, if they are not available to you the DST will attempt to intelligently generate files with default values. Future efforts will be directed to developing interfaces for modifying these default file values. An assessment name field is also required, this will be the display name shown in the selection drop-down once the assessment has been uploaded. Once all available files are uploaded and a name input clicking the upload assessment button will upload and modify available files, build missing files, run the base assessment to confirm parameters are optimally estimated, and build a dynamic user management interface dependent on the specific features of the assessment.    

![](/Tutorial%20manual%20screenshots/Upload%20Assessment.PNG)
Figure 2: Upload new assessment screen.

3) Selection options will also be available for every assessment saved in the DST assessments folder. This includes the "Tutorial"" example we will use here, base assessments distributed with the DST, and any additional assessments you have previously uploaded. Once you select an assessment name the DST will build a dynamic user management interface dependent on the specific features of the assessment *(Note: This can be an intensive process for assessments with seasonal dynamics and/or lots of fleets and therefore require a moderate delay in rendering. During the rendering process the message "Loading and building assessment model" will be displayed and you will see many progress bars popup in the lower right corner of the screen providing further detail. Once the rendering is almost complete the message will change to "Almost ready, preparing for model initialization". This message will finally change to a button labeled "Initialize Interface" click this button to proceed through to the management input page.)*. To proceed with this tutorial select the "Tutorial" option from the assessment selection list and click the "Initialize Interface" once it becomes available.     

*Management input page:* After clicking the "Initialize Interface" button the DST will move to the "management input" tab. At first sight this page is very busy and can be overwhelming, we will break down the components below to explore the three principle sections.    

![](Tutorial%20manual%20screenshots/Management%20Input.PNG)
Figure 3: Management input page.

*Global management input options:* The global management inputs section allows you to set the management start year, stock assessment population targets for calculating optimal fishing, and the expected fishing strategy to be implemented. Optimal fishing target options include:   
1) Spawning potential ratio, which is calculated as the equilibrium spawning biomass ratio expected for a given F and average recruitment. F will be adjusted such that the average spawning potential ratio during the target year range is equal to the target ratio.   
2) Spawning biomass ratio, which is calculated as the ratio of annual estimated spawning biomass to expected virgin spawning biomass. F will be adjusted such that the average spawning biomass ratio during the target year range is equal to the target ratio.   
3) Maximize catch, where catch is calculated as the total retained catch by the fishery. F will be adjusted such that the average retained catch during the target year range is maximized. Defaults to 100 years in the future to estimate maximum sustainable yield (MSY).  
4) Use previous estimate (only visible if assessment has been run before) allows you to reuse the last optimal target run which can save significant estimation time if you need to compare multiple implemented fishing options such as different fixed catch or F rates under the same allocation, selection, fixed catch, and population target conditions.   
Implemented fishing strategies available include implementing the optimal estimated strategy, fishing at a fixed fraction of Fmsy, or allocating a fixed constant catch which can be input as a raw value or as a fraction of MSY (fraction of MSY option is displayed if fixed value box is empty).  
There is also a reset values button to return all values to the base configuration, an update forecast button to run SS3 projections with all current inputs, and a forecast name input which will save the results of the run to a named folder (normally you will overwrite previous results each time you run update forecast).   

![](Tutorial%20manual%20screenshots/Projection%20target%20options.PNG)
Figure 4: Projection target option inputs.

For this tutorial try changing the optimal fishing target from "Spawning Biomass Ratio" to "Maximize Catch"  

*Stock allocation and catch history input tab:* Below the global inputs are a number of input tabs. The first of these which will be selected when you open the management action page is the allocations and catch tab. As the title states this tab is largely dedicated to setting relative fleet allocations and fixed catch values. The tab inputs are arranged into three columns: 1) Group specific allocation controls, 2) Fleet specific allocation controls, and 3) fleet specific fixed catch inputs.  
1) The group column starts with a radio selector for the group specific allocation basis with can be selected as dead weight, retained weight, dead numbers, or retained numbers. A slider is then available to match allocations to the average of a historic time period. The rest of the column is separated into a segment for each group. Each segment contains an input field to allow you to set the fraction of the catch allocated to that group and a radio selector to determine if allocation among fleets within the group is determined by catch or F. Allocation among groups will automatically re-scale to sum to one. Beside each group is a check-box which can be selected to fix that groups allocation when re-scaling.      
2) The fleet specific controls  starts with a radio selector to show allocation fractions to be displayed as annual fractions or by season, this also controls the display of fixed fleet inputs. Input fields are then available for each fleet to set the relative allocation among fleets within each group. Fleet allocations will automatically re-scale to sum to one within each group, check-boxes can also be selected to fix these values.     
3) Fixed catch inputs are available for each fleet for all years before management starting year. If an allocation fraction is not assigned to a fleet then fixed inputs are also available annually for three years after management is implemented and then as a fixed constant value for all subsequent years.  

![](Tutorial%20manual%20screenshots/Allocation%20tab.PNG)
Figure 5: Allocation and fixed catch inputs.

For this tutorial try adjusting the group allocation fraction so that the commercial fishery receives 45% of the catch and the recreational receives 55%. Now select the "Recreational Selectivity" tab.   

*Fleet selectivity tabs:* After selecting the "Recreational Selectivity" tab you will see a row for each fleet. Each row is comprised of a selectivity at length plot on the left and four user adjustable selection parameter sliders on the right. The first three sliders allow you to adjust the shape of the logistic retention function through its inflection point, slope, and asymptotic maximum. The final slider adjusts the discard mortality rate.   

![](Tutorial%20manual%20screenshots/Selectivity.PNG)
Figure 6: Fleet retention and discard mortality inputs.

For this forecast try setting discard mortality for all recreational fleets to zero. You see this update the plot as the red dead discards disappear (Figure 7). Also go to the "By-catch Selectivity" tab and set discard mortality to zero for "Recreational Closed East" and "Recreational Closed West".   

![](Tutorial%20manual%20screenshots/Zero%20Discard%20Mortality.PNG)
Figure 7: Fleet retention and discard mortality inputs.

If you select the "Allocations and Catch"" tab and have made all the changes above your screen should look like figure 8 below. You are now ready to run a new forecast but first click the "Forecast Results" tab to view the base model projections.   

*Base assessment results plotting:* Once you click on the "Forecast Results" tab you will see plotting options for the base model results (Figure 8).   

![](Tutorial%20manual%20screenshots/Base%20Results.PNG)
Figure 8: Base assessment forecast results interface.

Here you can see a number of radio button selectors and slider bar inputs and a plot output which will display different outputs depending on the radio and slider selections. Input selectors are first projection display data with options for: A) Landings, B) Dead Discards, C) Harvest Rate, D) SSB, E) Population Biomass, and F) Recruits. Depending on the selection here a number of additional selectors will be available. For all options a years to display slider is shown allowing the year range of the plot to be chosen and if applicable to the assessment options to aggregate results be area and/or season. If landings, dead discards, or harvest rate are selected then an option to display total, group specific, or fleet specific results is shown. If fleet or group specific display is chosen then additional toggles for each fleet or group are shown to allow a choice of which fleets/groups to display. When landings or dead discards are selected a units option is also shown to allow the data to be presented in numbers or weight.   

*Projection results page:* Once you have explored the base results return to the "Management Action" tab and click the "Update Forecast" button. The DST will now build a modified assessment with new fleets which implement the allocation and selectivity patterns you just selected. A search loop will now run to calculate the F required to maximize catch between 2100 and 2114. You will see the progress of this search as a progress bar in the lower right hand corner of the screen (Figure 9).      

![](Tutorial%20manual%20screenshots/Running%20Assessment.PNG)
Figure 9: Running assessment forecast progress box.

Once the search is completed the DST will automatically take you to the "Forecast Results" tab which will now be enhanced with additional plotting options and download capabilities. One of these is the Kobe matrix (Figure 10) which which tracks B/Bmsy and F/Fmsy through time. Fmsy and Bmsy are calculated as the values of F and population biomass in your target period assuming that if SPR or Spawning Biomass targets where chosen they where done so as MSY proxies.          

![](Tutorial%20manual%20screenshots/Kobe%20Results.PNG)
Figure 10: Kobe plot results.

The new results also allow comparison between the base, new target, and applied fishing results. These can be displayed as absolute differences, percentage differences, or as a ratio of one to the other. A range of download options are also available called: 1) "ICCAT"; 2) "Displayed Figure with Data"; and 3) "SS3 Report file".    
1) The ICCAT selection downloads a csv file containing Yield, SSB, Biomass, and F at three time points of the first year of management, the tenth year of management, and equilibrium (i.e. MSY). The first and tenth year relative to equilibrium are also included.    
2) The Displayed figure with data saves the current figure along with the specific data needed to replicate it and the detailed data needed to recreate any of the figure options.   
3) The SS3 report option simply downloads the SS3 report file that contains all pertinent information from the assessment in case you want to develop your own plots or get more detailed information.    

Take your time to explore these results and then go try your own adjustments on the tutorial, any of the other available assessments, or upload your own SS3.24 assessment and try it out. If you experience any bugs, have feature requests, or just can't seem to get things to work you can email me at nathan.vaughan@noaa.gov for assistance. Best of luck.  