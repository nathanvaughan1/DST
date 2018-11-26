#' new Text
#'
#' Inserts a new shiny text object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param inputText the displayed text.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newtext<-function(location,id,inputText,placement="beforeEnd",hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = strong(inputText,id=id),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(strong(inputText,id=id)),
      immediate=immediate
    )
  }

}

#' new download
#'
#' Inserts a new shiny download button object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param label the displayed label.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newDownload<-function(location,id,label=NULL,placement="beforeEnd",hidden=FALSE,immediate=FALSE){

  if(hidden==FALSE){
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = downloadButton(outputId = id, label=label),
      immediate=immediate
    )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(downloadButton(outputId = id, label=label)),
      immediate=immediate
    )
  }
}

#' new checkbox group
#'
#' Inserts a new shiny checkbox group object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param label the displayed label.
#' @param choices the available choices.
#' @param selected the selected choice at initialization.
#' @param choiceNames displayed names of the choices.
#' @param choiceValues the server side value of the choices.
#' @param inline should the choices be inline or vertical.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newCheckBoxGroup<-function(location,id,label=NULL,placement="beforeEnd",choices=NULL,selected=NULL,choiceNames=NULL,choiceValues=NULL,inline=TRUE,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = checkboxGroupInput(inputId = id, label=label,choices=choices,selected=selected,choiceNames = choiceNames, choiceValues = choiceValues,inline=inline),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(checkboxGroupInput(inputId = id, label=label,choices=choices,selected=selected,choiceNames = choiceNames, choiceValues = choiceValues,inline=inline)),
      immediate=immediate
    )
  }
}

#' new select input
#'
#' Inserts a new shiny select input object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param label the displayed label.
#' @param choices the available choices.
#' @param selected the selected choice at initialization.
#' @param multiple can multiple choices be selected at once.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newSelectInput<-function(location,id,label=NULL,placement="beforeEnd",choices=NULL,selected=NULL,multiple=FALSE,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = selectInput(inputId = id, label=label,choices=choices,selected=selected,multiple=multiple),
      immediate=immediate
    )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(selectInput(inputId = id, label=label,choices=choices,selected=selected,multiple=multiple)),
      immediate=immediate
    )
  }


}

#' new value input
#'
#' Inserts a new shiny value input object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param label the displayed label.
#' @param value the displayed value.
#' @param min minimum allowed value.
#' @param max maximum allowed value.
#' @param step step size for value choices.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newValueInput<-function(location,id,value=NULL,placement="beforeEnd",label=NULL,min=0,max=1,step=0.001,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = numericInput(inputId=id,label=label,value=value,min=min,max=max,step=step),
      immediate=immediate
    )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(numericInput(inputId=id,label=label,value=value,min=min,max=max,step=step)),
      immediate=immediate
    )
  }


}

#' new checkbox
#'
#' Inserts a new shiny checkbox object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param label the displayed label.
#' @param width width of checkbox.
#' @param value true or false.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newCheckbox<-function(location,id,label="",width=NULL,value=FALSE,placement="beforeEnd",hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = checkboxInput(inputId=id,label=label,value=value,width=width),
      immediate=immediate
    )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(ui = checkboxInput(inputId=id,label=label,value=value,width=width)),
      immediate=immediate
    )
  }
}

#' new Text input
#'
#' Inserts a new shiny text input object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param value the displayed text.
#' @param label the displayed label.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newtextInput<-function(location,id,value="",placement="beforeEnd",label=NULL,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = strong(textInput(inputId=id,label=label,value=value)),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(strong(textInput(inputId=id,label=label,value=value))),
      immediate=immediate
    )
  }


}

#' new plot
#'
#' Inserts a new shiny plot object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param x vector of x values
#' @param y vector of y values
#' @param height height of the plot
#' @param title plot title text
#' @param xlab x axis subtitle text
#' @param ylab y axis subtitle text
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @param input shiny input values list.
#' @param output shiny output values list.
#' @param session the current shiny session.
#' @author Nathan Vaughan
#' @keywords UI insertion
newPlot<-function(location,id,x=1:10,y=rnorm(10),height="400px",title="Main",xlab="X",ylab="Y",placement="beforeEnd",input=input,output=output,session=session,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = plotOutput(id, height=height),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(plotOutput(id, height=height)),
      immediate=immediate
    )
  }


}

#' new Text output
#'
#' Inserts a new shiny text output object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param inline object placement inline TRUE or FALSE
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newOutputtext<-function(location,id,placement="beforeEnd",inline=FALSE,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = strong(textOutput(id,inline = inline)),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(strong(textOutput(id,inline = inline))),
      immediate=immediate
    )
  }


}

#' new Text input area
#'
#' Inserts a new shiny text input area object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param titleText the displayed label.
#' @param placement Object placement relative to location object.
#' @param cols a number of columns
#' @param rows a number of rows
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newTextAreaInput<-function(location,id,titleText,placement="beforeEnd",cols=12,rows=20,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = textAreaInput(inputId = id,
                       label = titleText,value = "",
                       cols=cols, rows=rows),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(textAreaInput(inputId = id,
                         label = titleText,value = "",
                         cols=cols, rows=rows)),
      immediate=immediate
    )
  }


}

#' new slider input
#'
#' Inserts a new shiny slider input object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param titleText the displayed label.
#' @param value the displayed value.
#' @param min minimum allowed value.
#' @param max maximum allowed value.
#' @param step step size for value choices.
#' @param sep the separator for thousands between display values. default = ",".
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newSlider<-function(location,id,titleText,placement="beforeEnd",min=0,max=1,value=0.0,step=0.01,sep=",",hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = sliderInput(inputId = id,
                     label = titleText,
                     min = min, max = max, value = value,step=step,sep=sep),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(sliderInput(inputId = id,
                       label = titleText,
                       min = min, max = max, value = value,step=step,sep=sep)),
      immediate=immediate
    )
  }


}

#' new text slider
#'
#' Inserts a new shiny text slider input object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param placement Object placement relative to location object.
#' @param label the displayed label.
#' @param choices available options.
#' @param selected selected option.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newTextSlider<-function(location,id,label,placement="beforeEnd",choices,selected,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = sliderTextInput(inputId = id,
                         label = label,
                         choices=choices,
                         selected=selected),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(sliderTextInput(inputId = id,
                           label = label,
                           choices=choices,
                           selected=selected)),
      immediate=immediate
    )
  }


}

#' new column
#'
#' Inserts a new shiny column object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param placement Object placement relative to location object.
#' @param width column width an integer between 1-12.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newColumn<-function(location,id,placement="beforeEnd",width=4,hidden=FALSE,immediate=TRUE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = column(width,id=id),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(column(width,id=id)),
      immediate=immediate
    )
  }


}

#' new row
#'
#' Inserts a new shiny row object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newFluidRow<-function(location,id,placement="beforeEnd",hidden=FALSE,immediate=TRUE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = fluidRow(id=id),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(fluidRow(id=id)),
      immediate=immediate
    )
  }


}

#' new radio
#'
#' Inserts a new shiny radio input object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param label the displayed label.
#' @param placement Object placement relative to location object.
#' @param choices available options.
#' @param selected selected option.
#' @param choiceNames displayed names of the choices.
#' @param choiceValues the server side value of the choices.
#' @param inline should the choices be inline or vertical.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newRadio<-function(location,id,placement="beforeEnd",label=NULL,choices=NULL,selected=NULL,choiceNames=NULL,choiceValues=NULL,inline=TRUE,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = radioButtons(inputId = id, label=label,choices=choices,selected=selected,choiceNames=choiceNames,choiceValues=choiceValues,inline=inline),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(radioButtons(inputId = id, label=label,choices=choices,selected=selected,choiceNames=choiceNames,choiceValues=choiceValues,inline=inline)),
      immediate=immediate
    )
  }


}

#' new tab panel
#'
#' Inserts a new shiny tab panel object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param titleText the displayed label.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newTabPanel<-function(location,id,titleText,placement="beforeEnd",hidden=FALSE,immediate=TRUE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = tabPanel(titleText,id=id),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(tabPanel(titleText,id=id)),
      immediate=immediate
    )
  }


}

#' new div
#'
#' Inserts a new html div from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newDiv<-function(location,id,placement="beforeEnd",hidden=FALSE,immediate=TRUE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = tags$div(id=id),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(tags$div(id=id)),
      immediate=immediate
    )
  }


}

#' new action button
#'
#' Inserts a new shiny action button object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param label the displayed label.
#' @param placement Object placement relative to location object.
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newActionButton<-function(location,id,placement="beforeEnd",label,hidden=FALSE,immediate=FALSE){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
  insertUI(
    selector = paste0("#",location),
    where = placement,
    ui = actionButton(inputId=id,label=label),
    immediate=immediate
  )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = hidden(actionButton(inputId=id,label=label)),
      immediate=immediate
    )
  }


}

#' new modal
#'
#' Inserts a new shiny BS modal object from the
#' server code. Wraps the insert UI function.
#'
#' @param location The existing object that the new shiny object will
#' be placed relative to.
#' @param id the id of the new shiny object.
#' @param label the displayed label.
#' @param placement Object placement relative to location object.
#' @param ui the ui to be included in the modal
#' @param hidden Should the object be hidded when created. Default = FALSE.
#' @param immediate Should the object be created immediately. Default = FALSE.
#' @author Nathan Vaughan
#' @keywords UI insertion
newBSModal<-function(location,id,placement="beforeEnd",label,hidden=FALSE,immediate=FALSE,ui=NULL){
  removeUI(paste0("#",id,">*"),multiple=TRUE,immediate=TRUE)
  removeUI(paste0("#",id,""),multiple=TRUE,immediate=TRUE)

  if(hidden==FALSE){
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = bsModal(id=id,title=label,{ui}),
      immediate=immediate
    )
  }else{
    insertUI(
      selector = paste0("#",location),
      where = placement,
      ui = bsModal(id=id,title=label,{ui}),
      immediate=immediate
    )
  }


}
