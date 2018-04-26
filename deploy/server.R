library(shiny)
source("./centileFunctions.R")
source("./bloodPressure.R")


today <- Sys.Date()

#list of centiles to plot
reqCents <- c(2,5,8,25,50,75,92,95,98);

#stature data US
D_US_male_height   <- generateCentileDataFrame( LMS.params.stat.male.US, reqCents );   #generate centile data frame for plotting
D_US_female_height <- generateCentileDataFrame( LMS.params.stat.female.US, reqCents );   #generate centile data frame for plotting

#blood pressure parameters US
BP.p               <- getBP_US();

preparePlot <- function( plotType, params ) {
     if( params$child.sex == "male" ) {
           D <- D_US_male_height;
      } else {
           D <- D_US_female_height;
      }
                     
     if( plotType == "height") {
          plotStr <- sprintf("Age %.1f Years \nHeight %.1f \nCentile %.0f", 
          params$child.months / 12, params$child.height, params$height.cent )
          this.plot <- plotCentileChart( D, params$child.months, params$child.height, 
          params$child.sex, plotStr, "Age (Months)", "Height (cms)",
          paste("Height for Age (", params$child.sex ,")" , sep = ""))     
          print( this.plot );     
     }

     if( plotType == "sysbp" ) {               
          plotStr <- sprintf("Age %.1f Years \nSys. BP %.1f \nCentile %.0f", 
                         params$child.months / 12, params$child.sys.bp, params$child.sys.cent )               
          plotTitle <- sprintf("Systolic BP for Age (%s) given Height %.1f (%.0f Centile)", 
                                   params$child.sex, params$child.height, params$height.cent )               
          sysPlot <- generateBloodPressure.US("male", params$height.Zht, BP.p, reqCents )$sysTab;               
          this.plot <- plotCentileChart( sysPlot, params$child.months, params$child.sys.bp, 
                                              params$child.sex, plotStr, "Age (Months)", "Systolic BP",
                                              plotTitle)     
          print( this.plot )
     
     }             
     
     if( plotType == "diabp" ) {               
          plotStr <- sprintf("Age %.1f Years \nDia. BP %.1f \nCentile %.0f", 
                             params$child.months / 12, params$child.dia.bp, params$child.dia.cent )               
          plotTitle <- sprintf("Diastolic BP for Age (%s) given Height %.1f (%.0f Centile)", 
                               params$child.sex, params$child.height, params$height.cent )               
          diaPlot <- generateBloodPressure.US("male", params$height.Zht, BP.p, reqCents )$diaTab;               
          this.plot <- plotCentileChart( diaPlot, params$child.months, params$child.dia.bp, 
                                         params$child.sex, plotStr, "Age (Months)", "Diastolic BP",
                                         plotTitle)     
          print( this.plot )
          
     }      
          
}

function(input, output) {

     dataInput.All <- reactive({
          #read variables relevant to height centile calculations                    
          
          this.Zht <- LMS.stat.Z( elapsedMonths( today, input$child_dob ),
                      input$child_sex, 
                      input$child_height, 
                      input$app_loc)
               
          bps <- Z.bloodPressure.US( 
                         this.Zht, BP.p, 
                         input$child_sex, elapsedMonths( today, input$child_dob ), 
                         input$child_sys_bp, input$child_dia_bp )
          
          
          this.sys.cent <- bps$c.Sys
          this.dia.cent <- bps$c.Dia
          
          list(
               child.sex = input$child_sex,
               child.dob = input$child_dob,
               child.months = elapsedMonths( today, input$child_dob ),
               child.height = input$child_height,

               height.Zht = this.Zht,
               
               height.cent = centileFromZ( this.Zht ),
               
               child.sys.bp = input$child_sys_bp,
               child.dia.bp = input$child_dia_bp,
               child.sys.cent  = this.sys.cent,
               child.dia.cent  = this.dia.cent               
          )
     })
               
     
     output$summary <- renderUI({
          
          params  <- dataInput.All();
     
          
          str1 <- paste("Gender : ", params$child.sex );
          str2 <- paste("DoB : ",  format(params$child.dob,'%d-%m-%Y') );
          str3 <- sprintf("Age : %.0f months, %.1f years", 
                              params$child.months, params$child.months / 12);
          str4 <- sprintf("Height : %.1f centimeters (%.1f metres) = %.0f Centile", 
                              params$child.height, params$child.height / 100, params$height.cent);
          
          str5 <- sprintf("Systolic BP : %.2f = %.0f Centile", 
                          params$child.sys.bp, params$child.sys.cent);
          str6 <- sprintf("Diastolic BP : %.2f = %.0f Centile", 
                          params$child.dia.bp, params$child.dia.cent);
     
          
          HTML(paste(str1, str2, str3, str4, str5, str6,  sep = '<br/>'))
          
     })

     output$plot <- renderPlot({
       # Take a dependency on input$goButton
          input$plot_now
          isolate({   
               params <- dataInput.All();
               thisP <- preparePlot( input$plot_type, params ); 
          })               
     })
  
     
     output$about <- renderUI({
          includeHTML("about_txt.html")          
     })
     
}
