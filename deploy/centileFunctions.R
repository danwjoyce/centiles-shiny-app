library(reshape)
library(ggplot2)
library(grid)
#From : http://www.cdc.gov/growthcharts/percentile_data_files.htm
d <- read.csv('./data/LMS_stature_CDC.csv')

LMS.stat.male.US <- d[ d$Sex == 1, ];
LMS.stat.female.US <- d[ d$Sex == 2, ];

LMS.params.stat.male.US <- data.frame( months = LMS.stat.male.US$Agemos,
                                  L      = LMS.stat.male.US$L,
                                  M      = LMS.stat.male.US$M,
                                  S      = LMS.stat.male.US$S );

LMS.params.stat.female.US <- data.frame( months = LMS.stat.female.US$Agemos,
                                       L      = LMS.stat.female.US$L,
                                       M      = LMS.stat.female.US$M,
                                       S      = LMS.stat.female.US$S );


# #write out data in R binary format
# save( LMS.stat.male.US, file = "./data/LMS_stat_male_US.RData" )
# save( LMS.stat.female.US, file = "./data/LMS_stat_female_US.RData" )
rm( d )

#sanity checking functions DEPRECATED
# check_StatureData <- function( sex, ageMonths ) {
#      #returns a vector of either : the params passed, or 
#      #a set of params with minimum values set
#      ifelse( sex == "male" | sex == "female" , r.sex <- sex, r.sex <- "male" );
#      ifelse( ageMonths < 24 & ageMonths > 240, r.ageMonths <- ageMonths, r.ageMonths <- 24 );
#           
# }

retrieveStatData <- function( sex, loc ) {
     if( loc == "US" ) {
          if( sex == "male" ) {
               d <- LMS.stat.male.US;     
          } else {
               d <- LMS.stat.female.US;
          }     
     } else {
          if( loc == "UK") {
               if( sex == "male" ) {
                    d <- LMS.stat.male.US;     #THIS RETURNS US CDC at the moment !!! When
                                                  #we have UK charts, this can be modifed
               } else {
                    d <- LMS.stat.female.US;
               }                    
          }
          
     }
     return( d )
}

lookupLMS.stat <- function( months, sex, loc ) {
     #assumes globals LMS.stat.male.US and LMS.stat.male.UK and female respectively     

     d <- retrieveStatData( sex, loc );
     
     #sanity check the months param :
     if( 
          ( months > d$Agemos[ length( d$Agemos ) - 1 ] ) |
               ( months < d$Agemos[ 1 ] )
     ) {
          return( list( L = -1, M = -1, S = -1 ) );
     } else {
          
          indx <- max( which( d$Agemos <= months ) ) + 1;
          return( list( L = d$L[ indx ], M = d$M[ indx ], S = d$S[ indx ] ) );
          
     }
}



convertStrDates <- function(tDay, tMonth, tYear) {
     return( sprintf("%4d-%02d-%02d", tYear, tMonth, tDay) )
}

elapsedMonths <- function(end_date, start_date) {
     ed <- as.POSIXlt(end_date)
     sd <- as.POSIXlt(start_date)
     12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}



#equations for LMS to z score and percentiles are from :
#Flegal KM, Cole TJ. Construction of LMS parameters for the Centers for Disease Control and Prevention 2000
#growth chart. National health statistics reports; 
#no 63. Hyattsville, MD: National Center for Health Statistics. 2013.

generalZ <- function( L, M, S, X ) {
     #given anthropomorphic value X, compute centile from LMS params
     if( L == 0 ) {
          return( log(X/M)/S );
     } else {
          return( ( ( (X/M) ^ L) - 1 ) / (L * S) );
     }
}

generalX <- function( L, M, S, Z ) {
     #given a centile, compute the corresponding anthropomoprhic value X
     #note Z must be in standard deviation units (i.e. in R syntax : qnorm( percentile / 100 ))
     #so easier to compute first then call generalX
     if( L == 0 ) {
          return( M * exp( S * Z ) );
     } else {
          return( M * ( 1 + L * S * Z )^(1/L) );
     }
     
}

LMS.stat.Z <- function( months, sex, height, loc ) {
     #here, X is the height in cm
     #months is the age in months (funnily enough)
     #and sex = {"male", "female"}
     params <- lookupLMS.stat( months, sex, loc );
     L <- params$L; M <- params$M; S <- params$S;
     return( generalZ( L, M, S, height ) )         
}


centileFromZ <- function( Z ) {
     return( pnorm( Z ) * 100 )
}


generateCentileDataFrame <- function( LMS, reqCents ) {
     #takes a list of LMS params, and generates a ggplot compatible (long format) data frame
     #to faciliate plotting - this should be called at the startup of the application to avoid
     #duplications
     
     #LMS : a dataframe of month, L, M and S params (in that ordered, labelled)
     #reqCents = vector of required percentiles (e.g. c(3,7, 50, 75, 99) )
     
     #initialise a matrix to store the tabulated centiles
     nRows <- dim( LMS )[ 1 ];
     nCols <- length( reqCents ) + 1;
     tab   <- matrix(0, nrow = nRows, ncol = nCols);
     
     #convert centiles (as % e.g. 5, 95 ) to standard deviation units
     cents <- qnorm( reqCents / 100 );
     for( i in 1:nRows ) {
          #for each month in LMS
               L <- LMS$L[i]; M <- LMS$M[i]; S <- LMS$S[i];          
               tab[i,1] <- LMS$months[i];     
          
          #for each required centile 
               for( j in 2:nCols ) {
                    tab[i,j] <- generalX( L, M, S, cents[j-1] );                    
               }
          
     }
     my.df <- data.frame( tab );
     colnames(my.df) <- c("abscissa", paste( "C", reqCents, sep = "") );
     return( my.df )     
}

     
     
plotCentileChart <- function( D, xVal, yVal, sex, pointTxt, xLabel, yLabel, plotTitle ) {
     #takes a tabulation (D) as data.frame produced by generateCentileDataFrame and 
     #the formats for ggplot (by reshape) and then plots
     
     centList <- names( D )[2:length(names(D))];
     nCents <- (length( names(D) ) - 2)/2;
     
     minAbs <- min( D$abscissa );
     maxAbs <- max( D$abscissa );

     minOrd <- min( D[,2:dim(D)[2]] );
     maxOrd <- max( D[,2:dim(D)[2]] );
     
     #alphaLevels <- seq( 1/nCents, 0.2, length.out = nCents );
     alphaLevels <- rep( 0.08, length.out = nCents );
     alphaLevels[ nCents ] <- 0.15
     
     colCents <- dim( D )[2] + 1;
     
     if( sex == "male" ) {
          plotColour <- "blue"
     } else {
               
          plotColour <- "red";
     }
     
     PP <- ggplot(D, aes(x=abscissa, y = C50 ))
     PP <- PP + theme(panel.background = element_rect(fill="white"))
          #geom_line()
     for( i in 1:nCents ) {
          PP <- PP + geom_ribbon( ymin=D[ , i+1 ], ymax=D[ , colCents - i ], colour = "grey50",
                                  fill=plotColour, alpha = alphaLevels[i] )  
     }
     PP <- PP + geom_line(size = 1.25)  #plot 50th / median centile line
     
     
     if( xVal >= minAbs & xVal <= maxAbs ) {
          subLoc <- data.frame( xX = xVal, yY = yVal );
          PP <- PP + geom_hline(yintercept=yVal) + geom_vline(xintercept=xVal) 
          PP <- PP + geom_point( data = subLoc, aes( x = xX, y = yY), colour = "red", size = 4.0 )
     
          if( pointTxt != "" ) {
               #if there is a label for the point in xVal, yVal, print it
               #compute percentage offset from xVal, yVal
               
               
               my.grob = grobTree(textGrob(pointTxt, x=0.70,  y=0.2, hjust=0,
                                           gp=gpar(col="black", fontsize=15 )))
     
               PP <- PP + annotation_custom(my.grob)
          }
     }

     #add legend
     legLocX <- round( minAbs + (maxAbs - minAbs)/2 );
     legX    <- D$abscissa[ max( which( D$abscissa <= legLocX ) ) ];
     legLocY <- D[ max( which( D$abscissa <= legLocX ) ), 2:dim(D)[2] ];
     
     for( i in 1:length( centList ) ) {
          PP <- PP + annotate("text", x = legX, y = legLocY[1,i], label = centList[i], colour = "black", size = 3 )
     }
     
     #title up this bad boy ...
     PP <- PP + xlab(xLabel) + ylab(yLabel) + ggtitle( plotTitle )
     
     return( PP )
     
}

plotZoomStatureChart <- function( D, xVal, yVal, pointTxt, xWidth ) {
     #wraps plotStatureChart, but "zooms" into a predefined bracket around
     #xVal where xWidth is a percentage of maximum value on x-axis
     
     minAbs <- min( D$abscissa );
     maxAbs <- max( D$abscissa );
     
     #minOrd <- min( D[,2:dim(D)[2]] );
     #maxOrd <- max( D[,2:dim(D)[2]] );
     lower.Abs <- (xWidth / 100) * xVal
     
}