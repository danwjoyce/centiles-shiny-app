#this is for the BP centiles based on The Fourth Report on the Diagnosis, Evaluation, and Treatment of
#High Blood Pressure in Children and Adolescents, Paediatrics, 2004, Vol. 114 No. 2 August 2004
BP.p <- read.csv('./data/BP_params_US_2004.csv')
#note, that even tho paper mentions height in inches,
#the Z score (returned from LMS.stat.Z)  is derived from a centremetre LMS equation
#from the CDC data - in essence, the Z score for a child height is unit insensitive

getBP_US <- function() {
     return( BP.p )
}

calcBP_Regression <- function( alpha, betas, gammas, ageMonths, Zht ) {
     
     #make sure we use years not months
     y <- ageMonths / 12;
     
     term1  <- 0.0;  for( j in 1:4 ) { term1 <- term1 + betas[j] * (y-10)^j }
     term2  <- 0.0;  for( k in 1:4 ) { term2 <- term2 + gammas[k] * (Zht)^k }
     
     
     #expected (mean) systolic blood pressure from calaculation
     return( alpha + term1 + term2 );
     
}

bloodPressure.US <- function( Zht, BP.p, sexOfChild, ageOfChild_months ) {
     #compute blood pressure using normed data and regression equation from US paper
     #Paediatrics, 2004, Vol. 114 No. 2 August 2004
     
     #fetch params
     if( sexOfChild == "male" ) {
          params.Sys <- BP.p[ which( BP.p$Gender == "Male" & BP.p$BPvar == "Sys" ), ]
          params.Dia <- BP.p[ which( BP.p$Gender == "Male" & BP.p$BPvar == "Dia" ), ]
     } else {
          params.Sys <- BP.p[ which( BP.p$Gender == "Female" & BP.p$BPvar == "Sys" ), ]
          params.Dia <- BP.p[ which( BP.p$Gender == "Female" & BP.p$BPvar == "Dia" ), ]
          
     }
     
     
     #1 : compute expected systolic for age and height
     betas  <- c(params.Sys$B1, params.Sys$B2, params.Sys$B3, params.Sys$B4);
     gammas <- c(params.Sys$G1, params.Sys$G2, params.Sys$G3, params.Sys$G4);
     alpha  <- params.Sys$alpha;
     stdDev <- params.Sys$SD;
     SBP    <- calcBP_Regression( alpha, betas, gammas, ageOfChild_months, Zht );
     ###
     
     #1 : compute diastolic for age and height
     betas  <- c(params.Dia$B1, params.Dia$B2, params.Dia$B3, params.Dia$B4);
     gammas <- c(params.Dia$G1, params.Dia$G2, params.Dia$G3, params.Dia$G4);
     alpha  <- params.Dia$alpha;
     stdDev <- params.Dia$SD;
     DBP    <- calcBP_Regression( alpha, betas, gammas, ageOfChild_months, Zht );
     
     return( list( e.SBP = SBP, e.DBP = DBP ))
     
}

Z.bloodPressure.US <- function( Zht, BP.p, sexOfChild, ageOfChild_months, recorded_SBP, recorded_DBP ) {
     #given Z score height (Zht), params for US data (BP.p)
     #sexOfChile = "male" or "female", ageOfChild_months, recorded Systolic BP (SBP) and diastolic (DBP)
     #compute the Z score and centile of the child
     
     #get stand dev params for sex 
     #fetch params
     if( sexOfChild == "male" ) {
          params.Sys <- BP.p[ which( BP.p$Gender == "Male" & BP.p$BPvar == "Sys" ), ]
          params.Dia <- BP.p[ which( BP.p$Gender == "Male" & BP.p$BPvar == "Dia" ), ]
     } else {
          params.Sys <- BP.p[ which( BP.p$Gender == "Female" & BP.p$BPvar == "Sys" ), ]
          params.Dia <- BP.p[ which( BP.p$Gender == "Female" & BP.p$BPvar == "Dia" ), ]
          
     }
     
     SD.Sys <- params.Sys$SD;
     SD.Dia <- params.Dia$SD;
     
     e.BP <- bloodPressure.US( Zht, BP.p, sexOfChild, ageOfChild_months );
     e.Sys <- e.BP$e.SBP;
     e.Dia <- e.BP$e.DBP;
     
     Z.Sys <- (recorded_SBP - e.Sys) / SD.Sys;
     Z.Dia <- (recorded_DBP - e.Dia) / SD.Dia;
     
     #area under normal curve * 100 for centile.
     centile.Sys <- pnorm( Z.Sys ) * 100;
     centile.Dia <- pnorm( Z.Dia ) * 100;
     
     return( list( c.Sys = centile.Sys, c.Dia = centile.Dia  ) );
     
}



##Note the formula for BP centile calculations in Paediatrics (2004) upon which this is based
##are tabulated from ages 1 through to 17.  We use the same age range, but "finer" resolution
##assuming that the equations can be interpolated in 1 month intervals
##cf the calculations for height centiles which are in months

generateBloodPressure.US <- function( sexOfChild, Zht, BP.p, reqCents ) {
     #fetch stand dev params for sex and sys or dia
     if( sexOfChild == "male" ) {
          SD.sys <- BP.p[ which( BP.p$Gender == "Male" & BP.p$BPvar == "Sys" ), ]$SD
          SD.dia <- BP.p[ which( BP.p$Gender == "Male" & BP.p$BPvar == "Dia" ), ]$SD
     } else {
          SD.sys <- BP.p[ which( BP.p$Gender == "Female" & BP.p$BPvar == "Sys" ), ]$SD
          SD.dia <- BP.p[ which( BP.p$Gender == "Female" & BP.p$BPvar == "Dia" ), ]$SD
     }
     
     
     ageV <- seq(from = 12, to = (17 * 12), by = 1)
     sysMatrix <- matrix(0, length( ageV ), length( reqCents ) )
     diaMatrix <- matrix(0, length( ageV ), length( reqCents ) )
     
     colnames(sysMatrix) <- paste( "C", reqCents, sep = "")
     colnames(diaMatrix) <- paste( "C", reqCents, sep = "")
     
     for( i in 1:length( reqCents ) ) {
          sysMatrix[,i] <- qnorm( reqCents[i] / 100 ) * SD.sys + bloodPressure.US( Zht, BP.p, sexOfChild, ageV )$e.SBP;
          diaMatrix[,i] <- qnorm( reqCents[i] / 100 ) * SD.dia + bloodPressure.US( Zht, BP.p, sexOfChild, ageV )$e.DBP;
     }
     
     ## for given sex, Zht return two dataframes of age x require centiles systolic and diastolic
     return( list( sysTab = data.frame( abscissa = ageV, sysMatrix),
                   diaTab = data.frame( abscissa = ageV, diaMatrix) ) )
}
