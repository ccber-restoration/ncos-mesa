# this regression equation is from "NCOS_Biochar_EC_Correlation_FGL_KingLab
# it is based on 26 samples for which conductivity was measured both using the 1:5 ratio method and also analyzed by FGL (Fruit Growers Lab)

# y = 0.0061x + 1.567

#where y = estimated conductivity in dS/m and x = measured conductivity in microS/cm


uS_per_cm_to_dS_per_m <- function(x) {
  estimated_dS_per_m <- 0.0061*x + 1.567
  return(estimated_dS_per_m)
  } 


#test

uS_per_cm_to_dS_per_m(934)
#7.2644


uS_per_cm_to_dS_per_m(473.3)

uS_per_cm_to_dS_per_m(4210)


#next steps: for the data used to create the regression, compare estimated to observed values as percent error... 

