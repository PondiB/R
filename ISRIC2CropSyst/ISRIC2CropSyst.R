#batch Extract Cropsyst Soil Files
#author: John Mutua

#workspace clearance
rm(list = ls(all = TRUE))

require(raster) 
require(rgdal)

setwd("D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_Script")

#source dir
src.dir <- "D:/ToBackup/Scripts/R/ISRIC2CropSyst/src"

#calculate Curve Number
source(paste(src.dir,"/CN_Calculator.R",sep="")) #loading the script

#calculate Hydrologic Soil groups
source(paste(src.dir,"/CN2HSG_Calculator.R",sep="")) #loading the script

#calculate slope
source(paste(src.dir,"/CalculateSlope.R",sep="")) #loading the script

#calculate slope length
source(paste(src.dir,"/CalculateSlopeLength.R",sep="")) #loading the script

#read and load the soil property .tif files
rasList <- list.files(".", pattern = ".tif$", full.names = TRUE)
rStack<-stack(rasList)

#load and read the point shapefile
p <- shapefile("D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_Script/Farmer_sites.shp")
p <- spTransform(p, crs(rStack))

#extract data from .tif files
df1 <- as.data.frame(extract(rStack, p))

#recode HSG values to HSG groups
df1$HSG_code <- ifelse(df1$HSG==1, "A",
                       ifelse(df1$HSG==2, "B",
                              ifelse(df1$HSG==3, "C",
                                     ifelse(df1$HSG==4, "D",
                                            NA ))))

#define hydrologic conditions
df1$HC <- ifelse(df1$HSG==1, "GOOD",
                 ifelse(df1$HSG==2, "GOOD",
                        ifelse(df1$HSG==3, "GOOD",
                               ifelse(df1$HSG==4, "GOOD",
                                      NA ))))
#set thickness per layer
L1=0.05
L2=0.1
L3=0.15
L4=0.30
L5=0.40
L6=1

#some user values
u1=15
u2=15
u3=15
u4=15
u5=0
u6=0


###START LOOP###
for (i in 1:nrow(df1)){
  
  #create the final file to write all the parameters
  file<- paste(row.names(df1)[i], "soil.sil", sep="_")
  
  #write headers
  write.table("[soil]\ndetails_URL=\ndescription=",file, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #extracting curve numbers, slope, slope length
  CN <- df1$CN[i]
  slp <- df1$Sasumua_Slope[i]
  slpl <- df1$Sasumua_SlopeLength[i]
  hg<-df1$HSG_code[i]
  hc<-df1$HC[i]
  
    #set number of soil layers
  n_layers <- 6
  
  write.table(paste0("hydrologic_group=",hg), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("hydrologic_condition=",hc), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("override_curve_number=",""), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("compute_surface_storage=","true"), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("steepness=",slp), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("slope_length=",slpl), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("albedo_dry=",""), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("albedo_wet=",""), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("water_vapor_conductance_atmosphere_adj=",""), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("surface_storage=",""), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("fallow_curve_number=",CN), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("[version]",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("major=","0"), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("release","0"), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("minor=","0"), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("[profile]",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("number_layers=",n_layers), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("[evaporative_layer_thickness]",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("cascade=",L1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("finite_diff=",""), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("[CropGro]",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("SLPF=",""), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)

  
  #################READING SOIL LAYERS#########################
  #exract CLAY per layer, unit (%)
  CLAY1 <- df1$af_CLYPPT_T__M_sd1_250m[i]
  CLAY2 <- df1$af_CLYPPT_T__M_sd2_250m[i]
  CLAY3 <- df1$af_CLYPPT_T__M_sd3_250m[i]
  CLAY4 <- df1$af_CLYPPT_T__M_sd4_250m[i]
  CLAY5 <- df1$af_CLYPPT_T__M_sd5_250m[i]
  CLAY6 <- df1$af_CLYPPT_T__M_sd6_250m[i]
  
  #exract SAND per layer, unit (%)
  SAND1 <- df1$af_SNDPPT_T__M_sd1_250m[i]
  SAND2 <- df1$af_SNDPPT_T__M_sd2_250m[i]
  SAND3 <- df1$af_SNDPPT_T__M_sd3_250m[i]
  SAND4 <- df1$af_SNDPPT_T__M_sd4_250m[i]
  SAND5 <- df1$af_SNDPPT_T__M_sd5_250m[i]
  SAND6 <- df1$af_SNDPPT_T__M_sd6_250m[i]
  
  # #extract Bulk density in kg / m3; Divide ISRIC BLD by 1000
  # BLD1 <- df1$af_BLD_T__M_sd1_250m[i] / 1000
  # BLD2 <- df1$af_BLD_T__M_sd2_250m[i] / 1000
  # BLD3 <- df1$af_BLD_T__M_sd3_250m[i] / 1000
  # BLD4 <- df1$af_BLD_T__M_sd4_250m[i] / 1000
  # BLD5 <- df1$af_BLD_T__M_sd5_250m[i] / 1000
  # BLD6 <- df1$af_BLD_T__M_sd6_250m[i] / 1000
  
  #bulk density (BD) as per Minasny & Hartemink, Earth-Science Reviews 106 (2011) 52-62
  BLD1= 0.935 + 0.049 * log(L1) + 0.0055 * SAND1 + 0.000065 * (SAND1 - 38.96) ^ 2
  BLD2= 0.935 + 0.049 * log(L2) + 0.0055 * SAND2 + 0.000065 * (SAND2 - 38.96) ^ 2
  BLD3= 0.935 + 0.049 * log(L3) + 0.0055 * SAND3 + 0.000065 * (SAND3 - 38.96) ^ 2
  BLD4= 0.935 + 0.049 * log(L4) + 0.0055 * SAND4 + 0.000065 * (SAND4 - 38.96) ^ 2
  BLD5= 0.935 + 0.049 * log(L5) + 0.0055 * SAND5 + 0.000065 * (SAND5 - 38.96) ^ 2
  BLD6= 0.935 + 0.049 * log(L6) + 0.0055 * SAND6 + 0.000065 * (SAND6 - 38.96) ^ 2
  
  #extract Soil organic carbon (fine earth fraction) in permilles (g/kg)
  ORCDRC1 <- df1$af_ORCDRC_T__M_sd1_250m[i]/10 
  ORCDRC2 <- df1$af_ORCDRC_T__M_sd2_250m[i]/10 
  ORCDRC3 <- df1$af_ORCDRC_T__M_sd3_250m[i]/10 
  ORCDRC4 <- df1$af_ORCDRC_T__M_sd4_250m[i]/10 
  ORCDRC5 <- df1$af_ORCDRC_T__M_sd5_250m[i]/10 
  ORCDRC6 <- df1$af_ORCDRC_T__M_sd6_250m[i]/10 
  
  #exract SILT per layer, unit (%)
  SILT1 <- df1$af_SLTPPT_T__M_sd1_250m[i]
  SILT2 <- df1$af_SLTPPT_T__M_sd2_250m[i]
  SILT3 <- df1$af_SLTPPT_T__M_sd3_250m[i]
  SILT4 <- df1$af_SLTPPT_T__M_sd4_250m[i]
  SILT5 <- df1$af_SLTPPT_T__M_sd5_250m[i]
  SILT6 <- df1$af_SLTPPT_T__M_sd6_250m[i]
  
  #extract Soil PH, from ISRIC divide by 10
  PH1 <- df1$af_PHIHOX_T__M_sd1_250m[i] / 10
  PH2 <- df1$af_PHIHOX_T__M_sd2_250m[i] / 10
  PH3 <- df1$af_PHIHOX_T__M_sd3_250m[i] / 10
  PH4 <- df1$af_PHIHOX_T__M_sd4_250m[i] / 10
  PH5 <- df1$af_PHIHOX_T__M_sd5_250m[i] / 10
  PH6 <- df1$af_PHIHOX_T__M_sd6_250m[i] / 10
  
  #cation exchange capacity in cmol/kg
  CEC1 <- df1$af_CEC_T__M_sd1_250m[i]
  CEC2 <- df1$af_CEC_T__M_sd2_250m[i]
  CEC3 <- df1$af_CEC_T__M_sd3_250m[i]
  CEC4 <- df1$af_CEC_T__M_sd4_250m[i]
  CEC5 <- df1$af_CEC_T__M_sd5_250m[i]
  CEC6 <- df1$af_CEC_T__M_sd6_250m[i]
  
  ################WRITE SOIL DATA TO THE .SIL FILE#################
  #create a table to write some user parameters
  write.table("[user_value]",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  # Write Thickness parameters on the table
  write.table(paste0("1=",u1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",u2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",u3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",u4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",u5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",u6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
    
  #create a table to write Thickness parameters
  write.table("[thickness]\nunit=m",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  # Write Thickness parameters on the table
  write.table(paste0("1=",L1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",L2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",L3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",L4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",L5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",L6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Bulk Density parameters
  write.table("[bulk_density]\nunit=g/cm³",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  # Write Bulk Density parameters on the table
  write.table(paste0("1=",BLD1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",BLD2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",BLD3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",BLD4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",BLD5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",BLD6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Permanent wilting point parameters
  write.table("[perm_wilt_point]\nunit=m³/m³",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  # Calculate Permanent wilting point (PWP) as per Minasny & Hartemink, Earth-Science Reviews 106 (2011)
  PWP1 = (7.95 + 0.86 * ORCDRC1 + 0.4 * CLAY1 - 0.004 * (CLAY1 - 37.7) ^ 2) / 100
  PWP2 = (7.95 + 0.86 * ORCDRC2 + 0.4 * CLAY2 - 0.004 * (CLAY2 - 37.7) ^ 2) / 100
  PWP3 = (7.95 + 0.86 * ORCDRC3 + 0.4 * CLAY3 - 0.004 * (CLAY3 - 37.7) ^ 2) / 100
  PWP4 = (7.95 + 0.86 * ORCDRC4 + 0.4 * CLAY4 - 0.004 * (CLAY4 - 37.7) ^ 2) / 100
  PWP5 = (7.95 + 0.86 * ORCDRC5 + 0.4 * CLAY5 - 0.004 * (CLAY5 - 37.7) ^ 2) / 100
  PWP6 = (7.95 + 0.86 * ORCDRC6 + 0.4 * CLAY6 - 0.004 * (CLAY6 - 37.7) ^ 2) / 100
  
  #write Permanent wilting point parameters
  write.table(paste0("1=",PWP1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",PWP2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",PWP3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",PWP4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",PWP5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",PWP6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Field capacity (FC) parameters
  write.table("[field_capacity]\nunit=m³/m³",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #calculate Field capacity (FC) as per Minasny & Hartemink, Earth-Science Reviews 106 (2011) 52-62
  FC1 = (56.5 - 7.49 * BLD1 - 0.34 * SAND1) / 100
  FC2 = (56.5 - 7.49 * BLD2 - 0.34 * SAND2) / 100
  FC3 = (56.5 - 7.49 * BLD3 - 0.34 * SAND3) / 100
  FC4 = (56.5 - 7.49 * BLD4 - 0.34 * SAND4) / 100
  FC5 = (56.5 - 7.49 * BLD5 - 0.34 * SAND5) / 100
  FC6 = (56.5 - 7.49 * BLD6 - 0.34 * SAND6) / 100
  #write Field capacity parameters
  write.table(paste0("1=",FC1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",FC2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",FC3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",FC4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",FC5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",FC6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Soil Water potential at FC parameters
  write.table("[water_pot_at_FC]\nunit=kP",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #calculate Soil Water potential at FC (SWPFC), unit is (kPa)
  SWPFC1 = 0.0044 * CLAY1 ^ 2 + -0.78 * CLAY1 + -16 + (SILT1 / 93.5) ^ 4*-16
  SWPFC2 = 0.0044 * CLAY2 ^ 2 + -0.78 * CLAY2 + -16 + (SILT2 / 93.5) ^ 4*-16
  SWPFC3 = 0.0044 * CLAY3 ^ 2 + -0.78 * CLAY3 + -16 + (SILT3 / 93.5) ^ 4*-16
  SWPFC4 = 0.0044 * CLAY4 ^ 2 + -0.78 * CLAY4 + -16 + (SILT4 / 93.5) ^ 4*-16
  SWPFC5 = 0.0044 * CLAY5 ^ 2 + -0.78 * CLAY5 + -16 + (SILT5 / 93.5) ^ 4*-16
  SWPFC6 = 0.0044 * CLAY6 ^ 2 + -0.78 * CLAY6 + -16 + (SILT6 / 93.5) ^ 4*-16
  #write Soil Water potential at FC parameters
  write.table(paste0("1=",SWPFC1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",SWPFC2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",SWPFC3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",SWPFC4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",SWPFC5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",SWPFC6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Sand parameters
  write.table("[sand]\nunit=%",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #write SAND parameters
  write.table(paste0("1=",SAND1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",SAND2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",SAND3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",SAND4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",SAND5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",SAND6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Sand parameters
  write.table("[clay]\nunit=%",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  # Write CLAY parameters
  write.table(paste0("1=",CLAY1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",CLAY2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",CLAY3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",CLAY4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",CLAY5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",CLAY6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Silt parameters
  write.table("[silt]\nunit=%",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #write SILT parameters
  write.table(paste0("1=",SILT1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",SILT2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",SILT3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",SILT4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",SILT5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",SILT6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Saturated hydraulic conductivity parameters
  write.table("[sat_hydraul_cond]\nunit=m/day",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #first calculate Saturation (water content when soil is saturated, SWC), unit (m³/m³)
  SWC1 = 0.332 - 0.0007251 * SAND1 + log10(CLAY1) * 0.1276
  SWC2 = 0.332 - 0.0007251 * SAND2 + log10(CLAY2) * 0.1276
  SWC3 = 0.332 - 0.0007251 * SAND3 + log10(CLAY3) * 0.1276
  SWC4 = 0.332 - 0.0007251 * SAND4 + log10(CLAY4) * 0.1276
  SWC5 = 0.332 - 0.0007251 * SAND5 + log10(CLAY5) * 0.1276
  SWC6 = 0.332 - 0.0007251 * SAND6 + log10(CLAY6) * 0.1276
  #second Calculate parameter B
  B1 = (log(1500)-log(33))/(log(FC1)-log(SWC1))
  B2 = (log(1500)-log(33))/(log(FC2)-log(SWC2))
  B3 = (log(1500)-log(33))/(log(FC3)-log(SWC3))
  B4 = (log(1500)-log(33))/(log(FC4)-log(SWC4))
  B5 = (log(1500)-log(33))/(log(FC5)-log(SWC5))
  B6 = (log(1500)-log(33))/(log(FC6)-log(SWC6))
  #calculate Saturated hydraulic conductivity (Ks, mm/h) - divided by 1000 and multiplied by 24 to get to (m/d):
  Ks1 = (1930 *(SWC1 - FC1)^(3-(1/B1))) / 1000*24
  Ks2 = (1930 *(SWC2 - FC2)^(3-(1/B2))) / 1000*24
  Ks3 = (1930 *(SWC3 - FC3)^(3-(1/B3))) / 1000*24
  Ks4 = (1930 *(SWC4 - FC4)^(3-(1/B4))) / 1000*24
  Ks5 = (1930 *(SWC5 - FC5)^(3-(1/B5))) / 1000*24
  Ks6 = (1930 *(SWC6 - FC6)^(3-(1/B6))) / 1000*24
  #write Saturated hydraulic conductivity parameters
  write.table(paste0("1=",Ks1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",Ks2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",Ks3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",Ks4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",Ks5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",Ks6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Saturation parameters
  write.table("[saturation]\nunit=m³/m³",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #from SWV values, write the table
  write.table(paste0("1=",SWC1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",SWC2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",SWC3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",SWC4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",SWC5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",SWC6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Soil PH parameters
  write.table("[pH]\nunit=pH",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #write Saturated hydraulic conductivity parameters
  write.table(paste0("1=",PH1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",PH2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",PH3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",PH4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",PH5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",PH6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Cation Exchange Capacity (CEC) parameters
  write.table("[cation_exchange_capacity]\nunit=mEq/hg",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #write CEC parameters
  write.table(paste0("1=",CEC1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",CEC2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",CEC3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",CEC4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",CEC5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",CEC6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  #create a table to write Soil Organic Matter parameters
  write.table("[organic_matter]\nunit=%",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  #calculate Soil Organic Matter for each layer
  SOM1 = ORCDRC1 / 0.58
  SOM2 = ORCDRC2 / 0.58
  SOM3 = ORCDRC3 / 0.58
  SOM4 = ORCDRC4 / 0.58
  SOM5 = ORCDRC5 / 0.58
  SOM6 = ORCDRC6 / 0.58
  #write Soil Organic Matter parameters
  write.table(paste0("1=",SOM1), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("2=",SOM2), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("3=",SOM3), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("4=",SOM4), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("5=",SOM5), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table(paste0("6=",SOM6), file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  write.table("[STATSGO]",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("water_holding_capacity_to_1m=",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("aggregated_water_holding_capacity_to_1m=",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("capability_class_irrigated=",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("capability_class_dryland=",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("agricultural_irrigated=",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("agricultural_dryland=",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  write.table("bound_by_bedrock=",file, append = TRUE, row.names=FALSE, quote=FALSE, col.names=FALSE)
  
  
  # # Calculate BD influenced by SOM as per Minasny & Hartemink, Earth-Science Reviews 106 (2011) 52-62
  # BD_by_SOM1 = 100 / (SOM1 / 0.224 + (100 - SOM1) / BLD1)
  # BD_by_SOM2 = 100 / (SOM2 / 0.224 + (100 - SOM2) / BLD2)
  # BD_by_SOM3 = 100 / (SOM3 / 0.224 + (100 - SOM3) / BLD3)
  # BD_by_SOM4 = 100 / (SOM4 / 0.224 + (100 - SOM4) / BLD4)
  # BD_by_SOM5 = 100 / (SOM5 / 0.224 + (100 - SOM5) / BLD5)
  # BD_by_SOM6 = 100 / (SOM6 / 0.224 + (100 - SOM6) / BLD6)
  
} #end of the loop
