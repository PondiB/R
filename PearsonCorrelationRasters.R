library('raster', 'rgdal')  
setwd('C:/Users/jymutua/Dropbox/LDN_new')

list.files(pattern='tif')
layers <- list.files('.', pattern = '.tif$', full.names = TRUE)

r1=raster('otjo_bd1.tif')
r2=raster('otjo_bd2.tif')
r3=raster('otjo_bd3.tif')
r4=raster('ens_bd1.tif')
r5=raster('ens_ocs1.tif')
r6=raster('ens_soc1.tif')
r7=raster('ens_soc2.tif')

r4<-resample(r4, r1, method='nearest')
r5<-resample(r5, r1, method='bilinear')
r6<-resample(r6, r1, method='bilinear')
r7<-resample(r7, r1, method='bilinear')

r.stack = stack(r1,r2,r3,r4,r5,r6,r7)

jnk=layerStats(r.stack, 'pearson', na.rm=T)
corr_matrix=jnk$'pearson correlation coefficient'
corr_matrix

write.csv(corr_matrix, file = 'corr_matrix.csv')
