@bocalib.pro
@connected_components_edge.pro
@calc_metricas.pro
@get_edges.pro
@calc_proportions.pro
@tg_estatisticas.pro
@mapper.pro
@build_image.pro
@build_image_br.pro
@group_classes.pro
@group_classes_br.pro

PRO IMAGEMETRICSCALC

   ; Classified Image Path
   PATH_CLASS_IMG = '/home/SVM/Results/SVM_PrimaryClassification_BestResult.tif'
  
   ; Primary classes ROIs Path
   PATH_CLASS_ROI = '/home/SVM/ROI/ROIs_train.txt'
   
   ; Result Path
   PATH_RESULT = '/home/ImgMetrics/Results/' 

   Image = READ_TIFF(PATH_CLASS_IMG) 

   PtrROIs_CLASS = ASCII_READ_ROI(PATH_CLASS_ROI)
   
   ; Define the neighborhood influence radius in terms of 'h' (h = 2*rho + 1)
   h = 19
   
   ; Build the image of metrics
   IMG_STATS = BUILD_STATS_IMAGE(Image, PtrROIs_CLASS, h)
   
   write_tiff, PATH_RESULT + 'h' + string(h) + '_ImageMetrics.tif', IMG_STATS, /float
   
   stop
END