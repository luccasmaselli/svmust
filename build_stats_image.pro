FUNCTION BUILD_STATS_IMAGE, Image, PtrROIs_LS_CLASS, h
    
  dims = size(Image, /dimension)
  NB = dims[0]
  NCol = dims[1]
  NLin = dims[2]
  dims_lc = [dims[1], dims[2]]
  
  NClass = N_ELEMENTS(PtrROIs_LS_CLASS)
  
  IMG_TEU = DBLARR(4*NCLass, NCol, NLin)
  
  posicoes = DBLARR(NCol*NLin)
  FOR k=0L, N_ELEMENTS(posicoes)-1 DO posicoes[k]=k
  
  Id_Image = BUILD_IMAGE(Image, dims_lc, posicoes, PtrROIs_LS_CLASS)
  
  FOR i=0, NCol-1 DO BEGIN
    FOR j=0, NLin-1 DO BEGIN
    
      bloco = GET_WIN(Id_image, i, j, NCol, NLin, h);assembles the window where the metrics will be extracted
      bloco_dims = SIZE(bloco, /dimension)
      proporcao = CALC_PROPORTIONS(bloco, NClass);calculate the proportion of each class according to the whole image
      metricas = CALC_METRICAS(bloco, bloco_dims[0], bloco_dims[1], NClass) ;define the metrics for each class
      IMG_TEU[*,i,j] = [proporcao, metricas] 
      print, i, j
    ENDFOR
  ENDFOR
  Return, IMG_TEU
END