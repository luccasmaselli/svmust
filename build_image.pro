FUNCTION BUILD_IMAGE, img, dims_win, posicoes, ptrrois_ik_class

    dims = SIZE(img, /dimension)
    NB = dims[0]
    NCol = dims[1]
    NLin = dims[2] 

    image = INTARR(NB,dims_win[0],dims_win[1])
    image_ids = INTARR(dims_win[0],dims_win[1])-1
    
    lin1 = posicoes[0]/NCol  &  lin2 = posicoes[N_ELEMENTS(posicoes)-1]/NCol
    col1 = posicoes[0] mod NCol  &  col2 = posicoes[N_ELEMENTS(posicoes)-1] mod NCol
    image = img[*,uint(col1):uint(col2),uint(lin1):uint(lin2)]
    
    FOR j=0L, N_ELEMENTS(ptrrois_ik_class)-1 DO BEGIN ;build image according to each class
      Aux_ik_class = *ptrrois_ik_class[j]
      INDEX = WHERE(Aux_ik_class.RoiColor[0] EQ image[0,*,*] and Aux_ik_class.RoiColor[1] EQ image[1,*,*] and Aux_ik_class.RoiColor[2] EQ image[2,*,*],count)
      IF count NE 0 THEN image_ids(INDEX) = j
    ENDFOR
    
   Return, image_ids 
END