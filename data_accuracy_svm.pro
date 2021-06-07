FUNCTION DATA_ACCURACY_SVM, PntTEST, Image

dims = GET_DIMENSIONS(Image)

labelClass = [-1]
dataClass = FLTARR(dims[0])-1
FOR i = 0L, N_ELEMENTS(PntTEST)-1 DO BEGIN
   Aux = *PntTEST[i]
   
   inds = N_ELEMENTS(Aux.RoiLex)
   FOR j = 0L, inds-1 DO BEGIN
      
      lin = LONG(Aux.RoiLex[j]/dims[1])
      col = (Aux.RoiLex[j] MOD dims[1]) 
      info = Image[*,col,lin]
      
      labelClass = [labelClass , i]
      dataClass = [[dataClass] , [info]]
   ENDFOR

ENDFOR

Return, {labelClass: labelClass[1:N_ELEMENTS(labelClass)-1], $
         dataClass: dataClass[*,1:N_ELEMENTS(dataClass[0,*])-1]}
END