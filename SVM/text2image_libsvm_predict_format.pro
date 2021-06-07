FUNCTION TEXT2Image_LIBSVM_PREDICT_FORMAT, Path_Prediction, numCol, numLines

OpenR, Arq, Path_Prediction, /GET_LUN

imgPred = FLTARR(numCol, numLines)

Head = ''
ReadF, Arq, Head
FOR i = 0, numCol-1 DO BEGIN
   FOR j = 0, numLines-1 DO BEGIN
      ReadF, Arq, lab, yp, ym
      ;imgPred[i,j] = yp 
      imgPred[i,j] = yp - 0.5D     
   ENDFOR
ENDFOR

Close, Arq
FREE_LUN, Arq

Return, imgPred
END