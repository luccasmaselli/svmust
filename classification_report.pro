PRO CLASSIFICATION_REPORT, REPORT_PATH, ConfusionMatrix, Measures, PntROI, TestROI, Parameters_SVM
    
   ;Formated confusion matrix
   StringCM = CONFUSION_MATRIX_FORMATER(ConfusionMatrix, PntROI, TestROI)
   
   OpenW, Arq, REPORT_PATH, /APPEND, /GET_LUN
   
   PrintF, Arq, 'Kernel Type: ', Parameters_SVM.Kernel, '   Strategy: ', Parameters_SVM.Strategy
   PrintF, Arq, 'Best Kernel Parameter: ', Parameters_SVM.KernelParameters,'   Best Penalty:', Parameters_SVM.Penalty
   ;PrintF, Arq, 'Best Alphas: ', bestAlphas
   ;PrintF, Arq, 'Run GRID SEARCH time:', timeGS_SVM , 'seconds'
   ;PrintF, Arq, 'Run SVM time:', Time , 'seconds'
   PrintF, Arq, 'Class. Measures (OA, Tau, V(Tau), Kappa, V(Kappa)): '
   PrintF, Arq, STRTRIM(STRING(Measures),1)
   PrintF, Arq, 'Confusion Matrix'
   PrintF, Arq, ''
   FOR i = 0, N_ELEMENTS(StringCM)-1 DO PrintF, Arq, StringCM[i]
   PrintF, Arq, ''
   PrintF, Arq, ''
   Close, Arq
   Free_lun, Arq
END


;####################################
FUNCTION CONFUSION_MATRIX_FORMATER, ConfusionMatrix, PntROI, TestROI

TrainNames = STRARR(N_ELEMENTS(PntROI)) & TestNames = TrainNames
maxlen = 7 ;because 'Cla/Ref' string size...
FOR i = 0, N_ELEMENTS(PntROI)-1 DO BEGIN
   Aux1 = *PntROI[i]
   Aux2 = *TestROI[i]
   TrainNames[i] = Aux1.RoiName
   TestNames[i] = Aux2.RoiName
   
   IF STRLEN(TrainNames[i]) GT maxlen THEN maxlen = STRLEN(TrainNames[i])
   IF STRLEN(TestNames[i]) GT maxlen THEN maxlen = STRLEN(TestNames[i])
ENDFOR

FOR i = 0, N_ELEMENTS(ConfusionMatrix)-1 DO BEGIN
   IF STRLEN(ConfusionMatrix[i]) GT maxlen THEN maxlen = STRLEN(ConfusionMatrix[i])
ENDFOR

maxlen++

;The Confusion Matrix header
StringCM = PUT_TAIL('Cla\Ref', maxlen) + ' '
FOR i = 0, N_ELEMENTS(PntROI)-1 DO StringCM += PUT_TAIL(TestNames[i], maxlen) + ' ' 

;...and its body
 FOR i = 0, N_ELEMENTS(PntROI)-1 DO BEGIN
   Line = ''
   Line = PUT_TAIL(TrainNames[i], maxlen) + ' '
   FOR j = 0, N_ELEMENTS(PntROI)-1 DO $
      Line += PUT_TAIL(STRTRIM(STRING(ConfusionMatrix[j,i]),1), maxlen) + ' '
   StringCM = [StringCM , Line]
ENDFOR

Return, StringCM
END


;####################################
FUNCTION PUT_TAIL, Str, Len
TailLength = Len - STRLEN(Str)
Tail = ''
FOR i = 1, TailLength DO Tail += ' '
Return, Tail + Str
END