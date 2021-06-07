;alterado para windows...
FUNCTION INTERFACE_LIBSVM_GRID_SEARCH, PtrTRAINING, ParamSTRUCT, DataPred, command_train
COMMON PkgOpSolvers, PATH_OP_SOLVERS
;COMMON PkgOptimSolver, ParamOpSolver

;eps = 0.01
;Penalty = ParamSTRUCT.Penalty ;vindo do grid search...
;Shrinking = 0
;gama_par = ParamSTRUCT.KernelParameters[1] ;vindo do grid search...

;transforma no formato de entrada adequado
OpenW, File, PATH_OP_SOLVERS+'TrainingLibSVM', /GET_LUN
X = *PtrTRAINING[0]
Y = *PtrTRAINING[1]
n = N_ELEMENTS(Y)-1
d = N_ELEMENTS(X[*,0])-1

FOR i = 0L, n DO BEGIN
   Line = STRTRIM(STRING(FIX(Y[i])),1)
   FOR j = 0L, d DO BEGIN
      Att = ' '+STRTRIM(STRING(j+1),1) + ':'+STRTRIM(STRING(X[j,i], FORMAT='(F30.20)'),1)
      Line += Att   
   ENDFOR
   PrintF, File, Line
ENDFOR
Close, File   &   FREE_LUN, File


CD, PATH_OP_SOLVERS

;command_train = 'svm-train.exe -t 2' $ ;para ser so RBF
;command_train = './svm-train_LINUX -t 2' $ ;para ser so RBF
;          + ' -g ' + STRTRIM(STRING(gama_par),1) $
;          + ' -e ' + STRTRIM(STRING(eps),1) $
;          + ' -c ' + STRTRIM(STRING(Penalty),1) $
;          + ' -h ' + STRTRIM(STRING(Shrinking),1) $
;          + ' -b 1 ' $ ;para estimação na forma de probabilidades...
;          + ' TrainingLibSVM' + ' FileSV'         
;SPAWN, command_train, /HIDE
SPAWN, command_train

IMAGE2TEXT_LIBSVM_PREDICT_VETOR, DataPred

;command_pred = 'svm-predict.exe ' + ' -b 1 ' + ' predictFile_GS ' + ' FileSV ' + ' outPrediction' 
;command_pred = 'svm-predict.exe ' + ' predictFile_GS ' + ' FileSV ' + ' outPrediction' 
command_pred = './svm-predict_LINUX ' + ' -b 1 ' + ' predictFile_GS ' + ' FileSV ' + ' outPrediction'
;SPAWN, command_pred, /HIDE
SPAWN, command_pred

DiImage = TEXT2VETOR_LIBSVM_PREDICT(PATH_OP_SOLVERS+'outPrediction')

Return, DiImage
END


;############################################
PRO IMAGE2TEXT_LIBSVM_PREDICT_VETOR, DataPred
COMMON PkgOpSolvers, PATH_OP_SOLVERS

OpenW, Arq, PATH_OP_SOLVERS+'predictFile_GS', /GET_LUN

atts = N_ELEMENTS(DataPred[*,0])
itens = N_ELEMENTS(DataPred[0,*])

FOR i = 0, itens-1 DO BEGIN
   line = '+1 '
   FOR j = 0, atts-1 DO BEGIN
      ind = STRTRIM(STRING(j+1),2)
      val = STRTRIM(STRING(DataPred[j,i]),2)
      line += ind+':'+val+' '
   ENDFOR
   PrintF,Arq,line
ENDFOR
Close, Arq
FREE_LUN, Arq

END


;####################################
FUNCTION TEXT2VETOR_LIBSVM_PREDICT, Path_Prediction

OpenR, Arq, Path_Prediction, /GET_LUN

Pred = [-1]
Head = ''
ReadF, Arq, Head
WHILE ~EOF(Arq) DO BEGIN
   ReadF, Arq, lab, yp, ym
   Pred = [Pred , yp - 0.5D]
ENDWHILE
Close, Arq
FREE_LUN, Arq

Return, Pred[1:N_ELEMENTS(Pred)-1]
END