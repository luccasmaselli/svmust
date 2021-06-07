;alterado para windows...
FUNCTION INTERFACE_LIBSVM_TRAIN_PREDICT, PtrTRAINING, ParamSTRUCT, command_train
COMMON PkgOpSolvers, PATH_OP_SOLVERS
;COMMON PkgOptimSolver, ParamOpSolver

;eps = 0.01
;Penalty = ParamSTRUCT.Penalty ;vindo do grid search...
;Shrinking = 0 
;gama_par = ParamSTRUCT.KernelParameters[1] ;vindo do grid search...
;kType = ParamSTRUCT.Kernel

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

;command_train = 'svm-train -t ' + STRTRIM(STRING(kType),1) $                ;>>> Para Windows
;command_train = './svm-train_LINUX -t ' + STRTRIM(STRING(kType),1) $           ;>>> Para LINUX
;          + ' -g ' + STRTRIM(STRING(gama_par),1) $
;          + ' -e ' + STRTRIM(STRING(eps),1) $
;          + ' -c ' + STRTRIM(STRING(Penalty),1) $
;          + ' -h ' + STRTRIM(STRING(Shrinking),1) $
;          + ' -b 1 ' $ ;para estimação na forma de probabilidades...
;          + ' TrainingLibSVM' + ' FileSV'        
SPAWN, command_train;, /HIDE

;command_pred = 'svm-predict.exe ' + ' -b 1 ' + ' predictFile ' + ' FileSV ' + ' outPrediction'             ;>>> Para Windows
;command_pred = 'svm-predict.exe ' + ' predictFile ' + ' FileSV ' + ' outPrediction'
command_pred = './svm-predict_LINUX ' + ' -b 1 ' + ' predictFile ' + ' FileSV ' + ' outPrediction'        ;>>> Para LINUX
SPAWN, command_pred;, /HIDE

dims = GET_DIMENSIONS(*PtrTRAINING[2])
DiImage = TEXT2Image_LIBSVM_PREDICT_FORMAT(PATH_OP_SOLVERS+'outPrediction', dims[1], dims[2])

Return, DiImage
END