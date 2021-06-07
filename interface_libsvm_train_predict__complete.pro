FUNCTION INTERFACE_LIBSVM_TRAIN_PREDICT__COMPLETE, PtrTRAINING, ParamSTRUCT
COMMON PkgOpSolvers, PATH_OP_SOLVERS

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

command_train = BUILD_COMMAND_TRAIN(ParamSTRUCT)
 
SPAWN, command_train;, /HIDE

command_pred = './svm-predict_LINUX ' + ' -b 1 ' + ' predictFile ' + ' FileSV ' + ' outPrediction' 
SPAWN, command_pred;, /HIDE

;dims = GET_DIMENSIONS(*PtrTRAINING[2])
dims = *PtrTRAINING[2]
DiImage = TEXT2Image_LIBSVM_PREDICT_FORMAT(PATH_OP_SOLVERS+'outPrediction', dims[1], dims[2])


if ParamSTRUCT.strategy eq 1 then return, diimage[*] - 0.5   ;<<< adicionado em 14.06.18
Return, DiImage
END




;#################################################
 FUNCTION BUILD_COMMAND_TRAIN, Params
 
eps = 0.001
Shrinking = 0
command_train = './svm-train_LINUX'

CASE Params.Kernel OF
   ;Linear Kernel
   0: command_train += ' -t 0 '  
   
   ;Polynomial Kernel
   1: command_train += ' -t 1 ' + '-d ' + STRTRIM(STRING(Params.KernelParameters[0]),1) $
                                + ' -r ' + STRTRIM(STRING(Params.KernelParameters[2]),1)

   ;RBF Kernel
   2: command_train += ' -t 2 ' + '-g ' + STRTRIM(STRING(Params.KernelParameters[1]),1)

   ;Sigmoid Kernel
   3: command_train += ' -t 3 ' + '-g ' + STRTRIM(STRING(Params.KernelParameters[1]),1) $
                                        + ' -r ' + STRTRIM(STRING(Params.KernelParameters[2]),1)                                      
ENDCASE

command_train += ' -e ' + STRTRIM(STRING(eps),1) $
               + ' -c ' + STRTRIM(STRING(Params.Penalty),1) $
               + ' -h ' + STRTRIM(STRING(FIX(Shrinking)),1) $
               + ' -b 1 ' $ ;para estimação na forma de probabilidades...
               + ' TrainingLibSVM' + ' FileSV' 
 
 
Return, command_train 
END
