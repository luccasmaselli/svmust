FUNCTION INTERFACE_LIBSVM, PtrTRAINING, ParamSTRUCT
COMMON PkgOpSolvers, PATH_OP_SOLVERS
;COMMON PkgOptimSolver, ParamOpSolver

eps = 0.0001
Penalty = ParamSTRUCT.Penalty ;vindo do grid search...
Shrinking = 0
gama_par = ParamSTRUCT.KernelParameters[1] ;vindo do grid search...

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

;only linear machines!
command = './svm-train_LINUX -t 2' $ ;para ser so RBF
          + ' -g ' + STRTRIM(STRING(gama_par),1) $
          + ' -e ' + STRTRIM(STRING(eps),1) $
          + ' -c ' + STRTRIM(STRING(Penalty),1) $
          + ' -h ' + STRTRIM(STRING(Shrinking),1) $
          + ' TrainingLibSVM' + ' FileSV'
          
SPAWN, command

OpenR, File, PATH_OP_SOLVERS+'FileSV', /GET_LUN
OptAlpha = [0]
svSet = DBLARR(N_ELEMENTS(X[*,0]))

Line = ''
;Header... (junk)
FOR i = 1, 8 DO ReadF, File, Line

;Body 
WHILE ~EOF(File) DO BEGIN
   ReadF, File, Line
   xT1 = STRSPLIT(Line, ' ', /EXTRACT)
   OptAlpha = [ OptAlpha , DOUBLE(xT1[0]) ]
   
   svTemp = DBLARR(N_ELEMENTS(X[*,0]))
   FOR j = 1, N_ELEMENTS(xT1)-1 DO BEGIN
      fTemp = STRSPLIT(xT1[j],':',/EXTRACT)
      svTemp[j-1] = DOUBLE(fTemp[1])
   ENDFOR
   
   svSet = [[svSet],[svTemp]]
ENDWHILE
Close, File   &   FREE_LUN, File

Alpha =  OptAlpha[1:*]
SVs = svSet[*,1:*]

;Calculo do 'Weight'
W = X[*,0]*0
NL = N_ELEMENTS(SVs[0,*])-1
FOR i = 0L, NL DO W += Alpha[i]*SVs[*,i] ;Alpha já vem multiplicado por Y


;Modo do Webb
nbX = X[*,0]*0
nbY = 0
svY = 0
FOR i = 0L, NL DO BEGIN
   IF Alpha[i] GT 0 THEN svY = [svY , +1] ELSE svY = [svY , -1]
   IF (ABS(Alpha[i]) GT 0)AND(ABS(Alpha[i]) LT ParamSTRUCT.Penalty) THEN BEGIN
      IF Alpha[i] GT 0 THEN nbY = [nbY , +1] ELSE nbY = [nbY , -1]
      nbX = [[nbX] , [SVs[*,i]]]
   ENDIF
ENDFOR

;Check para ver se o 'Modo Webb' funciona...
IF N_ELEMENTS(nbY) GT 1 THEN BEGIN
   nbX = nbX[*,1:*]
   nbY = nbY[1:*]
   svY = svY[1:*]
   ;t1 = 1.0/N_ELEMENTS(nbY)
   
   term = 0
   FOR i = 0L, NL DO BEGIN
      FOR j = 0L, N_ELEMENTS(nbY)-1 DO term += Alpha[i]*TOTAL(SVs[*,i] * nbX[*,j])
   ENDFOR
   
   ;BIAS
   b = (1.0/N_ELEMENTS(nbY)) * (TOTAL(nbY) - term)
    
ENDIF ELSE BEGIN ;Modo Shawn-Taylor Chistianini
   ND = N_ELEMENTS(X[0,*])-1
   mini = [0]   &   maxi = mini
   FOR i = 0L, ND DO BEGIN
      IF Y[i] EQ -1 THEN maxi = [maxi, TOTAL(W*X[*,i])] $
      ELSE mini = [mini, TOTAL(W*X[*,i])]
   ENDFOR
   maxi = maxi[1:*]
   mini = mini[1:*]
   
   ;BIAS
   b = -0.5*(MAX(maxi) + MIN(mini))
ENDELSE

Return, {Weight: W, Bias: b}
END