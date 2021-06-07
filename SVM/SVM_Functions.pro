PRO SVM_Functions
   ;Bloco de funcoes gerais... (Abaixo)
END


;#######################
FUNCTION BUILD_TRAINING_DATA, R1, R2, Image
;#INPUT
;R1: Lexicographic pixel positions of ROI 1
;R2: Lexicographic pixel positions of ROI 2
;Image: Image
;atts: vector that inform the attributes considered on the classification problem

;#OUTPUT
;Vector containing with pointers to X and Y vectors
;[X,Y]: X = Pointer to Data Training matrix / Y = Pointer to Class Label vector 

Dims = size(Image,/dimension)
NL = Dims[2]
NC = Dims[1]

;Build a image just with interest attributes
;TEMP = Image  &  
ImageAux = Image

X = FLTARR(Dims[0],N_ELEMENTS(R1)+N_ELEMENTS(R2))
Y = FLTARR(N_ELEMENTS(R1)+N_ELEMENTS(R2))
pos = 0L

;Reading values on ROI1 and building the training vector (X,Y)
FOR i=0L, N_ELEMENTS(R1)-1 DO BEGIN
   lin = FIX(R1[i]/NC)
   col = (R1[i] MOD NC) 
   X[*,pos] = ImageAux[*,col,lin]
   Y[pos] = +1
   pos++
ENDFOR

;Reading values on ROI2 and building the training vector (X,Y)
FOR i=0L, N_ELEMENTS(R2)-1 DO BEGIN    
   lin = FIX(R2[i]/NC)
   col = (R2[i] MOD NC) 
   X[*,pos] = ImageAux[*,col,lin]
   Y[pos] = -1
   pos++
ENDFOR

dims = GET_DIMENSIONS(ImageAux) ;>>adicionado em 12.04.18 ---> casusa problema de alocação quando a imagem é grande (aloca ela varias vezes na memoria)
Return, [PTR_NEW(X),PTR_NEW(Y),PTR_NEW(dims)]
END

;#######################
FUNCTION BUILD_FUNCTION, PtrTRAINING, Alpha, ParamSTRUCT 

X = *PtrTRAINING[0]
Y = *PtrTRAINING[1]
Image = *PtrTRAINING[2]

xSV = FLTARR(N_ELEMENTS(X[*,0]))
xMarg = FLTARR(N_ELEMENTS(X[*,0]))
ySV = 0
yMarg = 0
alphaSV = 0

Eps = DOUBLE(1.0e-6);estava e-10

FOR i = 0, N_ELEMENTS(Alpha)-1 DO BEGIN
   IF DOUBLE(Alpha[i]) GT Eps THEN BEGIN
      xSV = [[xSV],[X[*,i]]]
      ySV = [ySV,Y[i]]
      alphaSV = [[alphaSV],[Alpha[i]]]
      IF DOUBLE(Alpha[i]) LE DOUBLE(ParamSTRUCT.Penalty) THEN BEGIN
      ;IF DOUBLE(Alpha[i]) LT DOUBLE(ParamSTRUCT.Penalty) THEN BEGIN   ;por def, vetor de margem: 0<alpha<C
         xMarg = [[xMarg],[X[*,i]]]
         yMarg = [[yMarg],[Y[i]]]
      ENDIF      
   ENDIF
ENDFOR

xSV = xSV[*,1:N_ELEMENTS(xSV[0,*])-1]
ySV = ySV[1:N_ELEMENTS(ySV)-1]
xMarg = xMarg[*,1:N_ELEMENTS(xMarg[0,*])-1]
yMarg = yMarg[1:N_ELEMENTS(yMarg)-1]
alphaSV = alphaSV[1:N_ELEMENTS(alphaSV)-1]

SumT = 0
FOR i = 0, N_ELEMENTS(ySV)-1 DO BEGIN ;The last element isn't valid
   FOR j = 0, N_ELEMENTS(yMarg)-1 DO BEGIN ;The last element isn't valid
      SumT += alphaSV[i]*ySV[i]*KERNEL(xSV[*,i],xMarg[*,j]) 
   ENDFOR
ENDFOR

;Calculo do 'Bias'
b = (1.0/(N_ELEMENTS(yMarg)))*(TOTAL(yMarg) - SumT)

;Calculo do 'Weight'
W = X[*,0]*0
FOR i = 0, N_ELEMENTS(X[0,*])-1 DO W += Alpha[i]*Y[i]*X[*,i] ;testar a funcao TOTAL!
nW = NORM(W)

;Discrimination Steps...
Dims = size(Image,/dimension)
IF (N_ELEMENTS(Dims) GT 2) THEN BEGIN
   NL = Dims[2]
   NC = Dims[1]
ENDIF ELSE BEGIN
   NL = Dims[1]
   NC = Dims[0]   
ENDELSE
DiImage = FLTARR(NC,NL)
DiX = FLTARR(NC,NL)

FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
      SumK = 0
      FOR k = 0, N_ELEMENTS(ySV)-1 DO BEGIN
         SumK += alphaSV[k]*ySV[k]*KERNEL(xSV[*,k],Image[*,i,j])
      ENDFOR
      DiImage[i,j] = SumK + b
      ;DiImage[i,j] = (SumK + b)/nW ;normalização...
   ENDFOR
ENDFOR

;DiX[*,*] = ((DiImage[*,*] - MIN(DiImage[*,*]))/(MAX(DiImage[*,*]) - MIN(DiImage[*,*])))*255
;LOADCT,13
;WINDOW, 0, XSIZE = NC, YSIZE = NL, TITLE = 'Discrimination map (Dark - Light)'
;TV, DiX

Return, DiImage
END


;#######################
FUNCTION KERNEL, xi, xj
COMMON PkgPARAMETERS, ParamSTRUCT

d = ParamSTRUCT.KernelParameters[0]
gama = ParamSTRUCT.KernelParameters[1]
delta = ParamSTRUCT.KernelParameters[2]

CASE ParamSTRUCT.Kernel OF
   0:  ke = TRANSPOSE(xi)#xj ;Linear
   1:  ke = (k*(TRANSPOSE(xi)#xj)+1)^d ;Polynomial
   ;2:  ke = EXP((-1.0*((NORM(xi - xj))^2))/(FLOAT(sigma)^2)) ;RBF (Gaussiano) ;PROBLEMA! float overflow
   2:  ke = EXP(-1.0*gama*((NORM(xi - xj))^2)) ;RBF (Gaussiano) ;PROBLEMA! float overflow
   3:  ke = TANH(k*(TRANSPOSE(xi)#xj)+delta) ;Sigmoid
ENDCASE

Return, Ke
END


;################################
FUNCTION CLASS_SELECTOR, PntROIs, ParamSTRUCT

CASE ParamSTRUCT.Strategy OF
   0: BinDT = OAA(PntROIs)
   1: BinDT = OAO(PntROIs)
   2: BinDT = OVO(PntROIs) ;a estrategia 2 (OVO) eh a mesmo que OAO, so que com critérios diferentes...
   3: BinDT = OJO(PntROIs) ;a estrategia 3 (OJO) eh a mesmo que OAO, so que com critérios diferentes...
   4: BinDT = OMO(PntROIs) ;a estrategia 4 (OJO) eh a mesmo que OAO, so que com critérios diferentes... 
ENDCASE

Return, BinDT
END

;#######################
FUNCTION OAA, PntROIs

BinDT = PTRARR(2,N_ELEMENTS(PntROIs))

FOR i = 0, N_ELEMENTS(PntROIs)-1 DO BEGIN
   BinDT[0,i] = PntROIs[i]
   TempLex = 0
   TempName = ''
   TempColor = ''
   FOR j = 0, N_ELEMENTS(PntROIs)-1 DO BEGIN
      Temp = *PntROIs[j]
      IF (j NE i) THEN BEGIN
         TempName += Temp.RoiName
         TempColor = Temp.RoiColor
         TempLex = [Temp.RoiLex,[TempLex]]
      ENDIF
   ENDFOR
   Lex = LONARR(N_ELEMENTS(TempLex)-1)
   FOR k = 0L, N_ELEMENTS(TempLex)-2 DO Lex[k] = TempLex[k]
   TempStruct = {RoiName: TempName, RoiColor: TempColor, RoiLex: Lex}   
   BinDT[1,i] = PTR_NEW(TempStruct)   
ENDFOR 

Return, BinDT
END

;#######################
FUNCTION OAO, PntROIs

DIM = (N_ELEMENTS(PntROIs)*(N_ELEMENTS(PntROIs)-1))/2
BinDT = PTRARR(2,DIM)
pos = 0

FOR i = 0, N_ELEMENTS(PntROIs)-2 DO BEGIN
   FOR j = i+1, N_ELEMENTS(PntROIs)-1 DO BEGIN
      BinDT[0,pos] = PntROIs[i]
      BinDT[1,pos] = PntROIs[j]
      pos++
   ENDFOR
ENDFOR 

Return, BinDT
END

;#######################
FUNCTION OVO, PntROIs

DIM = (N_ELEMENTS(PntROIs)*(N_ELEMENTS(PntROIs)-1))/2
BinDT = PTRARR(2,DIM)
pos = 0

FOR i = 0, N_ELEMENTS(PntROIs)-2 DO BEGIN
   FOR j = i+1, N_ELEMENTS(PntROIs)-1 DO BEGIN
      BinDT[0,pos] = PntROIs[i]
      BinDT[1,pos] = PntROIs[j]
      pos++
   ENDFOR
ENDFOR 

Return, BinDT
END

;#######################
FUNCTION OJO, PntROIs

DIM = (N_ELEMENTS(PntROIs)*(N_ELEMENTS(PntROIs)-1))/2
BinDT = PTRARR(2,DIM)
pos = 0

FOR i = 0, N_ELEMENTS(PntROIs)-2 DO BEGIN
   FOR j = i+1, N_ELEMENTS(PntROIs)-1 DO BEGIN
      BinDT[0,pos] = PntROIs[i]
      BinDT[1,pos] = PntROIs[j]
      pos++
   ENDFOR
ENDFOR 

Return, BinDT
END

;#######################
FUNCTION OMO, PntROIs

DIM = (N_ELEMENTS(PntROIs)*(N_ELEMENTS(PntROIs)-1))/2
BinDT = PTRARR(2,DIM)
pos = 0

FOR i = 0, N_ELEMENTS(PntROIs)-2 DO BEGIN
   FOR j = i+1, N_ELEMENTS(PntROIs)-1 DO BEGIN
      BinDT[0,pos] = PntROIs[i]
      BinDT[1,pos] = PntROIs[j]
      pos++
   ENDFOR
ENDFOR 

Return, BinDT
END

;###############################
FUNCTION MULTICLASS_CLASSIFICATION, DiImage, ParamSTRUCT, PtrROIs

CASE ParamSTRUCT.Strategy OF
   0: ClaImage = OAA_CLASSIFICATION(DiImage,PtrROIs)
   1: ClaImage = OAO_CLASSIFICATION(DiImage,PtrROIs)
;   2: ClaImage = OVO_CLASSIFICATION(DiImage,PtrROIs)
;   3: ClaImage = OJO_CLASSIFICATION(DiImage,PtrROIs)
;   4: ClaImage = OMO_CLASSIFICATION(DiImage,PtrROIs) 
ENDCASE

Return, ClaImage
END
;##########################

FUNCTION NEW_MULTICLASS_CLASSIFICATION, DiImageLin,DiImagePol,DiImageRBF,DiImageSig, ParamSTRUCT, PtrROIs

CASE ParamSTRUCT.Strategy OF
   0: ClaImage = NEW_OAA_CLASSIFICATION(DiImageLin,DiImagePol,DiImageRBF,DiImageSig,PtrROIs)
   1: ClaImage = NEW_OAO_CLASSIFICATION(DiImageLin,DiImagePol,DiImageRBF,DiImageSig,PtrROIs)
;   2: ClaImage = OVO_CLASSIFICATION(DiImage,PtrROIs)
;   3: ClaImage = OJO_CLASSIFICATION(DiImage,PtrROIs)
;   4: ClaImage = OMO_CLASSIFICATION(DiImage,PtrROIs) 
ENDCASE

Return, ClaImage
END
;##########################

FUNCTION NEW_MULTICLASS_CLASSIFICATION_LC, DiImageLin,DiImagePol,DiImageRBF,DiImageSig, ParamSTRUCT, PtrROIs, bestAlphas

CASE ParamSTRUCT.Strategy OF
   0: ClaImage = NEW_OAA_CLASSIFICATION_LC(DiImageLin,DiImagePol,DiImageRBF,DiImageSig,PtrROIs,bestAlphas)
   1: ClaImage = NEW_OAO_CLASSIFICATION_LC(DiImageLin,DiImagePol,DiImageRBF,DiImageSig,PtrROIs,bestAlphas)
;   2: ClaImage = OVO_CLASSIFICATION(DiImage,PtrROIs)
;   3: ClaImage = OJO_CLASSIFICATION(DiImage,PtrROIs)
;   4: ClaImage = OMO_CLASSIFICATION(DiImage,PtrROIs) 
ENDCASE

Return, ClaImage
END

;##########################
FUNCTION OAA_CLASSIFICATION,DiImage,PtrROIs

Dims = size(DiImage,/dimension)
ND = Dims[0]
NC = Dims[1]
NL = Dims[2]
ClaImage = BYTARR(3,NC,NL)

FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
         Index = WHERE(DiImage[*,i,j] EQ MAX(DiImage[*,i,j]))
         Roi = *PtrROIs[Index[0]]
         ClaImage[*,i,j] = Roi.RoiColor
   ENDFOR
ENDFOR

Return, ClaImage
END

;##########################
FUNCTION OAO_CLASSIFICATION,DiImage,PtrROIs

Dims = size(DiImage,/dimension)
ND = Dims[0]
NC = Dims[1]
NL = Dims[2]
ClaNum = N_ELEMENTS(PtrROIs)

Referee = INTARR(ND)
Wins = INTARR(ClaNum) ;each position is a class, used to identify how many wins

ClaImage = BYTARR(3,NC,NL)

;The structure of DiImage[k,i,j] is known
;k is the number of discriminant functions
;(1,2);(1,3);...;(1,n);(2,3),...(2,n);...;(n-1,n): in this order!

;The number of winnings is the criterion adopted...
FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
      
      FOR k = 0, ND-1 DO BEGIN
         IF(DiImage[k,i,j] GE 0) THEN Referee[k] = +1 ELSE Referee[k] = -1 
      ENDFOR
      
      pos = 0
      Wins[*] = 0
      FOR c1 = 0, (ClaNum-2) DO BEGIN
         FOR c2 = (c1 + 1), (ClaNum-1) DO BEGIN
            IF (Referee[pos] EQ +1) THEN Wins[c1]++ ELSE Wins[c2]++ 
            pos++
         ENDFOR
      ENDFOR
      
      Index = WHERE(Wins EQ Max(Wins))
      Roi = *PtrROIs[Index[0]]
      ClaImage[*,i,j] = Roi.RoiColor
      
   ENDFOR
ENDFOR

Return, ClaImage
END
;;#########################
FUNCTION NEW_OAA_CLASSIFICATION,DiImageLin,DiImagePol,DiImageRBF,DiImageSig,PtrROIs

Dims = size(DiImageLin,/dimension)
ND = Dims[0]
NC = Dims[1]
NL = Dims[2]
ClaImage = BYTARR(3,NC,NL)

FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
         Index0 = WHERE(DiImageLin[*,i,j] EQ MAX(DiImageLin[*,i,j]))
         Index1 = WHERE(DiImagePol[*,i,j] EQ MAX(DiImagePol[*,i,j]))
         Index2 = WHERE(DiImageRBF[*,i,j] EQ MAX(DiImageRBF[*,i,j]))
         Index3 = WHERE(DiImageSig[*,i,j] EQ MAX(DiImageSig[*,i,j]))
         IndexFinal = Index0>Index1>Index2>Index3
         print, IndexFinal
         Roi = *PtrROIs[IndexFinal[0]]
         ClaImage[*,i,j] = Roi.RoiColor
   ENDFOR
ENDFOR

Return, ClaImage
END

;;##########################
FUNCTION NEW_OAO_CLASSIFICATION,DiImageLin,DiImagePol,DiImageRBF,DiImageSig,PtrROIs

Dims = size(DiImageLin,/dimension)
ND = Dims[0]
NC = Dims[1]
NL = Dims[2]
ClaNum = N_ELEMENTS(PtrROIs)

RefereeLin = INTARR(ND)
RefereePol = INTARR(ND)
RefereeRBF = INTARR(ND)
RefereeSig = INTARR(ND)

WinsLin = INTARR(ClaNum) ;each position is a class, used to identify how many wins
WinsPol = INTARR(ClaNum)
WinsRBF = INTARR(ClaNum)
WinsSig = INTARR(ClaNum)

ClaImage = BYTARR(3,NC,NL)

;The structure of DiImage[k,i,j] is known
;k is the number of discriminant functions
;(1,2);(1,3);...;(1,n);(2,3),...(2,n);...;(n-1,n): in this order!

;The number of winnings is the criterion adopted...
FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
      
      FOR k = 0, ND-1 DO BEGIN
         IF(DiImageLin[k,i,j] GE 0) THEN RefereeLin[k] = +1 ELSE RefereeLin[k] = -1
         IF(DiImagePol[k,i,j] GE 0) THEN RefereePol[k] = +1 ELSE RefereePol[k] = -1
         IF(DiImageRBF[k,i,j] GE 0) THEN RefereeRBF[k] = +1 ELSE RefereeRBF[k] = -1
         IF(DiImageSig[k,i,j] GE 0) THEN RefereeSig[k] = +1 ELSE RefereeSig[k] = -1 
      ENDFOR
      
      pos = 0
      WinsLin[*] = 0
      WinsPol[*] = 0
      WinsRBF[*] = 0
      WinsSig[*] = 0
      
      FOR c1 = 0, (ClaNum-2) DO BEGIN
         FOR c2 = (c1 + 1), (ClaNum-1) DO BEGIN
            IF (RefereeLin[pos] EQ +1) THEN WinsLin[c1]++ ELSE WinsLin[c2]++ 
            IF (RefereePol[pos] EQ +1) THEN WinsPol[c1]++ ELSE WinsPol[c2]++ 
            IF (RefereeRBF[pos] EQ +1) THEN WinsRBF[c1]++ ELSE WinsRBF[c2]++ 
            IF (RefereeSig[pos] EQ +1) THEN WinsSig[c1]++ ELSE WinsSig[c2]++ 
            pos++
         ENDFOR
      ENDFOR
      
      Index0 = WHERE(WinsLin EQ Max(WinsLin))
      Index1 = WHERE(WinsPol EQ Max(WinsPol))
      Index2 = WHERE(WinsRBF EQ Max(WinsRBF))
      Index3 = WHERE(WinsSig EQ Max(WinsSig))
      IndexFinal = Index0>Index1>Index2>Index3
      print, IndexFinal
      Roi = *PtrROIs[IndexFinal[0]]
      ClaImage[*,i,j] = Roi.RoiColor
      
   ENDFOR
ENDFOR

Return, ClaImage
END
;###########################
FUNCTION NEW_OAA_CLASSIFICATION_LC,DiImageLin,DiImagePol,DiImageRBF,DiImageSig,PtrROIs,bestAlphas

Dims = size(DiImageLin,/dimension)
ND = Dims[0]
NC = Dims[1]
NL = Dims[2]
ClaImage = BYTARR(3,NC,NL)
DiImage = bestAlphas[0]*DiImageLin + bestAlphas[1]*DiImagePol + bestAlphas[2]*DiImageRBF + bestAlphas[3]*DiImageSig

FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
         Index = WHERE(DiImage[*,i,j] EQ MAX(DiImage[*,i,j]))
         Roi = *PtrROIs[Index[0]]
         ClaImage[*,i,j] = Roi.RoiColor
   ENDFOR
ENDFOR

Return, ClaImage
END

;;##########################
FUNCTION NEW_OAO_CLASSIFICATION_LC,DiImageLin,DiImagePol,DiImageRBF,DiImageSig,PtrROIs,bestAlphas

Dims = size(DiImageLin,/dimension)
ND = Dims[0]
NC = Dims[1]
NL = Dims[2]
ClaNum = N_ELEMENTS(PtrROIs)

Referee = INTARR(ND)
Wins = INTARR(ClaNum) ;each position is a class, used to identify how many wins

ClaImage = BYTARR(3,NC,NL)
DiImage = bestAlphas[0]*DiImageLin + bestAlphas[1]*DiImagePol + bestAlphas[2]*DiImageRBF + bestAlphas[3]*DiImageSig

;The structure of DiImage[k,i,j] is known
;k is the number of discriminant functions
;(1,2);(1,3);...;(1,n);(2,3),...(2,n);...;(n-1,n): in this order!

;The number of winnings is the criterion adopted...
FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
      
      FOR k = 0, ND-1 DO BEGIN
         IF(DiImage[k,i,j] GE 0) THEN Referee[k] = +1 ELSE Referee[k] = -1 
      ENDFOR
      
      pos = 0
      Wins[*] = 0
      FOR c1 = 0, (ClaNum-2) DO BEGIN
         FOR c2 = (c1 + 1), (ClaNum-1) DO BEGIN
            IF (Referee[pos] EQ +1) THEN Wins[c1]++ ELSE Wins[c2]++ 
            pos++
         ENDFOR
      ENDFOR
      
      Index = WHERE(Wins EQ Max(Wins))
      Roi = *PtrROIs[Index[0]]
      ClaImage[*,i,j] = Roi.RoiColor
      
   ENDFOR
ENDFOR

Return, ClaImage
END
;;##########################
FUNCTION OVO_CLASSIFICATION,DiImage,PtrROIs

Dims = size(DiImage,/dimension)
ND = Dims[0] & NC = Dims[1] & NL = Dims[2]
ClaNum = N_ELEMENTS(PtrROIs)
Sum = FLTARR(ClaNum)
ClaImage = BYTARR(3,NC,NL)

FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
      
      pos = 0
      Sum[*] = 0
      FOR c1 = 0, (ClaNum-2) DO BEGIN
         FOR c2 = (c1 + 1), (ClaNum-1) DO BEGIN
            IF(DiImage[pos,i,j] GE 0) THEN Sum[c1] += DiImage[pos,i,j] $
            ELSE Sum[c2] += ABS(DiImage[pos,i,j])
            pos++
         ENDFOR
      ENDFOR      
      
      Index = WHERE(Sum EQ Max(Sum))
      Roi = *PtrROIs[Index[0]]
      ClaImage[*,i,j] = Roi.RoiColor
      
      if n_elements(index) gt 1 then stop
      
   ENDFOR
ENDFOR

Return, ClaImage
END

;;##########################
FUNCTION OJO_CLASSIFICATION,DiImage,PtrROIs

Dims = size(DiImage,/dimension)
ND = Dims[0] & NC = Dims[1] & NL = Dims[2]
ClaNum = N_ELEMENTS(PtrROIs)
Sum = FLTARR(ClaNum)
ClaImage = BYTARR(3,NC,NL)

FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
      
      pos = 0
      Sum[*] = 0
      FOR c1 = 0, (ClaNum-2) DO BEGIN
         FOR c2 = (c1 + 1), (ClaNum-1) DO BEGIN
            IF(DiImage[pos,i,j] GE 0) THEN Sum[c1] += JM_DISTANCE(DiImage[pos,i,j]) $
            ELSE Sum[c2] += JM_DISTANCE(ABS(DiImage[pos,i,j]))
            pos++
         ENDFOR
      ENDFOR      
      
      Index = WHERE(Sum EQ Max(Sum))
      Roi = *PtrROIs[Index[0]]
      ClaImage[*,i,j] = Roi.RoiColor
      
      if n_elements(index) gt 1 then stop
      
   ENDFOR
ENDFOR

Return, ClaImage
END

;;##########################
FUNCTION OMO_CLASSIFICATION,DiImage,PtrROIs

Dims = size(DiImage,/dimension)
ND = Dims[0] & NC = Dims[1] & NL = Dims[2]
ClaNum = N_ELEMENTS(PtrROIs)
Sum = FLTARR(ClaNum)
ClaImage = BYTARR(3,NC,NL)

FOR i = 0, NC-1 DO BEGIN
   FOR j = 0, NL-1 DO BEGIN
      
      pos = 0
      MaxIndex = 0
      MaxValue = -1      
      FOR c1 = 0, (ClaNum-2) DO BEGIN
         FOR c2 = (c1 + 1), (ClaNum-1) DO BEGIN
            IF(DiImage[pos,i,j] GE 0) THEN BEGIN
               IF DiImage[pos,i,j] GT MaxValue THEN BEGIN
                  MaxValue = DiImage[pos,i,j]
                  MaxIndex = c1 
               ENDIF
            ENDIF ELSE BEGIN
               IF ABS(DiImage[pos,i,j]) GT MaxValue THEN BEGIN
                  MaxValue = ABS(DiImage[pos,i,j])
                  MaxIndex = c2
               ENDIF
            ENDELSE
            pos++
         ENDFOR
      ENDFOR      
      
      Roi = *PtrROIs[MaxIndex]
      ClaImage[*,i,j] = Roi.RoiColor
      
   ENDFOR
ENDFOR

Return, ClaImage
END




;######################################
FUNCTION RESAMPLE_ROI, ptrROIs, ratio

ROI = PTRARR(N_ELEMENTS(ptrROIs))
FOR i = 0, N_ELEMENTS(ptrROIs)-1 DO BEGIN
   Aux = *ptrROIs[i]
   
   rand = RANDOMU((systime(/seconds) mod 1000)+i, N_ELEMENTS(Aux.RoiLex))
   sort_rand = SORT(rand)
   sampled = Aux.RoiLex[sort_rand[0:floor(N_ELEMENTS(sort_rand)*ratio)]]

   ROI[i] = PTR_NEW({RoiName: Aux.RoiName, RoiColor: Aux.RoiColor, RoiLex: sampled})
ENDFOR

Return, ROI
END