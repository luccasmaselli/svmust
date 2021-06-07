FUNCTION Class_SVM, Image, PntROIs
COMMON PkgPARAMETERS, ParamSTRUCT
COMMON PkgPOINTERS, PtrTRAINING
COMMON PkgOptimSolver, ParamOpSolver

;Define the classes used in the binary separation
;The returned value is used to build the training samples (as ROIs)
BinDT = CLASS_SELECTOR(PntROIs,ParamSTRUCT)

Dims = size(Image,/dimension)
NC = Dims[1]
NL = Dims[2]

DiImage = FLTARR(N_ELEMENTS(BinDT[0,*]),NC,NL)
HipParams = PTRARR(N_ELEMENTS(BinDT[0,*]))

ti = systime(/second)
FOR i = 0, N_ELEMENTS(BinDT[0,*])-1 DO BEGIN
   R1 = *BinDT[0,i]  &  R1 = R1.RoiLex
   R2 = *BinDT[1,i]  &  R2 = R2.RoiLex

   ;Training data set building
   PtrTRAINING = BUILD_TRAINING_DATA(R1,R2,Image,InImg)
   
   ;All Alpha values that maximizes the Dual Lagrangean problem (for a given binary problem)  
   t0 = systime(/second)
   hPar = Interface_LibSVM(PtrTRAINING, ParamSTRUCT)
   print,'Optimization time... ', systime(/second) - t0
   
   DiImage[i,*,*] = FX_IMAGE(Image,hPar)/NORM(hPar.Weight)
ENDFOR
time = systime(/second) - ti

ClaImage = MULTICLASS_CLASSIFICATION(DiImage,ParamSTRUCT.Strategy,PntROIs)
IndexImage = CLASSIF_INDEX(ClaImage,PntROIs)

PLOT_IMAGE_PREVIEW, ClaImage ;classification preview...
Print, 'END OF PROCESS...'

Return, {Decision: DiImage, Index: IndexImage, FunParams: hPar, Runtime: time, Classification: ClaImage}
END