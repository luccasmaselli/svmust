FUNCTION SVM_PIXEL_WISE, PATH_IMG, PntROIs, TestROIs, Attributes, InterfaceParameters

;general parameters
ParameStruct = InterfaceParameters 

Image = READ_TIFF(PATH_IMG)
Image = IMAGE_INSPECT(Image) ;Check image dimensions
Image = FLOAT(Image[Attributes,*,*]) ;Set the attributes

FOR i = 0, N_ELEMENTS(Image[*,0,0])-1 DO BEGIN
   IF MIN(Image[i,*,*]) LT 0 THEN $
      Image[i,*,*] = (Image[i,*,*] + ABS(MIN(Image[i,*,*])))/MAX(Image[i,*,*] + ABS(MIN(Image[i,*,*]))) $
   ELSE $ 
      Image[i,*,*] = (Image[i,*,*] - ABS(MIN(Image[i,*,*])))/MAX(Image[i,*,*] - ABS(MIN(Image[i,*,*])))
ENDFOR

Dims = size(Image,/dimension)   &   NC = Dims[1]   &   NL = Dims[2]

IMAGE2TEXT_LIBSVM_PREDICT_FORMAT, Image ;create LibSVM  image format

ROIs = RESAMPLE_ROI(PntROIs, 1.0) ;sampling of the total pixel amount

;Define the classes used in the binary separation
;The returned value is used to build the training samples (as ROIs)

BinDT = CLASS_SELECTOR(ROIs,ParameSTRUCT) ;responsible for organizing the training data according to the multiclass strategy

DiImage = FLTARR(N_ELEMENTS(BinDT[0,*]),NC,NL)
HipParams = PTRARR(N_ELEMENTS(BinDT[0,*]))

ti = systime(/second)
FOR i = 0, N_ELEMENTS(BinDT[0,*])-1 DO BEGIN
   R1 = *BinDT[0,i]  &  R1 = R1.RoiLex
   R2 = *BinDT[1,i]  &  R2 = R2.RoiLex

   ;Training data set building
   PtrTRAINING = BUILD_TRAINING_DATA(R1,R2,Image)
   
   ;libsvm documentation: https://www.csie.ntu.edu.tw/~cjlin/libsvm/  
   DiImage[i,*,*] = INTERFACE_LIBSVM_TRAIN_PREDICT__COMPLETE(PtrTRAINING, ParameSTRUCT)
ENDFOR

ClaImage = MULTICLASS_CLASSIFICATION(DiImage,ParameSTRUCT,PntROIs)
IndexImage = CLASSIF_INDEX(ClaImage,PntROIs)

ConfMatrix = CONFUSION_MATRIX(IndexImage, TestROIs)
Measures = CONCORDANCE_MEASURES(ConfMatrix)

Return, {ClassificationIndex: IndexImage, ColorClassification: ClaImage, $
         ConfusionMatrix:ConfMatrix, AccuracyMeasures: Measures}
   
END

;###############################################################