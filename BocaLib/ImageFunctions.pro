PRO ImageFunctions
   ;...
END

;+
; Resolve future problems when line/column images are used, adding one more 
; dimension to represent a single band image. If the input image is represented 
; by a line/column matrix, the output is a image with single-band/line/column dimensions 
;
; @returns (kxMxN) matrix, k>=1
;
; @param Image {in}{required}{type=numeric matrix} image matrix representation. 
;-
FUNCTION IMAGE_INSPECT, Image

Dims = size(Image,/dimension)
IF (N_ELEMENTS(Dims) LE 2) THEN BEGIN
   Temp = FLTARR(1,Dims[0],Dims[1]) 
   Temp[0,*,*] = Image
   Return, Temp
ENDIF ELSE Return, Image


END

;####################################
FUNCTION IMAGE_3B, Image

Dims = size(Image,/dimension)
NB = Dims[0]

IF (NB EQ 3) THEN Return, Image

Temp = FLTARR(3,Dims[1],Dims[2])
IF (NB GT 3) THEN FOR i = 0, 2 DO Temp[i,*,*] = Image[i,*,*]   
IF (NB LT 3) THEN FOR i = 0, (NB-1) DO Temp[i,*,*] = Image[i,*,*]
Return, Temp
END

;####################################
FUNCTION ATTRIBUTE_VECTOR, PATH_IMG

Image = READ_TIFF(PATH_IMG)
Image = IMAGE_INSPECT(Image)
Dims = SIZE(Image, /DIMENSION)

Return, INDGEN(Dims[0])
END


;+
; Return the dimensions of a image. Avoid problems with MxN and kxMxN images, 
; when the 
;
; @returns [Bands,Column,Line] vector. If Image has MxN then Bands is zero. 
;
; @param Image {in}{required}{type=numeric matrix} a MxN or kxMxN image. 
;-
FUNCTION GET_DIMENSIONS, Image

;Dimensao dos dados
Dim = SIZE(Image, /DIMENSION)
IF N_ELEMENTS(Dim) EQ 2 THEN BEGIN
   NB = 0
   NC = Dim[0]
   NL = Dim[1]
ENDIF ELSE BEGIN
   NB = Dim[0]
   NC = Dim[1]
   NL = Dim[2]
ENDELSE

Return, [NB,NC,NL]
END