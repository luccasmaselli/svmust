FUNCTION GET_NEIGHS_CROSS, Segm, Free, id, pos

   nei = [-1] 
   Dims = GET_DIMENSIONS(Segm) 
   lin = LONG(pos / Dims[1])   &   col = LONG(pos MOD Dims[1]) 
   
   ;up neighboor
   IF ((lin-1) GE 0) THEN $ ;confere se nãoo é borda da imagem
      IF ((Segm[col,lin-1] EQ id) AND (Free[col,lin-1] EQ -1)) THEN nei = [nei , (lin-1)*Dims[1] + col]
  

   ;down neighboor
   IF ((lin+1) LT Dims[2]) THEN $
      IF ((Segm[col,lin+1] EQ id) AND (Free[col,lin+1] EQ -1)) THEN nei = [nei , (lin+1)*Dims[1] + col]
   
   ;left neighboor
   IF ((col-1) GE 0) THEN $
      IF ((Segm[col-1,lin] EQ id) AND (Free[col-1,lin] EQ -1)) THEN nei = [nei , lin*Dims[1] + (col-1)]

   ;right neighboor
   IF ((col+1) LT Dims[1]) THEN $
      IF ((Segm[col+1,lin] EQ id) AND (Free[col+1,lin] EQ -1)) THEN nei = [nei , lin*Dims[1] + (col+1)]

   Return, nei
END