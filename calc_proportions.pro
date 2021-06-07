FUNCTION CALC_PROPORTIONS, Image, NClass
   
   Contador = DBLARR(NClass)
   
   FOR i=0L, NClass-1 DO BEGIN
    pos = WHERE(Image EQ i, count)
    Contador[i] = count
   ENDFOR
   
   Return, Contador/TOTAL(Contador)

END