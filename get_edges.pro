FUNCTION GET_EDGES, Segm, Free, id, pos ;segm=imagem, free=matris de posições livres, id=classe em questao, pos=posição livre em questão
   
   ;>>>esta matriz e vetor estavam como int, o que causava overflow muito facil
   vec = LONARR(5,2)
   nei_aux = LONARR(5)
   
   nei_aux(0) = [-1] ;valor padrão para nei
   Dims = GET_DIMENSIONS(Segm)
   lin = LONG(pos / Dims[1])   &   col = LONG(pos MOD Dims[1]) ;acha o número da linhas e da coluna da posição livre
   count_edge = 0
   
   ;up neighboor
   IF ((lin-1) GE 0) THEN BEGIN ;confere se nãoo é borda da imagem
      IF ((Segm[col,lin-1] EQ id) AND (Free[col,lin-1] EQ -1)) THEN BEGIN
        nei_aux(1) = [(lin-1)*Dims[1] + col]
      ENDIF ELSE BEGIN
        count_edge = count_edge + 1
      ENDELSE
   ENDIF
      ;confere se a posição superior é da mesma classe e se a posição está livre => retorna a posição lexicografica do vizinho da linha superior

   ;down neighboor
   IF ((lin+1) LT Dims[2]) THEN BEGIN
      IF ((Segm[col,lin+1] EQ id) AND (Free[col,lin+1] EQ -1)) THEN BEGIN
        nei_aux(2) = [(lin+1)*Dims[1] + col]
      ENDIF ELSE BEGIN
        count_edge = count_edge + 1
      ENDELSE
   ENDIF
   ;left neighboor
   IF ((col-1) GE 0) THEN BEGIN
      IF ((Segm[col-1,lin] EQ id) AND (Free[col-1,lin] EQ -1)) THEN BEGIN
        nei_aux(3) = [lin*Dims[1] + (col-1)]
      ENDIF ELSE BEGIN
        count_edge = count_edge + 1
      ENDELSE
   ENDIF
   
   ;right neighboor
   IF ((col+1) LT Dims[1]) THEN BEGIN
      IF ((Segm[col+1,lin] EQ id) AND (Free[col+1,lin] EQ -1)) THEN BEGIN
        nei_aux(4) = [lin*Dims[1] + (col+1)]
      ENDIF ELSE BEGIN
        count_edge = count_edge + 1
      ENDELSE
   ENDIF
   
   vec[*,0] = count_edge
   vec[*,1] = nei_aux
   Return, vec
END