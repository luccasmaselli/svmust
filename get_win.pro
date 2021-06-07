FUNCTION GET_WIN, Id_Image, i, j, NCol, NLin, h
  
  block = INTARR(h,h)-1
  
  countcol=0
  FOR col = i-(h/2), i+(h/2) DO BEGIN ; Coluna
    countlin = 0
    FOR lin = j-(h/2), j+(h/2) DO BEGIN ; Linha
      
      IF ((col GE 0) AND (col LT NCol) AND (lin GE 0) AND (lin LT NLin)) THEN BEGIN
        block(countcol, countlin) = Id_Image(col,lin)
        countlin ++ 
      ENDIF
      
    ENDFOR
    countcol ++
  ENDFOR
  
  pos = WHERE(block NE -1)
  
  esq_sup = MIN(pos)
  lin_min = esq_sup/h
  col_min = esq_sup mod h
  
  dir_inf = MAX(pos)
  lin_max = dir_inf/h
  col_max = dir_inf mod h
  
  block = block[col_min:col_max,lin_min:lin_max]
  
  Return, block
END