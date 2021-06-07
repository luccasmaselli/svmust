FUNCTION CONNECTED_COMPONENTS_EDGE, image, nclass
   
   Dim = SIZE(image, /DIMENSION)
   NC = Dim[0]
   NL = Dim[1]
       
   freePos = INTARR(NC,NL) - 1 
   Connected = LONARR(NC,NL) 
   Count_edge = LONARR(nclass)
   
   newIdReg = 1L
   REPEAT BEGIN
      candidates = WHERE(freePos EQ -1)
      rand = (LONG(RANDOMU(systime(/seconds))*10000000L)) MOD N_ELEMENTS(candidates)  
      seedPosi = candidates[rand]
      
      targetID = image[seedPosi] 
      lista = [seedPosi , -999L]
     
      WHILE (N_ELEMENTS(lista) GT 1) DO BEGIN
         choose = lista[0] 
         Connected[choose] = newIdReg 
         freePos[choose] = 255 
         lista = lista[1:*] 
      
         count_bordas = GET_EDGES(image, freePos, targetID, choose)
          
         Count_edge[targetID] = Count_edge[targetID] + count_bordas[0,0] ;count the neighbors of different classes
         
         posicoes = WHERE(count_bordas[*,1] ne 0)
         
         neighs = count_bordas[posicoes,1]
         
          IF (N_ELEMENTS(neighs) NE 1) THEN BEGIN 
            temp = [neighs[1:*] , lista] 
            temp = temp[0:N_ELEMENTS(temp)-2]
            sortVec = temp[sort(temp)] 
            lista = [sortVec[uniq(sortVec)] , -999L]
         ENDIF   
      ENDWHILE
      
      newIdReg++
      flag = WHERE(freePos EQ -1)   
   ENDREP UNTIL (flag[0] EQ -1)
   
   Return, Count_edge
   END