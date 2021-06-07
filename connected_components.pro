FUNCTION CONNECTED_COMPONENTS, image
   
   Dim = SIZE(image, /DIMENSION)
   NC = Dim[0]
   NL = Dim[1]
       
   freePos = INTARR(NC,NL) - 1 ;initializing all elements as -1 to denote 'free positions'
   Connected = LONARR(NC,NL) ;initializing the matrix that will receive the conected components image
   
   newIdReg = 1L
   REPEAT BEGIN
      candidates = WHERE(freePos EQ -1) ;find the posisition of the candidates (where have the value -1)
      rand = (LONG(RANDOMU(systime(/seconds))*10000000L)) MOD N_ELEMENTS(candidates) ;draw a position
      seedPos = candidates[rand] ;select a random free position (-1)
      
      targetID = image[seedPos] ;select the class value of the image corresponding to the random free position
      list = [seedPos , -999L] ;keep the random free position as a very negative number
      
      WHILE (N_ELEMENTS(list) GT 1) DO BEGIN
         choose = list[0] ;choose the random free position
         Connected[choose] = newIdReg ;a matrix of conected components, in the random position, receive its index
         freePos[choose] = 255 ;matrix of free positions receive 255 (indicates that it is not available)
         list = list[1:*] ;extrai o primeiro termo do vetor list
      
         neighs = GET_NEIGHS_CROSS(image, freePos, targetID, choose) ;returns the free positions that have the same class of the interest target
          
         
         
          IF (N_ELEMENTS(neighs) NE 1) THEN BEGIN ;check if exists neighbors of same class
            temp = [neighs[1:*] , list] 
            temp = temp[0:N_ELEMENTS(temp)-2] 
            sortVec = temp[sort(temp)]
            list = [sortVec[uniq(sortVec)] , -999L] 
         ENDIF   
      ENDWHILE
      
      newIdReg++
      flag = WHERE(freePos EQ -1)   
   ENDREP UNTIL (flag[0] EQ -1)
   
   Return, Connected
   END