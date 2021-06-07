FUNCTION CALC_METRICAS, image, NCol, NLin, NClass
   
  teu_comp_conect = LONARR(NCol, NLin)
  
  Estats = DBLARR(NClass,3)
  
  sum_vec = FLTARR(NClass)
  count = FLTARR(NClass)
  media = FLTARR(NClass)
  dividendo_vec = FLTARR(NClass)

   comp_conect_img = CONNECTED_COMPONENTS(image)
   
   sort_vec = comp_conect_img[UNIQ(comp_conect_img, SORT(comp_conect_img))] ;order the conected components index
   
   FOR j=0L, N_ELEMENTS(sort_vec)-1 DO BEGIN ;calc the "mean" variables
    pos_id = WHERE(comp_conect_img eq sort_vec[j]) ;find the positions in the conectes components 
    sum_vec[image(pos_id(0))] += N_ELEMENTS(pos_id) ;count the pixels for each class
    count[image(pos_id(0))] = count[image(pos_id(0))] + 1 ;count the number of spots of each class
   ENDFOR
   
   INDEX = WHERE(count EQ 0, contador)
   IF contador NE 0 THEN count(INDEX) = 1 ;avoids division of 0/0
   
   media = sum_vec/count ;calc the mean size of the spots
   
   FOR k=0L, N_ELEMENTS(sort_vec)-1 DO BEGIN ;calc the "Standard Deviation" variables
    pos_id = WHERE(comp_conect_img eq sort_vec[k]) 
    dividendo_vec[image(pos_id(0))] += ((N_ELEMENTS(pos_id) - media(image(pos_id(0))))^2)
   ENDFOR
   
   desv_pad = SQRT(dividendo_vec/count)
   
   INDEX = WHERE(media EQ 0, contador) 
   IF contador NE 0 THEN media(INDEX) = 1 
   
   Coef_var = desv_pad/media
   IF MAX(Coef_var)-MIN(Coef_var) EQ 0 THEN DIV = 1 ELSE DIV = MAX(Coef_var)-MIN(Coef_var) 
    Coef_var = (Coef_var-MIN(Coef_var))/DIV
   
   
   Dens_manchas = count/DOUBLE(NCol*NLin) ;area of the image
   IF MAX(Dens_manchas)-MIN(Dens_manchas) EQ 0 THEN DIV = 1 ELSE DIV = MAX(Dens_manchas)-MIN(Dens_manchas)
    Dens_manchas = (Dens_manchas-MIN(Dens_manchas))/DIV
    
   Edge_lenghts = CONNECTED_COMPONENTS_EDGE(image, NCLass)
   
   Dens_bordas = Edge_lenghts/DOUBLE(NCol*NLin)
   IF MAX(Dens_bordas)-MIN(Dens_bordas) EQ 0 THEN DIV = 1 ELSE DIV = MAX(Dens_bordas)-MIN(Dens_bordas)
    Dens_bordas = (Dens_bordas-MIN(Dens_bordas))/DIV   

  Return, [Coef_var, Dens_manchas, Dens_bordas]
END