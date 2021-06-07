@bocalib.pro

@compute_parameters.pro
@miscs.pro

@SVM_Functions.pro

@svm_pixel_wise.pro

;@INTERFACE_LIBSVM_TRAIN_PREDICT.pro
@text2image_libsvm_predict_format.pro
@image2text_libsvm_predict_format.pro

@data_accuracy_svm.pro

@interface_libsvm_alpha.pro
@build_function_pw.pro
@interface_libsvm_grid_search.pro

@classification_report.pro

@auto_gs_svm_pw.pro

@grow.pro
@pack_roi.pro
@create_command.pro

@interface_libsvm_train_predict__complete.pro


PRO SVM_UST
COMMON PkgOpSolvers, PATH_OP_SOLVERS
  
  PATH_STATS_IMG = '/home/SVM/imgs/RemoteSensing_imagery.tif'
  PATH_ROI_TREINO = '/home/SVM/ROI/ROIs_train.txt'
  PATH_ROI_TESTE = '/home/SVM/ROI/ROIs_test.txt'
  PATH_RESULTS = '/home/SVM/Results/'
  
  stats_img = READ_TIFF(PATH_STATS_IMG)

  dims = GET_DIMENSIONS(stats_img)
  NCol = dims[1]
  NLin = dims[2]
  NB = dims[0]
  
  Attributes = INTARR(NB)
  
  grauPol = [2, 3, 4, 5] & Ngrau = N_ELEMENTS(grauPol)
  gamaPar = [0.05, 0.1, 0.25, 0.5, 1.0, 1,5, 2.0, 3.0] &  Ngama = N_ELEMENTS(gamaPar)
  penaltyPar = [1 , 10 , 100 , 1000, 10000] & Npenalty = N_ELEMENTS(penaltyPar)
  
  posTab = 0L
  KappasList = DBLARR(4,Npenalty*(1+Ngama+Ngrau)*3)  ;*3 to encompass the kernel functions
  
  FOR k=0l, 2 DO BEGIN ;Kernel loop (0=Linear, 1=Polynomial, 2=RBF)
    
    ;PARAMETERS#################################

    Attributes = INDGEN(NB)
    
    Parameters_SVM = {KernelParameters: [0.0,0.0,0.0], Penalty: 0, Kernel: k, Strategy: 0}
    
    PATH_OP_SOLVERS = '/home/SVM/OptSolver/svm_solver/'
    ;###########################################
    
    
    IF Parameters_SVM.Kernel EQ 0 THEN BEGIN
    loop = 1
    parametro = 1
    ENDIF
    IF Parameters_SVM.Kernel EQ 1 THEN BEGIN
    loop = Ngrau
    parametro = grauPol
    ENDIF
    IF Parameters_SVM.Kernel EQ 2 THEN BEGIN
    loop = Ngama
    parametro = gamaPar
    ENDIF
    
    FOR i=0L, loop-1 DO BEGIN ;kernel parameter loop
      FOR j=0L, Npenalty-1 DO BEGIN ;penalty loop
        
        IF k EQ 0 THEN BEGIN
        parameters_SVM.KernelParameters[0] = parametro
        ENDIF ELSE BEGIN
        parameters_SVM.KernelParameters[0] = parametro[i]
        ENDELSE
        
        parameters_SVM.Penalty[0] = penaltyPar[j]
        lin = MIN(WHERE(KappasList[1,*] EQ 0))
        
        PtrROItreino = ASCII_READ_ROI(PATH_ROI_TREINO)
        PtrROIteste = ASCII_READ_ROI(PATH_ROI_TESTE)
        StructRes = SVM_PIXEL_WISE(PATH_STATS_IMG, PtrROItreino, PtrROIteste, Attributes, Parameters_SVM)
        KappasList[*,posTab] = [StructRes.AccuracyMeasures[3],parameters_SVM.Kernel, $
              parameters_SVM.KernelParameters[0],parameters_SVM.Penalty[0]]
        posTab++
        
       HEAP_GC, /ptr
       ENDFOR    
    ENDFOR     
  ENDFOR

  BestPar = WHERE(KappasList[0,*] EQ MAX(KappasList[0,*]))
  Parameters_SVM.Kernel = KappasList[1,BestPar]
  Parameters_SVM.KernelParameters[0] = KappasList[2,BestPar]
  Parameters_SVM.Penalty[0] = KappasList[3,BestPar]
  BestClass = SVM_PIXEL_WISE(PATH_STATS_IMG, PtrROItreino, PtrROIteste, Attributes, Parameters_SVM)
  
   Classification_report, PATH_RESULTS+'SVM_Report_' + string(Parameters_SVM.Kernel) + '_' + string(Parameters_SVM.KernelParameters[0])+ '_' + string(Parameters_SVM.Penalty) + '.txt', BestClass.ConfusionMatrix, BestClass.AccuracyMeasures, PtrROItreino, PtrROIteste, Parameters_SVM
   write_tiff, PATH_RESULTS + 'SVM_Classification_' + string(Parameters_SVM.Kernel) + '_' + string(Parameters_SVM.KernelParameters[0]) + '_' + string(Parameters_SVM.Penalty) + '.tif', BestClass.ColorClassification
END