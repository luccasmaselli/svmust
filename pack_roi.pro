;###############################################
FUNCTION PACK_ROI, Train, ROI

Lexes = [-1L]
FOR i = 0, N_ELEMENTS(Train)-1 DO Lexes = [Lexes , *Train[i]]
Packed = {RoiName: ROI.RoiName, RoiColor: ROI.RoiColor, RoiLex: Lexes[1:*]}

Return, Packed
END