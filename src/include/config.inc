; The four colors used for palettes
; BGP colors
COLOR_WHITE = RGB(31,31,31)
COLOR_LIGHT_GRAY = RGB(20,20,20)
COLOR_DARK_GRAY = RGB(10,10,10)
COLOR_BLACK = RGB(0,0,0)
; OBP0 colors
COLOR_WHITE_OBP0 = RGB(31,0,31)
COLOR_LIGHT_GRAY_OBP0 = RGB(20,0,20)
COLOR_DARK_GRAY_OBP0 = RGB(10,0,10)    
COLOR_BLACK_OBP0 = RGB(0,0,0)   
; OBP1 colors
COLOR_WHITE_OBP1 = RGB(31,31,0)
COLOR_LIGHT_GRAY_OBP1 = RGB(20,20,0)
COLOR_DARK_GRAY_OBP1 = RGB(10,10,0)
COLOR_BLACK_OBP1 = RGB(0,0,0)

; OAMDMA Location
OAMDMA_ADDR = $FF94 ; this is used for a hack to avoid emualting RAM code
SHADOWOAM_ADDR = $C000 ; this is the location of the shadow OAM that gets DMA'd

; CPU Emulation Optimizations
CONFIG_LDD_CAN_WRITE_IO = 1
CONFIG_LD_HL_A_CAN_WRITE_IO = 1
CONFIG_SLA_R_CAN_SET_Z = 1