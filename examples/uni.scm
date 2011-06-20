; Define the function of the script and list its parameters
; The parameters will be matched with the parameters listed
; below in script-fu-register.

(define (uni-img size color)
  ; Create an img and a layer
  (set! img (car (gimp-image-new size size RGB)))
  (set! layer (car (gimp-layer-new img size size
                                   RGB "layer 1" 100 NORMAL)))

 ; The following is done for all scripts
 (gimp-image-undo-disable img)
 (gimp-image-add-layer img layer 0)

 ; Here is where the painting starts. We now have an image
 ; and layer and may paint in the layer through the PDB functions.
 (gimp-palette-set-background color)
 (gimp-edit-fill layer BG-IMAGE-FILL)

 ; The following is also done for all script
 (gimp-display-new img)
 (gimp-image-undo-enable img))

; Finally register our script with script-fu.
(script-fu-register "uni-img"
                    "Uniform image"
                    "Creates a uniform image"
                    "Dov Grobgeld <dov@imagic.weizmann.ac.il>"
                    "Dov Grobgeld"
                    "2002-02-12"
                    ""
                    SF-VALUE "size" "100"
                    SF-COLOR "color" '(255 127 0))
(script-fu-menu-register "uni-img" "<Toolbox>/Xtns/Script-Fu/Tutorials")