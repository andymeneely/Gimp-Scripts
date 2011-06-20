; GIMP Layer Effects
; Copyright (c) 2011 Andy Meneely
; me@andymeneely.com

; ---------------------------------------------------------------------

; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(define (math-round input)
  (floor (+ input 0.5))
)

(define (math-ceil input)
  (if (= input (floor input))
    input
    (+ (floor input) 1)
  )
)

(define (get-layer-pos img layer)
  (let* ((layerdata (gimp-image-get-layers img))
	 (numlayers (car layerdata))
	 (layerarray (cadr layerdata))
	 (i 0)
	 (pos -1)
	)
    (while (< i numlayers)
      (if (= layer (vector-ref layerarray i))
	(begin
	  (set! pos i)
	  (set! i numlayers)
	)
	(set! i (+ i 1))
      )
    )
    pos
  )
)

(define (add-under-layer img newlayer oldlayer)
  (gimp-image-add-layer img newlayer (+ (get-layer-pos img oldlayer) 1))
)

(define (add-over-layer img newlayer oldlayer)
  (gimp-image-add-layer img newlayer (get-layer-pos img oldlayer))
)

(define (draw-blurshape img drawable size initgrowth sel invert)
  (let* ((k initgrowth)
	 (currshade 0)
	 (i 0))
    (while (< i size)
      (if (> k 0)
	(gimp-selection-grow img k)
	(if (< k 0)
	  (gimp-selection-shrink img (abs k))
	)
      )
      (if (= invert 1)
	(set! currshade (math-round (* (/ (- size (+ i 1)) size) 255)))
	(set! currshade (math-round (* (/ (+ i 1) size) 255)))
      )
      (gimp-palette-set-foreground (list currshade currshade currshade))
      (if (= (car (gimp-selection-is-empty img)) 0)
	(gimp-edit-fill drawable 0)
      )
      (gimp-selection-load sel)
      (set! k (- k 1))
      (set! i (+ i 1))
    )
  )
)


(define (script-fu-chaoticbits-border-shadow img
				  origlayer
				  color
				  width
				  iterations)
  (gimp-image-undo-group-start img)
   
  ;TODO Make sure they have an active layer first
  
  
  ;Back up original stuff going in
  (let* (
	  	 (origfgcolor (car (gimp-palette-get-foreground)))
		 (origselection (car (gimp-selection-save img)))
		 (layername (car (gimp-drawable-get-name origlayer)))
		 (imgheight (car (gimp-drawable-height origlayer)))
		 (imgwidth (car (gimp-drawable-width origlayer)))	
  		 (newlayer (car (gimp-layer-new img imgwidth imgheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername " border shadow") 100 0)))
  		 (blurperiter (/ width iterations))
  		 (blur_width width)
  		 (blur_iter 1)
	    )
		;Add the new layer above the current
  		(gimp-image-add-layer img newlayer -1) 

		;Set color
  		(gimp-palette-set-foreground color) 

		;Loop over the iterations
		(while (< blur_iter (+ iterations 1)) 
			;Select from original layer
	  		(gimp-selection-layer-alpha origlayer) 
	  		(gimp-selection-invert img)
	  
			;Fill inverted layer
			(gimp-edit-bucket-fill newlayer FG-BUCKET-FILL NORMAL-MODE 100 255 FALSE 0 0)
	  
			;Blur
			(gimp-selection-none img) 
	 		(plug-in-gauss-rle2 1 img newlayer blur_width blur_width) 
	 		
	 		;Iterate
	 		(set! blur_iter (+ blur_iter 1))
	 		(set! blur_width (* blurperiter blur_iter ))
		)  		
 
 		;Delete from original layer
		(gimp-selection-layer-alpha origlayer)
		(gimp-selection-invert img)
		(gimp-edit-clear newlayer)
		
		;Restore original settings
		(gimp-palette-set-foreground origfgcolor)
		(gimp-selection-load origselection)
		(gimp-image-set-active-layer img origlayer)
  )
    
  (gimp-displays-flush)
  (gimp-image-undo-group-end img)
)


(script-fu-register "script-fu-chaoticbits-border-shadow"
		    _"<Image>/Script-Fu/_Border Shadow..."
		    "Adds a blurred shadow from the edges"
		    "Andy Meneely <me@andymeneely.com>"
		    "Andy Meneely"
		    "June 2011"
		    "RGBA, GRAYA"
		    SF-IMAGE		"Image"			0
		    SF-DRAWABLE		"Drawable"		0
		    SF-COLOR		_"Color"		'(0 0 0)
		    SF-ADJUSTMENT	"Width"		'(100 0 500 1 10 0 0)
		    SF-ADJUSTMENT	"Blur Iterations"	'(4 1 10 1 1 0 0)	
)