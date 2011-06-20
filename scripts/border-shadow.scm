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
				  drawable
				  color
				  width
				  iteration
				  merge)
  (gimp-image-undo-group-start img)
  (let* ((origfgcolor (car (gimp-palette-get-foreground)))
	 (origselection (car (gimp-selection-save img)))
	 (drwwidth (car (gimp-drawable-width drawable)))
	 (drwheight (car (gimp-drawable-height drawable)))
	 (drwoffsets (gimp-drawable-offsets drawable))
	 (layername (car (gimp-drawable-get-name drawable)))
	 (strokelayer 0)
	 (drwoffsets (gimp-drawable-offsets drawable))
	 (alphaselection 0)
	 (outerselection 0)
	 (innerselection 0)
	 (origmask 0)
	 (alphamask 0)
	 (outerwidth 0)
	 (innerwidth 0)
	 (growamt 0)
	)
    (if (= position 0)
      (begin
	(set! strokelayer (car (gimp-layer-new img drwwidth drwheight (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-stroke") opacity (get-blending-mode mode))))
	(add-over-layer img strokelayer drawable)
	(gimp-layer-set-offsets strokelayer (car drwoffsets) (cadr drwoffsets))
	(gimp-selection-all img)
	(gimp-edit-clear strokelayer)
	(gimp-selection-none img)
	(gimp-selection-layer-alpha drawable)
	(if (> (car (gimp-layer-get-mask drawable)) -1)
	  (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
	)
	(set! alphaselection (car (gimp-selection-save img)))
	(gimp-selection-shrink img size)
	(set! innerselection (car (gimp-selection-save img)))
	(if (= merge 1)
	  (begin
	    (set! origmask (car (gimp-layer-get-mask drawable)))
	    (if (> origmask -1)
	      (begin
		(set! origmask (car (gimp-channel-copy origmask)))
		(gimp-layer-remove-mask drawable 1)
	      )
	    )
	    (set! alphamask (car (gimp-layer-create-mask drawable 3)))
	    (gimp-selection-none img)
	    (gimp-threshold alphaselection 1 255)
	    (gimp-selection-load alphaselection)
	    (gimp-selection-combine innerselection 1)
	    (gimp-palette-set-foreground color)
	    (gimp-edit-fill strokelayer 0)
	    (set! strokelayer (car (gimp-image-merge-down img strokelayer 0)))
	    (gimp-drawable-set-name strokelayer layername)
	    (gimp-layer-add-mask strokelayer alphamask)
	    (gimp-layer-remove-mask strokelayer 0)
	    (if (> origmask -1)
	      (gimp-layer-add-mask strokelayer origmask)
	    )
	  )
	  (begin
	    (gimp-selection-load alphaselection)
	    (gimp-selection-combine innerselection 1)
	    (gimp-palette-set-foreground color)
	    (gimp-edit-fill strokelayer 0)
	  )
	)
      )
      (if (= position 100)
	(begin
	  (set! growamt (math-round (* size 1.2)))
	  (set! strokelayer (car (gimp-layer-new img (+ drwwidth (* growamt 2)) (+ drwheight (* growamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-stroke") opacity (get-blending-mode mode))))
	  (add-under-layer img strokelayer drawable)
	  (gimp-layer-set-offsets strokelayer (- (car drwoffsets) growamt) (- (cadr drwoffsets) growamt))
	  (gimp-selection-all img)
	  (gimp-edit-clear strokelayer)
	  (gimp-selection-none img)
	  (gimp-selection-layer-alpha drawable)
	  (if (> (car (gimp-layer-get-mask drawable)) -1)
	    (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
	  )
	  (set! alphaselection (car (gimp-selection-save img)))
	  (set! innerselection (car (gimp-selection-save img)))
	  (gimp-selection-none img)
	  (gimp-threshold innerselection 255 255)
	  (gimp-selection-load alphaselection)
	  (gimp-selection-grow img size)
	  (gimp-selection-combine innerselection 1)
	  (gimp-palette-set-foreground color)
	  (gimp-edit-fill strokelayer 0)
	  (if (= merge 1)
	    (begin
	      (set! origmask (car (gimp-layer-get-mask drawable)))
	      (if (> origmask -1)
		(gimp-layer-remove-mask drawable 0)
	      )
	      (set! strokelayer (car (gimp-image-merge-down img drawable 0)))
	      (gimp-drawable-set-name strokelayer layername)
	    )
	  )
	)
	(begin
	  (set! outerwidth (math-round (* (/ position 100) size)))
	  (set! innerwidth (- size outerwidth))
	  (set! growamt (math-round (* outerwidth 1.2)))
	  (set! strokelayer (car (gimp-layer-new img (+ drwwidth (* growamt 2)) (+ drwheight (* growamt 2)) (cond ((= (car (gimp-image-base-type img)) 0) 1) ((= (car (gimp-image-base-type img)) 1) 3)) (string-append layername "-stroke") opacity (get-blending-mode mode))))
	  (add-over-layer img strokelayer drawable)
	  (gimp-layer-set-offsets strokelayer (- (car drwoffsets) growamt) (- (cadr drwoffsets) growamt))
	  (gimp-selection-all img)
	  (gimp-edit-clear strokelayer)
	  (gimp-selection-none img)
	  (gimp-selection-layer-alpha drawable)
	  (if (> (car (gimp-layer-get-mask drawable)) -1)
	    (gimp-selection-combine (car (gimp-layer-get-mask drawable)) 3)
	  )
	  (set! alphaselection (car (gimp-selection-save img)))
	  (gimp-selection-shrink img innerwidth)
	  (set! innerselection (car (gimp-selection-save img)))
	  (gimp-selection-load alphaselection)
	  (gimp-selection-grow img outerwidth)
	  (gimp-selection-combine innerselection 1)
	  (gimp-palette-set-foreground color)
	  (gimp-edit-fill strokelayer 0)
	  (if (= merge 1)
	    (begin
	      (set! origmask (car (gimp-layer-get-mask drawable)))
	      (if (> origmask -1)
		(gimp-layer-remove-mask drawable 0)
	      )
	      (set! strokelayer (car (gimp-image-merge-down img strokelayer 0)))
	      (gimp-drawable-set-name strokelayer layername)
	    )
	  )
	)
      )
    )
    (gimp-palette-set-foreground origfgcolor)
    (gimp-selection-load origselection)
    (gimp-image-remove-channel img alphaselection)
    (gimp-image-remove-channel img innerselection)
    (gimp-image-remove-channel img origselection)
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
		    SF-TOGGLE		"Merge with layer"	FALSE)