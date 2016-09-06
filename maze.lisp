(require 'asdf)
(asdf:load-system :zpng)
(use-package :zpng)

;arg1 is a flag and it returns the flags integer value 
(setf arguments '("-w" "1920" "-h" "1080"))
(defun get-arg (arg1) 
  (setf pos (position arg1 arguments :test #'equal)  ) 
  (handler-case
    (parse-integer ( elt arguments (+ pos 1) ) )
    (error (ex) nil))
  )
(defun get-args (arg1 arg2 default) 
  (setf result (car (remove nil(list (get-arg arg1) (get-arg arg2) ))))
  (if (equal result nil)default result )
  )
;Width of picture
(defvar global-width (get-args "-w" "--width" 1920))
;Height of picture
(defvar global-height (get-args "-h" "--height" 1080))

(defvar rand-state1 (make-random-state t))

(defun get-maze-map (width height &optional seed) 
  (setf maze-array (make-array (list width height) :initial-element nil ))
  ;Returns true if spot isn't filled
  (defun check (xpos ypos)
    (handler-case
      (null (aref maze-array xpos ypos))
      (error (ex) nil))
    )
  (setf coor-loop '((0 1) (0 -1) (1 0) (-1 0)))
  ;Returns true if there is one open spot next to (xpos ypos)
  (defun check-all (xpos ypos)
    (loop for coor in coor-loop
	  do
	  (
	   if (check (+ (car coor) xpos) (+ (cadr coor) ypos) ) (return T)
	   )
	  )
    )

  (setf coor-array (make-array 4 :initial-contents coor-loop))
  (defun get-random-coor (xpos ypos)
    (if (check-all xpos ypos)
      (loop 
	(setf coor (aref coor-array (random 4 rand-state1)))
	(let ((x (car coor))(y (cadr coor)))
	  (if (check (+ x xpos) (+ y ypos))
	    (return coor))
	  )
	)
      nil
      )
    )
  ;Stores the coordinates of the last location
  (setf last-coor '())
  (setf max-value 1)
  ;Populates the map with values, xpos, ypos are initial position, value is 1 tcount is 1
  (defun populate-map (xpos ypos value tcount) 
    (if (<= tcount (* width height))
      (progn
	(setf next-move (get-random-coor xpos ypos))
	(if (null next-move) 
	  (let ((coor (pop last-coor) )) 
	    (populate-map (car coor) (cadr coor) (- value 1) tcount)
	    ) 
	  (let ( 
		 (nextx (+ xpos (car next-move))) 
		 (nexty (+ ypos (cadr next-move)))
		 ) 
	    (push (list xpos ypos) last-coor)
	    (setf max-value (max max-value value))
	    (setf (aref maze-array nextx nexty) value)
	    (populate-map nextx nexty (+ 1 value) (+ 1 tcount))
	    )
	  )
	)
      max-value
      )
    )
    (let ((x (random width rand-state1))(y (random height rand-state1)))
  (populate-map x y 1 1)
  )
  (list maze-array max-value)
  )
(defun draw-png (coor-map max-value)
  (setf dimentions (array-dimensions coor-map))
  (let ((width (car dimentions)) (height (cadr dimentions)))
    (setf png (make-instance 'png :width width :height height :color-type :truecolor))
    (setf image (data-array png))
    (loop for x from 0 to (- width 1)
	  do
	  (loop for y from 0 to (- height 1)
		do
		(setf value ( floor (* 255 (/ (aref coor-map x y) max-value))))
		(setf (aref image y x 0) value)
		))
    (write-png png "test.png")
    )
  )
;executes everything
((lambda (pa) (draw-png (car pa) (cadr pa))) (get-maze-map global-width global-height))
