
;; 2 byte le anzeigedauer in 20ms 
;; 1 byte pro pixel intensitaet
;; von oben nach unten dann von links nach rechts
;; 8x18

(+ 2 (* 8 18))


(defparameter *dat*
 (with-open-file (s "c:/Users/martin/Desktop/blinkstroem/BS2.BIN"
		    :element-type `(unsigned-byte 8))
   (let* ((n (file-length s))
	  (a (make-array n :element-type '(unsigned-byte 8))))
     (read-sequence a s)
     a)))

(defparameter *imgs*
 (let ((step (+ 2 (* 8 18))))
   (loop for i below (length *dat*) by step collect
	(list (+ (* 256 (aref *dat* (+ i 1))) (aref *dat* i))
	      (loop for j below (* 8 18) collect
		   (aref *dat* (+ i 2 j)))))))

(defparameter *vid*
 (loop for (tim img) in *imgs* collect
      (let* ((h 8)
	     (w 18)
	     (im (make-array (list w h) :element-type '(unsigned-byte 8)))
	     (im1 (sb-ext:array-storage-vector im)))
	(dotimes (i (length im1))
	  (setf (aref im1 i) (elt img i)))
	im)))
#+nil
(length *vid*)

#+nil
(with-open-file (s "c:/Users/martin/Desktop/blinkstroem/o.txt" :direction :output :if-exists :supersede
		   :if-does-not-exist :create)
 (loop for k below (length *vid*) do
      (let ((frame (elt *vid* k)))
	(destructuring-bind (h w) (array-dimensions frame)
	  (dotimes (j h)
	    (dotimes (i w)
	      (format s "~2d" (aref frame j i)))
	    (terpri s)))
	(terpri s))))
