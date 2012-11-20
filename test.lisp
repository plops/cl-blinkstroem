
;; 2 byte le anzeigedauer in 20ms 
;; 1 byte pro pixel intensitaet
;; von oben nach unten dann von links nach rechts
;; 8x18

(+ 2 (* 8 18))


(defparameter *dat*
 (with-open-file (s "BS2.BIN"
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

#+nil
(loop for (tim frame) in *imgs* maximize
     (loop for v in frame maximize v))
;; max value is 11


(defparameter *vid*
 (loop for (tim img) in *imgs* collect
      (let* ((h 8)
	     (w 18)
	     (im (make-array (list w h) :element-type '(unsigned-byte 8)))
	     (im1 (sb-ext:array-storage-vector im)))
	(dotimes (i (length im1))
	  (setf (aref im1 i) (floor (* 255 (elt img i))
				    11)))
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

(defun write-pgm (filename img)
  (declare (simple-string filename)
           ((array (unsigned-byte 8) 2) img)
           (values null &optional))
  (destructuring-bind (h w)
      (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" w h))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append)
      (let ((data-1d (make-array 
                      (* h w)
                      :element-type '(unsigned-byte 8)
                      :displaced-to img)))
        (write-sequence data-1d s)))
    nil))

(loop for k below (length *vid*) do
     (let ((frame (elt *vid* k)))
       (write-pgm (format nil "/dev/shm/f~6,'0d.pgm" k)
		  frame)))


(defparameter *balken*
 (let* ((h 8)
	(w 18)
	(n 180)
	(a (make-array (list n h w) :element-type '(unsigned-byte 8))))
   (loop for k below n do
	(loop for j below h do
	     (loop for i below w do
		  (setf (aref a k j i) (if (= i (mod k 18)) 11 0)))))
   a))

(with-open-file (s "/dev/shm/o.bin" :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create
		   :element-type '(unsigned-byte 8))
  (destructuring-bind (n h w) (array-dimensions *balken*)
    (loop for k below n do
	 (write-byte 64 s)
	 (write-byte 0 s)
	 (loop for j below h do
	      (loop for i below w do
		   (write-byte (aref *balken* k j i) s))))))
