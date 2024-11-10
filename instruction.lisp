
(in-package #:iv-debugger)

(defparameter *instruction-map* (make-hash-table :test #'equal)
  "hashmap from instruction pointer (u64) to capstone instruction object or objects
   on this capstone instruction object we can use our instruction-* functions
   to obtain info")

(defparameter *capstone-engine*
  (make-instance 'capstone::capstone-engine :architecture :x86 :mode :64))

(defun instructions-add (ip bytes instruction-map)
  (let* ((bytes 
	 (capstone-instruction-object (instruction-disasm :string bytes)))
    (setf (gethash ip instruction-map) capstone-instruction-object)))

(defun instruction-from-ip (ip instruction-map)
  (let ((instruction-found (gethash ip instruction-map)))
    (if (not instruction-found)
	     (instructions-add ip instruction-map))
	(gethash ip instruction-map)))

(defun instruction-disasm (&key bytes string)
  (let ((data (if string
		     (bit-smasher:hex->octets string)
		     bytes)))
      (capstone:disasm *capstone-engine* data)))

(defun instruction-mnemonics (capstone-instruction-vectoqr)
  (let ((mnemonics '()))
    (dotimes (i (length capstone-instruction-vector))
      (push (instruction-mnemonic (aref capstone-instruction-vector i)) mnemonics))
    (nreverse mnemonics)))

(defun instruction-mnemonic (capstone-instruction-object)
  (capstone:mnemonic capstone-instruction-object))

(defun instruction-address (capstone-instruction-object)
  (capstone:address capstone-instruction-object))

(defun instruction-operands (capstone-instruction-object)
  (capstone:operands capstone-instruction-object))


(defun instruction-read (address)
  "read instruction from address"
  (let ((data1 (memory-read address))
	(data2 (memory-read address :offset 8)))
    (if (or (= -1 data1) (= -1 data2))
	nil
	(progn
	  (let* ((octets1 (bitsmash:int->octets data1))
		 (octets2 (bitsmash:int->octets data2))
		 (merged (concatenate 'vector octets1 octets2)))
	    ;; cut the last bit off, since one instruction can be a
	    ;; max length of 15 bytes (for capstone)
	    (subseq merged 0 (min 15 (length merged))))))))
  

(defun instruction-pretty-print (instruction)
  (force-format t "~t[~a] ~a ~a~%"
		(instruction-address instruction)
		(instruction-mnemonic instruction)
		(instruction-operands instruction)))

(defun instructions-pretty-print (instructions)
  (format t "[~%")
  (dotimes (i (length instructions))
    (let ((instruction (aref instructions i)))
      (instruction-pretty-print instruction)))
  (format t "]~%"))






