

(in-package #:iv-debugger)

;; (defun memory-read (address &key (offset 0))
;;   (let ((word (ptrace-peekdata (process-info-child *process*) (+ address offset))))
;;     word))
 
(defun memory-read (address &key (offset 0))
  (ptrace-peekdata (process-info-child *process*) (+ address offset)))

(defun instruction-read (address)
  "read instruction from address"
  (let ((data1 (memory-read address))
	(data2 (memory-read address :offset 8)))
    (if (and (ptrace-successful? data1) (ptrace-successful? data2))
	(progn
	  (format t "data1: ~a~%" data1)
	  (format t "data2: ~a~%" data2)
	  (let* ((octets1 (bitsmash:int->octets data1))
		 (octets2 (bitsmash:int->octets data2))
		 (merged (concatenate 'vector octets1 octets2)))
	    ;; cut the last bit off, since one instruction can be a
	    ;; max length of 15 bytes (for capstone)
	    (subseq merged 0 (min 15 (length merged)))))
	(format t "instruction-read failed!~a~%~a~%" data1 data2))))

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




;;(cffi:convert-to-foreign

