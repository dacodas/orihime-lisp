(in-package :orihime)

(defparameter *goo-trie-socket* (usocket:socket-connect "localhost" 7081))
(defparameter *goo-local-cache* #P"/home/dacoda/projects/goo-processing/dictionary-entries/")

(defun lookup-word-local (reading)

  (let ((stream (usocket:socket-stream *goo-trie-socket*)))
    (format stream "LEMMEKNOW ~A" reading)
    (force-output stream))

  (sleep 0.1)

  ;; Handle the error here I get when the UTF-8 character ends in the middle
  (let ((results (let ((input-buffer (make-array 1048576 :element-type '(unsigned-byte 8))))
                   (multiple-value-bind (buffer length peer-address)
                       (sb-bsd-sockets:socket-receive (usocket:socket *goo-trie-socket*) input-buffer nil)
                     (read-from-string (sb-ext:octets-to-string buffer))))))
    results))
