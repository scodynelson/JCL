;;;; Copyright (C) 2011-2014 Cody Nelson - All rights reserved.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "base-macro-lambdas")
  (require "macros")
  (require "iterators")
) ;eval-when

(in-package "COMMON-LISP")

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: def-var: *terminal-io* *debug-io* *query-io*
;; TODO: def-var: *standard-input*
;; TODO: def-var: *standard-output* *error-output* *trace-output*

;;;;;;;;;;;;;;;;;;;;;;

(defun make-broadcast-stream (&rest output-streams)
  "Returns a broadcast stream that has the indicated output-streams initially associated with it."
  (declare (system::%java-class-name "jcl.streams.functions.MakeBroadcastStream"))
  (ext:jinvoke-static
    (ext:jmethod "toBroadcastStream" (ext:jclass "jcl.lang.BroadcastStreamStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           output-streams)))

(defun make-concatenated-stream (&rest input-streams)
  "Returns a concatenated stream that has the indicated input-streams initially associated with it."
  (declare (system::%java-class-name "jcl.streams.functions.MakeConcatenatedStream"))
  (ext:jinvoke-static
    (ext:jmethod "toConcatenatedStream" (ext:jclass "jcl.lang.ConcatenatedStreamStruct")
                 (ext:jclass "java.util.List"))
    (ext:jinvoke-interface (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
                           input-streams)))

(defun make-echo-stream (input-stream output-stream)
  "Returns a echo stream that gets its input from input-stream and sends its output to output-stream."
  (declare (system::%java-class-name "jcl.streams.functions.MakeEchoStream"))
  (ext:jinvoke-static
    (ext:jmethod "toEchoStream" (ext:jclass "jcl.lang.EchoStreamStruct")
                 (ext:jclass "jcl.lang.InputStreamStruct")
                 (ext:jclass "jcl.lang.OutputStreamStruct"))
    input-stream output-stream))

(defun make-two-way-stream (input-stream output-stream)
  "Returns a two-way stream that gets its input from input-stream and sends its output to output-stream."
  (declare (system::%java-class-name "jcl.streams.functions.MakeTwoWayStream"))
  (ext:jinvoke-static
    (ext:jmethod "toTwoWayStream" (ext:jclass "jcl.lang.TwoWayStreamStruct")
                 (ext:jclass "jcl.lang.InputStreamStruct")
                 (ext:jclass "jcl.lang.OutputStreamStruct"))
    input-stream output-stream))

(defun make-string-input-stream (string &optional (start 0) end)
  "Returns an input string stream."
  (declare (system::%java-class-name "jcl.streams.functions.MakeStringInputStream"))
  (let* ((string (string string))
         (end (or end (length string))))
    (ext:jinvoke-static
      (ext:jmethod "toStringInputStream" (ext:jclass "jcl.lang.StringInputStreamStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.FixnumStruct")
                   (ext:jclass "jcl.lang.FixnumStruct"))
      string start end)))

(defun make-string-output-stream (&key (element-type 'character))
  "Returns an output string stream that accepts characters and makes available (via get-output-stream-string) a string
  that contains the characters that were actually output."
  (declare (system::%java-class-name "jcl.streams.functions.MakeStringOutputStream"))
  (ext:jinvoke-static
    (ext:jmethod "toStringOutputStream" (ext:jclass "jcl.lang.StringOutputStreamStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    element-type))

(defun make-synonym-stream (symbol)
  "Returns a synonym stream whose synonym stream symbol is symbol."
  (declare (system::%java-class-name "jcl.streams.functions.MakeSynonymStream"))
  (ext:jinvoke-static
    (ext:jmethod "toSynonymStream" (ext:jclass "jcl.lang.SynonymStreamStruct")
                 (ext:jclass "jcl.lang.SymbolStruct"))
    symbol))

;;;;;;;;;;;;;;;;;;;;;;

(defun streamify-designator (designator input-p)
  (if (eql designator nil)
      (if input-p
          *standard-input*
        *standard-output*)
    (if (eql designator t)
        *terminal-io*
      (if (streamp designator)
           designator
        (error "The designator value is not either T, NIL, or a Stream.")))))

;;;;;;;;;;;;;;;;;;;;;;

(defun input-stream-p (stream)
  "Returns true if stream is an input stream; otherwise, returns false."
  (declare (system::%java-class-name "jcl.streams.functions.InputStreamP"))
  (ext:jinvoke-interface
    (ext:jmethod "inputStreamP" (ext:jclass "jcl.lang.StreamStruct"))
    stream))

(defun output-stream-p (stream)
  "Returns true if stream is an output stream; otherwise, returns false."
  (declare (system::%java-class-name "jcl.streams.functions.OutputStreamP"))
  (ext:jinvoke-interface
    (ext:jmethod "outputStreamP" (ext:jclass "jcl.lang.StreamStruct"))
    stream))

(defun interactive-stream-p (stream)
  "Returns true if stream is an interactive stream; otherwise, returns false."
  (declare (system::%java-class-name "jcl.streams.functions.InteractiveStreamP"))
  (ext:jinvoke-interface
    (ext:jmethod "interactiveStreamP" (ext:jclass "jcl.lang.StreamStruct"))
    stream))

(defun open-stream-p (stream)
  "Returns true if stream is an open stream; otherwise, returns false."
  (declare (system::%java-class-name "jcl.streams.functions.OpenStreamP"))
  (ext:jinvoke-interface
    (ext:jmethod "openStreamP" (ext:jclass "jcl.lang.StreamStruct"))
    stream))

(defun stream-element-type (stream)
  "Returns a type specifier that indicates the types of objects that may be read from or written to stream."
  (declare (system::%java-class-name "jcl.streams.functions.StreamElementType"))
  (ext:jinvoke-interface
    (ext:jmethod "streamElementType" (ext:jclass "jcl.lang.StreamStruct"))
    stream))

;;;;;;;;;;;;;;;;;;;;;;

(defun read-byte (input-stream &optional (eof-error t) (eof-value nil))
  "Returns the next byte from input-stream."
  (declare (system::%java-class-name "jcl.streams.functions.ReadByte"))
  (ext:jinvoke-interface
    (ext:jmethod "readByte" (ext:jclass "jcl.lang.InputStreamStruct")
                 (ext:jclass "jcl.lang.BooleanStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    input-stream eof-error eof-value))

(defun write-byte (byte output-stream)
  "Outputs the byte to the output-stream."
  (declare (system::%java-class-name "jcl.streams.functions.WriteByte"))
  (ext:jinvoke-interface
    (ext:jmethod "writeByte" (ext:jclass "jcl.lang.OutputStreamStruct")
                 (ext:jclass "jcl.lang.IntegerStruct"))
    output-stream byte))

;;;;;;;;;;;;;;;;;;;;;;
#|
(defun peek-char (&optional (peek-type nil) (input-stream *standard-input*) (eof-error t) (eof-value nil) (recursive-p nil))
  "Obtains the next character in input-stream without actually reading it."
  (declare (system::%java-class-name "jcl.streams.functions.PeekChar"))
  (let ((input-stream (streamify-designator input-stream t)))
    (cond ((eq peek-type nil)
           (let ((c (read-char input-stream eof-error eof-value recursive-p)))
             (unread-char c)
             c))
          ((eq peek-type t)
           (do ((c (read-char input-stream eof-error eof-value recursive-p)))
               ((not (ext::whitespacep c)) ;TODO: implement whitespacep; export whitespacep [ext:whitespacep]
                (progn (unread-char c) c))))
          ((characterp peek-type)
           (do ((c (read-char input-stream eof-error eof-value recursive-p)))
               ((eq peek-type c)
                (progn (unread-char c) c))))
          (t (error "Peek-type must be: nil, t, character.")))))
|#
(defun read-char (&optional (input-stream *standard-input*) (eof-error t) (eof-value nil) (recursive-p nil))
  "Returns the next character from input-stream."
  (declare (system::%java-class-name "jcl.streams.functions.ReadChar"))
  (let ((input-stream (streamify-designator input-stream t)))
    (ext:jinvoke-interface
      ;; TODO: not using recursive-p; Need to fix/research the correct approach
      (ext:jmethod "readChar" (ext:jclass "jcl.lang.InputStreamStruct")
                   (ext:jclass "jcl.lang.BooleanStruct")
                   (ext:jclass "jcl.lang.LispStruct"))
      input-stream eof-error eof-value)))

(defun read-char-no-hang (&optional (input-stream *standard-input*) (eof-error t) (eof-value nil) (recursive-p nil))
  "Returns the next character from input-stream."
  (declare (system::%java-class-name "jcl.streams.functions.ReadCharNoHang"))
  (let ((input-stream (streamify-designator input-stream t)))
    (ext:jinvoke-interface
      ;; TODO: not using recursive-p; Need to fix/research the correct approach
      (ext:jmethod "readCharNoHang" (ext:jclass "jcl.lang.InputStreamStruct")
                   (ext:jclass "jcl.lang.BooleanStruct")
                   (ext:jclass "jcl.lang.LispStruct"))
      input-stream eof-error eof-value)))

(defun unread-char (character &optional (input-stream *standard-input*))
  "Places character back onto the front of input-stream so that it will again be the next character in input-stream."
  (declare (system::%java-class-name "jcl.streams.functions.UnreadChar"))
  (let ((input-stream (streamify-designator input-stream t)))
    (ext:jinvoke-interface
      (ext:jmethod "unreadChar" (ext:jclass "jcl.lang.InputStreamStruct")
                   (ext:jclass "jcl.lang.CharacterStruct"))
      input-stream character)))

(defun write-char (character &optional (output-stream *standard-output*))
  "Outputs character to output-stream."
  (declare (system::%java-class-name "jcl.streams.functions.WriteChar"))
  (let ((output-stream (streamify-designator output-stream nil)))
    (ext:jinvoke-interface
      (ext:jmethod "writeChar" (ext:jclass "jcl.lang.OutputStreamStruct")
                   (ext:jclass "jcl.lang.CharacterStruct"))
      output-stream character)))

;;;;;;;;;;;;;;;;;;;;;;

(defun read-line (&optional (input-stream *standard-input*) (eof-error t) (eof-value nil) (recursive-p nil))
  "Returns the next line of text that is terminated by a newline or end of file."
  (declare (system::%java-class-name "jcl.streams.functions.ReadLine"))
  (let ((input-stream (streamify-designator input-stream t)))
    (ext:jinvoke-virtual
      (ext:jmethod "toValues" (ext:jclass "jcl.lang.ReadLineResult"))
      (ext:jinvoke-interface
        ;; TODO: not using recursive-p; Need to fix/research the correct approach
        (ext:jmethod "readLine" (ext:jclass "jcl.lang.InputStreamStruct")
                     (ext:jclass "jcl.lang.BooleanStruct")
                     (ext:jclass "jcl.lang.LispStruct"))
        input-stream eof-error eof-value))))

(defun write-line (string &optional (output-stream *standard-output*) &key (start 0) end)
  "Writes the characters of the sub-sequence of string bounded by start and end to output-stream followed by a newline."
  (declare (system::%java-class-name "jcl.streams.functions.WriteLine"))
  (let ((output-stream (streamify-designator output-stream nil))
        (end (or end (length string))))
    (ext:jinvoke-interface
      (ext:jmethod "writeLine" (ext:jclass "jcl.lang.OutputStreamStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.FixnumStruct")
                   (ext:jclass "jcl.lang.FixnumStruct"))
      output-stream string start end)))

(defun write-string (string &optional (output-stream *standard-output*) &key (start 0) end)
  "Writes the characters of the sub-sequence of string bounded by start and end to output-stream."
  (declare (system::%java-class-name "jcl.streams.functions.WriteString"))
  (let ((output-stream (streamify-designator output-stream nil))
        (end (or end (length string))))
    (ext:jinvoke-interface
      (ext:jmethod "writeString" (ext:jclass "jcl.lang.OutputStreamStruct")
                   (ext:jclass "jcl.lang.StringStruct")
                   (ext:jclass "jcl.lang.FixnumStruct")
                   (ext:jclass "jcl.lang.FixnumStruct"))
      output-stream string start end)))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: read-sequence/write-sequence

;;;;;;;;;;;;;;;;;;;;;;

(defun file-length (stream)
  "Returns the length of stream, or nil if the length cannot be determined."
  (declare (system::%java-class-name "jcl.streams.functions.FileLength"))
  (ext:jinvoke-interface
    (ext:jmethod "fileLength" (ext:jclass "jcl.lang.StreamStruct"))
    stream))

(defun file-position (stream &optional position-spec)
  "Returns the length of stream, or nil if the length cannot be determined."
  (declare (system::%java-class-name "jcl.streams.functions.FilePosition"))
  (cond ((integerp position-spec)
         (ext:jinvoke-interface
           (ext:jmethod "filePosition" (ext:jclass "jcl.lang.StreamStruct")
                        (ext:jclass "jcl.lang.IntegerStruct"))
           stream position-spec))
        ((eq position-spec :start)
         (ext:jinvoke-interface
           (ext:jmethod "filePosition" (ext:jclass "jcl.lang.StreamStruct")
                        (ext:jclass "jcl.lang.IntegerStruct"))
           stream 0))
        ((eq position-spec :end)
         (ext:jinvoke-interface
           (ext:jmethod "filePosition" (ext:jclass "jcl.lang.StreamStruct")
                        (ext:jclass "jcl.lang.IntegerStruct"))
           stream (file-length stream)))
        ((null position-spec)
         (ext:jinvoke-interface
           (ext:jmethod "filePosition" (ext:jclass "jcl.lang.StreamStruct"))
           stream))
        (t
         (error "Invalid position-spec value. Must be: an integer, :start, :end, or nil."))))

(defun file-string-length (stream object)
  "Returns the difference between what (file-position stream) would be after writing object and its current value,
   or nil if this cannot be determined."
  (declare (system::%java-class-name "jcl.streams.functions.FileStringLength"))
  (ext:jinvoke-static
    (ext:jmethod "fileStringLength" (ext:jclass "jcl.lang.FileStreamStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    object))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Expand `open` function to include all checks and full functionality

(defun open (filespec &key (direction :input) element-type if-exists if-does-not-exist (external-format :default))
  "Writes the characters of the sub-sequence of string bounded by start and end to output-stream."
  (declare (system::%java-class-name "jcl.streams.functions.Open"))
  (ext:jinvoke-static
    (ext:jmethod "toFileStream" (ext:jclass "jcl.lang.FileStreamStruct")
                 (ext:jclass "jcl.lang.PathnameStruct")
                 (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct")
                 (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.SymbolStruct")
                 (ext:jclass "jcl.lang.LispStruct"))
    filespec direction element-type if-exists if-does-not-exist external-format))

(defun close (stream &key (abort nil))
  "Closes Stream."
  (declare (system::%java-class-name "jcl.streams.functions.Close"))
  (ext:jinvoke-interface
    (ext:jmethod "close" (ext:jclass "jcl.lang.StreamStruct")
                 (ext:jclass "jcl.lang.BooleanStruct"))
    stream abort))

;;;;;;;;;;;;;;;;;;;;;;

(defun listen (&optional (input-stream *standard-input*))
  "Returns true if there is a character immediately available from input-stream; otherwise, returns false."
  (declare (system::%java-class-name "jcl.streams.functions.Listen"))
  (let ((output-stream (streamify-designator input-stream t)))
    (ext:jinvoke-interface
      (ext:jmethod "listen" (ext:jclass "jcl.lang.InputStreamStruct"))
      input-stream)))

;;;;;;;;;;;;;;;;;;;;;;

(defun terpri (&optional (output-stream *standard-output*))
  "Outputs a newline to output-stream."
  (declare (system::%java-class-name "jcl.streams.functions.Terpri"))
  (let ((output-stream (streamify-designator output-stream nil)))
    (ext:jinvoke-interface
      (ext:jmethod "terpri" (ext:jclass "jcl.lang.OutputStreamStruct"))
      output-stream)))

(defun fresh-line (&optional (output-stream *standard-output*))
  "Outputs a newline only if the output-stream is not already at the start of a line."
  (declare (system::%java-class-name "jcl.streams.functions.FreshLine"))
  (let ((output-stream (streamify-designator output-stream nil)))
    (ext:jinvoke-interface
      (ext:jmethod "freshLine" (ext:jclass "jcl.lang.OutputStreamStruct"))
      output-stream)))

;;;;;;;;;;;;;;;;;;;;;;

(defun stream-external-format (file-stream)
  "Returns an external file format designator for the stream."
  (declare (system::%java-class-name "jcl.streams.functions.StreamExternalFormat"))
  (ext:jinvoke-interface
    (ext:jmethod "streamExternalFormat" (ext:jclass "jcl.lang.FileStreamStruct"))
    file-stream))

;;;;;;;;;;;;;;;;;;;;;;

(defun clear-input (&optional (input-stream *standard-input*))
  "Clears any available input from input-stream."
  (declare (system::%java-class-name "jcl.streams.functions.ClearInput"))
  (let ((output-stream (streamify-designator input-stream t)))
    (ext:jinvoke-interface
      (ext:jmethod "clearInput" (ext:jclass "jcl.lang.InputStreamStruct"))
      input-stream)))

(defun clear-output (&optional (output-stream *standard-output*))
  "Attempts to abort any outstanding output operation in progress in order to allow as little output as possible to
  continue to the destination."
  (declare (system::%java-class-name "jcl.streams.functions.ClearOutput"))
  (let ((output-stream (streamify-designator output-stream nil)))
    (ext:jinvoke-interface
      (ext:jmethod "clearOutput" (ext:jclass "jcl.lang.OutputStreamStruct"))
      output-stream)))

(defun finish-output (&optional (output-stream *standard-output*))
  "Attempts to ensure that any buffered output sent to output-stream has reached its destination, and then returns."
  (declare (system::%java-class-name "jcl.streams.functions.FinishOutput"))
  (let ((output-stream (streamify-designator output-stream nil)))
    (ext:jinvoke-interface
      (ext:jmethod "finishOutput" (ext:jclass "jcl.lang.OutputStreamStruct"))
      output-stream)))

(defun force-output (&optional (output-stream *standard-output*))
  "Initiates the emptying of any internal buffers but does not wait for completion or acknowledgment to return."
  (declare (system::%java-class-name "jcl.streams.functions.ForceOutput"))
  (let ((output-stream (streamify-designator output-stream nil)))
    (ext:jinvoke-interface
      (ext:jmethod "forceOutput" (ext:jclass "jcl.lang.OutputStreamStruct"))
      output-stream)))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: y-or-n-p/yes-or-no-p

;;;;;;;;;;;;;;;;;;;;;;

(defun broadcast-stream-streams (broadcast-stream)
  "Returns a list of output streams that constitute all the streams to which the broadcast-stream is broadcasting."
  (declare (system::%java-class-name "jcl.streams.functions.BroadcastStreamStreams"))
  (ext:jinvoke-interface
    (ext:jmethod "broadcastStreamStreams" (ext:jclass "jcl.lang.BroadcastStreamStruct"))
    broadcast-stream))

(defun concatenated-stream-streams (concatenated-stream)
  "Returns a list of input streams that constitute the ordered set of streams the concatenated-stream still has to read
  from, starting with the current one it is reading from."
  (declare (system::%java-class-name "jcl.streams.functions.ConcatenatedStreamStreams"))
  (ext:jinvoke-interface
    (ext:jmethod "concatenatedStreamStreams" (ext:jclass "jcl.lang.ConcatenatedStreamStruct"))
    concatenated-stream))

(defun echo-stream-input-stream (echo-stream)
  "Returns the input stream from which echo-stream receives input."
  (declare (system::%java-class-name "jcl.streams.functions.EchoStreamInputStream"))
  (ext:jinvoke-interface
    (ext:jmethod "echoStreamInputStream" (ext:jclass "jcl.lang.EchoStreamStruct"))
    echo-stream))

(defun echo-stream-output-stream (echo-stream)
  "Returns the output stream to which echo-stream sends output."
  (declare (system::%java-class-name "jcl.streams.functions.EchoStreamOutputStream"))
  (ext:jinvoke-interface
    (ext:jmethod "echoStreamOutputStream" (ext:jclass "jcl.lang.EchoStreamStruct"))
    echo-stream))

(defun two-way-stream-input-stream (two-way-stream)
  "Returns the input stream from which two-way-stream receives input."
  (declare (system::%java-class-name "jcl.streams.functions.TwoWayStreamInputStream"))
  (ext:jinvoke-interface
    (ext:jmethod "twoWayStreamInputStream" (ext:jclass "jcl.lang.TwoWayStreamStruct"))
    two-way-stream))

(defun two-way-stream-output-stream (two-way-stream)
  "Returns the output stream to which two-way-stream sends output."
  (declare (system::%java-class-name "jcl.streams.functions.TwoWayStreamOutputStream"))
  (ext:jinvoke-interface
    (ext:jmethod "twoWayStreamOutputStream" (ext:jclass "jcl.lang.TwoWayStreamStruct"))
    two-way-stream))

;;;;;;;;;;;;;;;;;;;;;;

(defun synonym-stream-symbol (synonym-stream)
  "Returns the symbol whose symbol-value the synonym-stream is using."
  (declare (system::%java-class-name "jcl.streams.functions.SynonymStreamSymbol"))
  (ext:jinvoke-interface
    (ext:jmethod "synonymStreamSymbol" (ext:jclass "jcl.lang.SynonymStreamStruct"))
    synonym-stream))

;;;;;;;;;;;;;;;;;;;;;;

(defun get-output-stream-string (string-output-stream)
  "Returns a string containing, in order, all the characters that have been output to string-output-stream."
  (declare (system::%java-class-name "jcl.streams.functions.GetOutputStreamString"))
  (ext:jinvoke-interface
    (ext:jmethod "getOutputStreamString" (ext:jclass "jcl.lang.StringOutputStreamStruct"))
    string-output-stream))

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: stream-error-stream
#|
(defun stream-error-stream (condition)
  "Returns the offending stream in the situation represented by the condition."
  (declare (system::%java-class-name "jcl.streams.functions.StreamErrorStream"))

  ;; StreamErrorException streamErrorException = (StreamErrorException) condition;
  ;; return streamErrorException.getStreamWithError();

  nil)
|#
;;;;;;;;;;;;;;;;;;;;;;

;; TODO: with-open-stream
;; TODO: with-open-file

;;;;;;;;;;;;;;;;;;;;;;

;; TODO: with-input-from-string
;; TODO: with-output-to-string

;;;;;;;;;;;;;;;;;;;;;;

(provide "streams")