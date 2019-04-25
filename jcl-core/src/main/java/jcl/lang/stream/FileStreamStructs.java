/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import java.nio.file.Path;

import jcl.lang.LispStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.internal.stream.FileStreamStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/**
 * Created by codynelson on 2/15/16.
 */
public final class FileStreamStructs {

	private FileStreamStructs() {
	}

	public static FileStreamStructImpl open(final PathnameStruct pathname, final DirectionType directionType, final LispStruct elementType,
	                                        final IfExistsType ifExistsType, final IfDoesNotExistType ifDoesNotExistType,
	                                        final ExternalFormat externalFormat) {

		final Path filePath = pathname.getPath();

		final FileStreamStructImpl fileStream = new FileStreamStructImpl(filePath);
		if (directionType == DirectionType.PROBE) {
			fileStream.close();
		}
		return fileStream;
	}

	public static FileStreamStructImpl open(final PathnameStruct pathname, final SymbolStruct directionSymbol, final LispStruct elementType,
	                                        final SymbolStruct ifExistsSymbol, final SymbolStruct ifDoesNotExistSymbol,
	                                        final SymbolStruct externalFormat) {

		final DirectionType directionType = DirectionType.fromValue(directionSymbol);
		final IfExistsType ifExistsType = IfExistsType.fromValue(ifExistsSymbol);
		final IfDoesNotExistType ifDoesNotExistType = IfDoesNotExistType.fromValue(ifDoesNotExistSymbol);

		return open(pathname, directionType, CommonLispSymbols.CHARACTER, ifExistsType, ifDoesNotExistType, ExternalFormat.DEFAULT);
	}
}

/*
(defun open (filename
	     &key
	     (direction :input)
	     (element-type 'character)
	     (if-exists nil if-exists-p)
	     (if-does-not-exist nil if-does-not-exist-p)
	     (external-format :default))
  (setf element-type (case element-type
                       ((character base-char)
                        'character)
                       (:default
                        '(unsigned-byte 8))
                       (t
                        (upgraded-element-type element-type))))

  (when (memq direction '(:output :io))
    (when (not if-exists-p)
      (setf if-exists
            (if (eq (pathname-version pathname) :newest)
                :new-version
              :error))))

  (when (not if-does-not-exist-p)
    (setf if-does-not-exist
          (cond ((eq direction :input) :error)
                ((and (memq direction '(:output :io))
                      (memq if-exists '(:overwrite :append)))
                 :error)
                ((eq direction :probe)
                 nil)
                (t
                 :create))))

  (case direction
    (:input
     (case if-does-not-exist
       (:error
        (when (not (probe-file pathname))
          (error "The file ~S does not exist."))))
     (make-file-stream pathname namestring element-type :input nil external-format))

    (:probe
     (case if-does-not-exist
       (:error
        (when (not (probe-file pathname))
          (error "The file ~S does not exist.")))
       (:create
        (create-new-file namestring)))
     (let ((stream (make-file-stream pathname namestring element-type :input nil external-format)))
       (when stream
         (close stream))
       stream))

    ((:output :io)
     (case if-does-not-exist
       (:error
        (when (not (probe-file pathname))
          (error "The file ~S does not exist.")))
       ((nil)
        (when (not (probe-file pathname))
          (return-from open nil))))
     (case if-exists
       (:error
        (when (probe-file pathname)
          (error "The file ~S already exists.")))

       ((nil)
        (when (probe-file pathname)
          (return-from open nil)))

       ((:rename :rename-and-delete)
        (when (probe-file pathname)
          ;; Make sure the original file is not a directory.
          (when (probe-directory pathname)
            (error "The file ~S is a directory."))
          (let ((backup-name (concatenate 'string namestring ".bak")))
            (when (probe-file backup-name)
              (when (probe-directory backup-name)
                (error "Unable to rename ~S."))
              (delete-file backup-name))
            (rename-file pathname backup-name))))

       ((:new-version :supersede :overwrite :append)) ; OK to proceed.

       (t
        (error "Option not supported: ~S.")))

     (let ((stream (make-file-stream pathname namestring element-type direction if-exists external-format)))
       (when (not stream)
         (error "Unable to open ~S."))
       stream))
    (t
     (error ":DIRECTION ~S not supported.")))))
 */