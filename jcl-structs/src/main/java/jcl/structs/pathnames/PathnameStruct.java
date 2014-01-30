package jcl.structs.pathnames;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.pathnames.Pathname;

import java.util.List;

public class PathnameStruct implements LispStruct {

	/*
(defstruct (pathname
	    (:conc-name %pathname-)
	    (:print-function %print-pathname)
	    (:constructor
	     %make-pathname (host device directory name type version))
	    (:predicate pathnamep)
	    (:make-load-form-fun :just-dump-it-normally))
  ;; Slot holds the host, at present either a UNIX or logical host.
  (host nil :type (or host null))
  ;; Device is the name of a logical or physical device holding files.
  (device nil :type (or simple-string component-tokens))
  ;; A list of strings that are the component subdirectory components.
  (directory nil :type list)
  ;; The filename.
  (name nil :type (or simple-string pattern component-tokens))
  ;; The type extension of the file.
  (type nil :type (or simple-string pattern component-tokens))
  ;; The version number of the file, a positive integer, but not supported
  ;; on standard UNIX filesystems.
  (version nil :type (or integer component-tokens (member :newest))))
	 */

	private Object host = null;
	private Object device = null;
	private List directory = null;
	private Object name = null;
	private Object type = null;
	private Object version = null;

	@Override
	public LispType getType() {
		return Pathname.INSTANCE;
	}

	// BUILDERS

	public static PathnameStruct getStruct(final String nameString) {
		return new PathnameStruct();
	}
}
