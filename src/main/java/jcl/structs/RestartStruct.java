package jcl.structs;

import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.Restart;

public class RestartStruct implements LispStruct {

	/*
(defstruct (restart (:print-function restart-print))
  name
  function
  report-function
  interactive-function
  (test-function #'(lambda (cond) (declare (ignore cond)) t)))
	 */

	@Override
	public LispType getType() {
		return Restart.INSTANCE;
	}
}
