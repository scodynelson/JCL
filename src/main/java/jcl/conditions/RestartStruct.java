package jcl.conditions;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.Restart;

public abstract class RestartStruct implements LispStruct {

	private static final long serialVersionUID = 521240973174042359L;

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
