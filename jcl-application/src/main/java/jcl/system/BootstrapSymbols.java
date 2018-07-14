package jcl.system;

import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;

final class BootstrapSymbols {

	static void bootstrap() {
		GlobalPackageStruct.COMMON_LISP.export(CommonLispSymbols.SETF);
	}
}
