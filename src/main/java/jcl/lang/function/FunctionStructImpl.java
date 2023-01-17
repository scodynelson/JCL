package jcl.lang.function;

import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.internal.LispStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public abstract class FunctionStructImpl extends LispStructImpl implements FunctionStruct {

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.FUNCTION;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.FUNCTION;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.FUNCTION) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.COMPILED_FUNCTION) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.FUNCTION) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
