package jcl.lang.internal;

import jcl.compiler.GenerationUnsupportedException;
import jcl.compiler.icg.GeneratorState;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;

public abstract class LispStructImpl implements LispStruct {

	@Override
	public void generate(final GeneratorState generatorState) {
		throw new GenerationUnsupportedException();
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.T;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.CLASS_T;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.T) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.CLASS_T) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.ATOM) {
			return TStruct.INSTANCE;
		}
		return NILStruct.INSTANCE;
	}
}
