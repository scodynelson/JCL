package jcl.lang.internal;

import jcl.compiler.GenerationUnsupportedException;
import jcl.compiler.icg.GeneratorState;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;

public abstract class LispStructImpl implements LispStruct {

	private String documentation;

	@Override
	public void generate(final GeneratorState generatorState) {
		throw new GenerationUnsupportedException();
	}

	@Override
	public LispStruct documentation() {
		if (documentation == null) {
			return NILStruct.INSTANCE;
		}
		return StringStruct.toLispString(documentation);
	}

	@Override
	public LispStruct setDocumentation(final StringStruct docString) {
		documentation = docString.toJavaString();
		return docString;
	}

	@Override
	public final LispStruct setDocumentation(final String docString) {
		documentation = docString;
		return StringStruct.toLispString(documentation);
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

	@Override
	public String toString() {
		return "#<" + typeOf() + '>';
	}
}
