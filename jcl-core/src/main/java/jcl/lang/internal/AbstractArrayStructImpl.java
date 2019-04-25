package jcl.lang.internal;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;

public abstract class AbstractArrayStructImpl extends LispStructImpl implements ArrayStruct {

	protected LispStruct elementType;

	protected AbstractArrayStructImpl(final LispStruct elementType) {
		this.elementType = elementType;
	}

	@Override
	public LispStruct arrayElementType() {
		return elementType;
	}

	@Override
	public LispStruct typeOf() {
		// TODO: Simple vs Not???
		return ListStruct.toLispList(CommonLispSymbols.SIMPLE_ARRAY, elementType, arrayDimensions());
	}

	@Override
	public ClassStruct classOf() {
		// TODO: Simple vs Not???
		return BuiltInClassStruct.SIMPLE_ARRAY;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		// TODO: Simple vs Not???
		if (typeSpecifier == CommonLispSymbols.SIMPLE_ARRAY) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SIMPLE_ARRAY) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
