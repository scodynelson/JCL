package jcl.lang.internal;

import jcl.lang.BitVectorStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;

public abstract class AbstractBitVectorStructImpl extends AbstractVectorStructImpl implements BitVectorStruct {

	protected AbstractBitVectorStructImpl(final Integer totalSize) {
		super(CommonLispSymbols.BIT, totalSize);
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.BIT_VECTOR;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.BIT_VECTOR) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.BIT_VECTOR) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
