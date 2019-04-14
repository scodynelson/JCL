package jcl.lang.function;

import java.util.List;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.type.FunctionType;
import jcl.type.LispType;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public abstract class FunctionStructImpl extends BuiltInClassStruct implements FunctionStruct {

	protected FunctionStructImpl(final String documentation) {
		this(documentation, FunctionType.INSTANCE);
	}

	protected FunctionStructImpl(final String documentation, final FunctionType type) {
		this(documentation, type, null, null);
	}

	protected FunctionStructImpl(final String documentation, final LispType type,
	                             final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(documentation, type, directSuperClasses, subClasses);
	}
}
