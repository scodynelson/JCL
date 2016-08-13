package jcl.lang.function;

import java.util.List;

import jcl.lang.internal.BuiltInClassStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.type.FunctionType;
import jcl.type.LispType;
import org.springframework.beans.factory.InitializingBean;

/**
 * The {@link FunctionStructImpl} is the object representation of a Lisp 'function' type.
 */
public abstract class FunctionStructImpl extends BuiltInClassStruct implements InitializingBean {

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

	@Override
	public void afterPropertiesSet() throws Exception {
		final SymbolStruct functionSymbol = getFunctionSymbol();
		functionSymbol.setFunction(this);
	}

	public abstract SymbolStruct getFunctionSymbol();

	public abstract LispStruct apply(LispStruct... lispStructs);
}
