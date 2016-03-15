package jcl.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.classes.BuiltInClassStruct;
import jcl.compiler.environment.binding.lambdalist.AuxParameter;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.symbols.SymbolStruct;
import jcl.types.FunctionType;
import org.springframework.beans.factory.InitializingBean;

/**
 * The {@link FunctionStruct} is the object representation of a Lisp 'function' type.
 */
public abstract class FunctionStruct extends BuiltInClassStruct implements InitializingBean {

	protected OrdinaryLambdaList lambdaListBindings;

	protected FunctionStruct() {
		this(null, FunctionType.INSTANCE, null, null);
	}

	protected FunctionStruct(final String documentation) {
		this(documentation, FunctionType.INSTANCE, null, null);
	}

	protected FunctionStruct(final String documentation, final OrdinaryLambdaList lambdaListBindings) {
		this(documentation, FunctionType.INSTANCE, null, null);
		this.lambdaListBindings = lambdaListBindings;
	}

	protected FunctionStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(null, FunctionType.INSTANCE, directSuperClasses, subClasses);
	}

	protected FunctionStruct(final String documentation,
	                         final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(documentation, FunctionType.INSTANCE, directSuperClasses, subClasses);
	}

	protected FunctionStruct(final LispType type,
	                         final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		this(null, type, directSuperClasses, subClasses);
	}

	protected FunctionStruct(final String documentation, final LispType type,
	                         final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(documentation, type, directSuperClasses, subClasses);
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		final SymbolStruct functionSymbol = getFunctionSymbol();
		functionSymbol.setFunction(this);
	}

	private static final SymbolStruct DUMMY_SYMBOL = new SymbolStruct("dummySymbol");

	public SymbolStruct getFunctionSymbol() {
		// TODO: we can do this better
		return DUMMY_SYMBOL;
	}

	public abstract LispStruct apply(LispStruct... lispStructs);

	protected void initLambdaListBindings() {
		final List<RequiredParameter> requiredBindings = getRequiredBindings();
		final List<OptionalParameter> optionalBindings = getOptionalBindings();
		final RestParameter restBinding = getRestBinding();
		final List<KeyParameter> keyBindings = getKeyBindings();
		final boolean allowOtherKeys = getAllowOtherKeys();
		final List<AuxParameter> auxBindings = getAuxBindings();
		lambdaListBindings = OrdinaryLambdaList.builder()
		                                       .requiredBindings(requiredBindings)
		                                       .optionalBindings(optionalBindings)
		                                       .restBinding(restBinding)
		                                       .keyBindings(keyBindings)
		                                       .allowOtherKeys(allowOtherKeys)
		                                       .auxBindings(auxBindings)
		                                       .build();
	}

	protected List<RequiredParameter> getRequiredBindings() {
		return Collections.emptyList();
	}

	protected List<OptionalParameter> getOptionalBindings() {
		return Collections.emptyList();
	}

	protected RestParameter getRestBinding() {
		return null;
	}

	protected List<KeyParameter> getKeyBindings() {
		return Collections.emptyList();
	}

	protected boolean getAllowOtherKeys() {
		return false;
	}

	protected List<AuxParameter> getAuxBindings() {
		return Collections.emptyList();
	}
}
