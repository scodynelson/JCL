package jcl.functions;

import jcl.lang.LispStruct;
import jcl.lang.function.FunctionStructImpl;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public abstract class BuiltInFunctionStructImpl extends FunctionStructImpl {

	protected final String functionName;
	private final Parameters parameters;

	protected BuiltInFunctionStructImpl(final String documentation, final String functionName, final Parameters parameters) {
		super(documentation);
		this.functionName = functionName;
		this.parameters = parameters;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final Arguments arguments = parameters.build(lispStructs);
		return apply(arguments);
	}

	public abstract LispStruct apply(Arguments arguments);

	public Parameters getParameters() {
		return parameters;
	}
}
