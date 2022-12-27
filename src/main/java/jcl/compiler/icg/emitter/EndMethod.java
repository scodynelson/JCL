package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EndMethod extends BuiltInFunctionStructImpl {

	public EndMethod() {
		super("Emitter function to finish generation for method",
		      EmitterSymbols.END_METHOD.getName(),
		      Parameters.forFunction(EmitterSymbols.END_METHOD.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.endMethod();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.END_METHOD;
	}
}