package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class NewMethodAnnotation extends BuiltInFunctionStructImpl {

	public NewMethodAnnotation() {
		super("Emitter function for starting a new method annotation for generation",
		      EmitterSymbols.NEW_METHOD_ANNOTATION.getName(),
		      Parameters.forFunction(EmitterSymbols.NEW_METHOD_ANNOTATION.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("name")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		return methodDef.newAnnotation(name.toJavaString());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.NEW_METHOD_ANNOTATION;
	}
}