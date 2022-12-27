package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EndClass extends BuiltInFunctionStructImpl {

	public EndClass() {
		super("Emitter function to finish generation for class",
		      EmitterSymbols.END_CLASS.getName(),
		      Parameters.forFunction(EmitterSymbols.END_CLASS.getName())
		                .requiredParameter("class-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ClassDef classDef = arguments.getRequiredArgument("class-def", ClassDef.class);
		return classDef.endClass();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.END_CLASS;
	}
}