package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class NewClassAnnotation extends BuiltInFunctionStructImpl {

	public NewClassAnnotation() {
		super("Emitter function for starting a new class annotation for generation",
		      EmitterSymbols.NEW_CLASS_ANNOTATION.getName(),
		      Parameters.forFunction(EmitterSymbols.NEW_CLASS_ANNOTATION.getName())
		                .requiredParameter("class-def")
		                .requiredParameter("name")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ClassDef classDef = arguments.getRequiredArgument("class-def", ClassDef.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		return classDef.newAnnotation(name.toJavaString());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.NEW_CLASS_ANNOTATION;
	}
}