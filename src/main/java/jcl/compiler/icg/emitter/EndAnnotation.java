package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EndAnnotation extends BuiltInFunctionStructImpl {

	public EndAnnotation() {
		super("Emitter function to finish generation for annotation",
		      EmitterSymbols.END_ANNOTATION.getName(),
		      Parameters.forFunction(EmitterSymbols.END_ANNOTATION.getName())
		                .requiredParameter("annotation-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final AnnotationDef annotationDef = arguments.getRequiredArgument("annotation-def", AnnotationDef.class);
		return annotationDef.endAnnotation();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.END_ANNOTATION;
	}
}