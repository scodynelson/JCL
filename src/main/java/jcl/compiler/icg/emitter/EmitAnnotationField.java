package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EmitAnnotationField extends BuiltInFunctionStructImpl {

	public EmitAnnotationField() {
		super("Emitter function to emit a new annotation field",
		      EmitterSymbols.EMIT_ANNOTATION_FIELD.getName(),
		      Parameters.forFunction(EmitterSymbols.EMIT_ANNOTATION_FIELD.getName())
		                .requiredParameter("annotation-def")
		                .requiredParameter("name")
		                .requiredParameter("value")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final AnnotationDef annotationDef = arguments.getRequiredArgument("annotation-def", AnnotationDef.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final LispStruct value = arguments.getRequiredArgument("value", LispStruct.class);
		return annotationDef.emitAnnotationField(name.toJavaString(), value);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.EMIT_ANNOTATION_FIELD;
	}
}