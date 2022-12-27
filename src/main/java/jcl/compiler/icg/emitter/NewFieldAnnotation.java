package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class NewFieldAnnotation extends BuiltInFunctionStructImpl {

	public NewFieldAnnotation() {
		super("Emitter function for starting a new field annotation for generation",
		      EmitterSymbols.NEW_FIELD_ANNOTATION.getName(),
		      Parameters.forFunction(EmitterSymbols.NEW_FIELD_ANNOTATION.getName())
		                .requiredParameter("field-def")
		                .requiredParameter("name")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FieldDef fieldDef = arguments.getRequiredArgument("field-def", FieldDef.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		return fieldDef.newAnnotation(name.toJavaString());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.NEW_FIELD_ANNOTATION;
	}
}