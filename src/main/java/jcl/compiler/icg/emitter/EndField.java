package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EndField extends BuiltInFunctionStructImpl {

	public EndField() {
		super("Emitter function to finish generation for field",
		      EmitterSymbols.END_FIELD.getName(),
		      Parameters.forFunction(EmitterSymbols.END_FIELD.getName())
		                .requiredParameter("field-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FieldDef fieldDef = arguments.getRequiredArgument("field-def", FieldDef.class);
		return fieldDef.endField();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.END_FIELD;
	}
}