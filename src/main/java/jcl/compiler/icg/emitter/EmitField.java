package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EmitField extends BuiltInFunctionStructImpl {

	public EmitField() {
		super("Emitter function to emit a new field",
		      EmitterSymbols.EMIT_FIELD.getName(),
		      Parameters.forFunction(EmitterSymbols.EMIT_FIELD.getName())
		                .requiredParameter("class-def")
		                .requiredParameter("access")
		                .requiredParameter("name")
		                .requiredParameter("descriptor")
		                .requiredParameter("signature")
		                .requiredParameter("value")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ClassDef classDef = arguments.getRequiredArgument("class-def", ClassDef.class);
		final IntegerStruct access = arguments.getRequiredArgument("access", IntegerStruct.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		final StringStruct signature = arguments.getRequiredArgument("signature", StringStruct.class);
		final LispStruct value = arguments.getRequiredArgument("value", LispStruct.class);
		return classDef.newField(access.toJavaInt(), name.toJavaString(), descriptor.toJavaString(),
		                         signature.toJavaString(), value);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.EMIT_FIELD;
	}
}