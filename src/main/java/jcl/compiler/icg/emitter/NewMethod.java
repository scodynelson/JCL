package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class NewMethod extends BuiltInFunctionStructImpl {

	public NewMethod() {
		super("Emitter function for starting a new method for generation",
		      EmitterSymbols.NEW_METHOD.getName(),
		      Parameters.forFunction(EmitterSymbols.NEW_METHOD.getName())
		                .requiredParameter("class-def")
		                .requiredParameter("access")
		                .requiredParameter("name")
		                .requiredParameter("descriptor")
		                .requiredParameter("signature")
		                .requiredParameter("exceptions")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ClassDef classDef = arguments.getRequiredArgument("class-def", ClassDef.class);
		final IntegerStruct access = arguments.getRequiredArgument("access", IntegerStruct.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		final StringStruct signature = arguments.getRequiredArgument("signature", StringStruct.class);
		final ListStruct exceptions = arguments.getRequiredArgument("exceptions", ListStruct.class);

		final String[] exceptionsArray
				= exceptions.stream()
				            .map(StringStruct.class::cast)
				            .map(StringStruct::toJavaString)
				            .toArray(value -> new String[exceptions.length().toJavaInt()]);

		return classDef.newMethod(access.toJavaInt(), name.toJavaString(), descriptor.toJavaString(),
		                          signature.toJavaString(), exceptionsArray);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.NEW_METHOD;
	}
}