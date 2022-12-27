package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class AddOuterClassReference extends BuiltInFunctionStructImpl {

	public AddOuterClassReference() {
		super("Emitter function for adding a new outer class reference",
		      EmitterSymbols.ADD_OUTER_CLASS_REF.getName(),
		      Parameters.forFunction(EmitterSymbols.ADD_OUTER_CLASS_REF.getName())
		                .requiredParameter("class-def")
		                .requiredParameter("owner")
		                .requiredParameter("name")
		                .requiredParameter("descriptor")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ClassDef classDef = arguments.getRequiredArgument("class-def", ClassDef.class);
		final StringStruct owner = arguments.getRequiredArgument("owner", StringStruct.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		return classDef.addOuterClassReference(owner.toJavaString(), name.toJavaString(), descriptor.toJavaString());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.ADD_OUTER_CLASS_REF;
	}
}