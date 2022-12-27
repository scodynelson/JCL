package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class AddInnerClassReference extends BuiltInFunctionStructImpl {

	public AddInnerClassReference() {
		super("Emitter function for adding a new inner class reference",
		      EmitterSymbols.ADD_INNER_CLASS_REF.getName(),
		      Parameters.forFunction(EmitterSymbols.ADD_INNER_CLASS_REF.getName())
		                .requiredParameter("class-def")
		                .requiredParameter("name")
		                .requiredParameter("outer-name")
		                .requiredParameter("inner-name")
		                .requiredParameter("access")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ClassDef classDef = arguments.getRequiredArgument("class-def", ClassDef.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct outerName = arguments.getRequiredArgument("outer-name", StringStruct.class);
		final StringStruct innerName = arguments.getRequiredArgument("inner-name", StringStruct.class);
		final IntegerStruct access = arguments.getRequiredArgument("access", IntegerStruct.class);
		return classDef.addInnerClassReference(name.toJavaString(), outerName.toJavaString(), innerName.toJavaString(),
		                                       access.toJavaInt());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.ADD_INNER_CLASS_REF;
	}
}