package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Getfield extends BuiltInFunctionStructImpl {

	public Getfield() {
		super("Emitter function for Java opcode GETFIELD",
		      EmitterSymbols.GETFIELD.getName(),
		      Parameters.forFunction(EmitterSymbols.GETFIELD.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("owner")
		                .requiredParameter("name")
		                .requiredParameter("descriptor")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final StringStruct owner = arguments.getRequiredArgument("owner", StringStruct.class);
		final StringStruct name = arguments.getRequiredArgument("name", StringStruct.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		return methodDef.emitGetfield(owner.toJavaString(), name.toJavaString(), descriptor.toJavaString());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.GETFIELD;
	}
}