package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Ixor extends BuiltInFunctionStructImpl {

	public Ixor() {
		super("Emitter function for Java opcode IXOR",
		      EmitterSymbols.IXOR.getName(),
		      Parameters.forFunction(EmitterSymbols.IXOR.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitIxor();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.IXOR;
	}
}