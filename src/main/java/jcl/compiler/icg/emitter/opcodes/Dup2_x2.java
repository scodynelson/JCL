package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Dup2_x2 extends BuiltInFunctionStructImpl {

	public Dup2_x2() {
		super("Emitter function for Java opcode DUP2_X2",
		      EmitterSymbols.DUP2_X2.getName(),
		      Parameters.forFunction(EmitterSymbols.DUP2_X2.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitDup2_x2();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.DUP2_X2;
	}
}