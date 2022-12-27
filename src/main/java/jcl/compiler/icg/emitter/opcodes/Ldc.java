package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Ldc extends BuiltInFunctionStructImpl {

	public Ldc() {
		super("Emitter function for Java opcode LDC",
		      EmitterSymbols.LDC.getName(),
		      Parameters.forFunction(EmitterSymbols.LDC.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("constant")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final LispStruct constant = arguments.getRequiredArgument("constant", LispStruct.class);
		return methodDef.emitLdc(constant);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.LDC;
	}
}