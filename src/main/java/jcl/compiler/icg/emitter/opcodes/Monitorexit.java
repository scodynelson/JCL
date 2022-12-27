package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Monitorexit extends BuiltInFunctionStructImpl {

	public Monitorexit() {
		super("Emitter function for Java opcode MONITOREXIT",
		      EmitterSymbols.MONITOREXIT.getName(),
		      Parameters.forFunction(EmitterSymbols.MONITOREXIT.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitMonitorexit();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.MONITOREXIT;
	}
}