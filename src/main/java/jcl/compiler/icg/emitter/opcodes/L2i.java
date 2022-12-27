package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class L2i extends BuiltInFunctionStructImpl {

	public L2i() {
		super("Emitter function for Java opcode L2I",
		      EmitterSymbols.L2I.getName(),
		      Parameters.forFunction(EmitterSymbols.L2I.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitL2i();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.L2I;
	}
}