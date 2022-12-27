package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Pop2 extends BuiltInFunctionStructImpl {

	public Pop2() {
		super("Emitter function for Java opcode POP2",
		      EmitterSymbols.POP2.getName(),
		      Parameters.forFunction(EmitterSymbols.POP2.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitPop2();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.POP2;
	}
}