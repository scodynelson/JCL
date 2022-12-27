package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Lshr extends BuiltInFunctionStructImpl {

	public Lshr() {
		super("Emitter function for Java opcode LSHR",
		      EmitterSymbols.LSHR.getName(),
		      Parameters.forFunction(EmitterSymbols.LSHR.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitLshr();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.LSHR;
	}
}