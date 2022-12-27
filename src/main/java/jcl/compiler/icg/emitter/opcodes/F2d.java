package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class F2d extends BuiltInFunctionStructImpl {

	public F2d() {
		super("Emitter function for Java opcode F2D",
		      EmitterSymbols.F2D.getName(),
		      Parameters.forFunction(EmitterSymbols.F2D.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitF2d();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.F2D;
	}
}