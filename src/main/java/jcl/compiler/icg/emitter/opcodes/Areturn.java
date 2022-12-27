package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Areturn extends BuiltInFunctionStructImpl {

	public Areturn() {
		super("Emitter function for Java opcode ARETURN",
		      EmitterSymbols.ARETURN.getName(),
		      Parameters.forFunction(EmitterSymbols.ARETURN.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitAreturn();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.ARETURN;
	}
}