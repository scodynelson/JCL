package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Ineg extends BuiltInFunctionStructImpl {

	public Ineg() {
		super("Emitter function for Java opcode INEG",
		      EmitterSymbols.INEG.getName(),
		      Parameters.forFunction(EmitterSymbols.INEG.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitIneg();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.INEG;
	}
}