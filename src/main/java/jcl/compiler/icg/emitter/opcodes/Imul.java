package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Imul extends BuiltInFunctionStructImpl {

	public Imul() {
		super("Emitter function for Java opcode IMUL",
		      EmitterSymbols.IMUL.getName(),
		      Parameters.forFunction(EmitterSymbols.IMUL.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitImul();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.IMUL;
	}
}