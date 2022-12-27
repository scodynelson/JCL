package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Lushr extends BuiltInFunctionStructImpl {

	public Lushr() {
		super("Emitter function for Java opcode LUSHR",
		      EmitterSymbols.LUSHR.getName(),
		      Parameters.forFunction(EmitterSymbols.LUSHR.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitLushr();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.LUSHR;
	}
}