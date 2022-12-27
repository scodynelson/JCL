package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class D2f extends BuiltInFunctionStructImpl {

	public D2f() {
		super("Emitter function for Java opcode D2F",
		      EmitterSymbols.D2F.getName(),
		      Parameters.forFunction(EmitterSymbols.D2F.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitD2f();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.D2F;
	}
}