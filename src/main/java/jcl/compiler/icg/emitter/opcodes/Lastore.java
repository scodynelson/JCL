package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Lastore extends BuiltInFunctionStructImpl {

	public Lastore() {
		super("Emitter function for Java opcode LASTORE",
		      EmitterSymbols.LASTORE.getName(),
		      Parameters.forFunction(EmitterSymbols.LASTORE.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitLastore();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.LASTORE;
	}
}