package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Fcmpl extends BuiltInFunctionStructImpl {

	public Fcmpl() {
		super("Emitter function for Java opcode FCMPL",
		      EmitterSymbols.FCMPL.getName(),
		      Parameters.forFunction(EmitterSymbols.FCMPL.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitFcmpl();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.FCMPL;
	}
}