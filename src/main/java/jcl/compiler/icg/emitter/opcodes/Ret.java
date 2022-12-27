package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Ret extends BuiltInFunctionStructImpl {

	public Ret() {
		super("Emitter function for Java opcode RET",
		      EmitterSymbols.RET.getName(),
		      Parameters.forFunction(EmitterSymbols.RET.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("var-index")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final IntegerStruct varIndex = arguments.getRequiredArgument("var-index", IntegerStruct.class);
		return methodDef.emitRet(varIndex.toJavaInt());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.RET;
	}
}