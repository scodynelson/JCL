package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Iinc extends BuiltInFunctionStructImpl {

	public Iinc() {
		super("Emitter function for Java opcode IINC",
		      EmitterSymbols.IINC.getName(),
		      Parameters.forFunction(EmitterSymbols.IINC.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("var-index")
		                .requiredParameter("increment")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final IntegerStruct varIndex = arguments.getRequiredArgument("var-index", IntegerStruct.class);
		final IntegerStruct increment = arguments.getRequiredArgument("increment", IntegerStruct.class);
		return methodDef.emitIinc(varIndex.toJavaInt(), increment.toJavaInt());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.IINC;
	}
}