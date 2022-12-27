package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Lconst extends BuiltInFunctionStructImpl {

	public Lconst() {
		super("Emitter function for Java opcode LCONST",
		      EmitterSymbols.LCONST.getName(),
		      Parameters.forFunction(EmitterSymbols.LCONST.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("constant")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final IntegerStruct constant = arguments.getRequiredArgument("constant", IntegerStruct.class);
		return methodDef.emitLconst(constant.toJavaInt());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.LCONST;
	}
}