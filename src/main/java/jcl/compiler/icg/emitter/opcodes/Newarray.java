package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Newarray extends BuiltInFunctionStructImpl {

	public Newarray() {
		super("Emitter function for Java opcode NEWARRAY",
		      EmitterSymbols.NEWARRAY.getName(),
		      Parameters.forFunction(EmitterSymbols.NEWARRAY.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("operand")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final IntegerStruct operand = arguments.getRequiredArgument("operand", IntegerStruct.class);
		return methodDef.emitNewarray(operand.toJavaInt());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.NEWARRAY;
	}
}