package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Multianewarray extends BuiltInFunctionStructImpl {

	public Multianewarray() {
		super("Emitter function for Java opcode MULTIANEWARRAY",
		      EmitterSymbols.MULTIANEWARRAY.getName(),
		      Parameters.forFunction(EmitterSymbols.MULTIANEWARRAY.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("descriptor")
		                .requiredParameter("num-dimensions")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final StringStruct descriptor = arguments.getRequiredArgument("descriptor", StringStruct.class);
		final IntegerStruct numDimensions = arguments.getRequiredArgument("num-dimensions", IntegerStruct.class);
		return methodDef.emitMultianewarray(descriptor.toJavaString(), numDimensions.toJavaInt());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.MULTIANEWARRAY;
	}
}