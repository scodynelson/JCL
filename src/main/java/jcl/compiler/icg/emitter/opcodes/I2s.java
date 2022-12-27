package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class I2s extends BuiltInFunctionStructImpl {

	public I2s() {
		super("Emitter function for Java opcode I2S",
		      EmitterSymbols.I2S.getName(),
		      Parameters.forFunction(EmitterSymbols.I2S.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitI2s();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.I2S;
	}
}