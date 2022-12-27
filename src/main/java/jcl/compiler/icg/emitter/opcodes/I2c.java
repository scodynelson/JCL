package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class I2c extends BuiltInFunctionStructImpl {

	public I2c() {
		super("Emitter function for Java opcode I2C",
		      EmitterSymbols.I2C.getName(),
		      Parameters.forFunction(EmitterSymbols.I2C.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitI2c();
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.I2C;
	}
}