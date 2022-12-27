package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Instanceof extends BuiltInFunctionStructImpl {

	public Instanceof() {
		super("Emitter function for Java opcode INSTANCEOF",
		      EmitterSymbols.INSTANCEOF.getName(),
		      Parameters.forFunction(EmitterSymbols.INSTANCEOF.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("type")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final StringStruct type = arguments.getRequiredArgument("type", StringStruct.class);
		return methodDef.emitInstanceof(type.toJavaString());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.INSTANCEOF;
	}
}