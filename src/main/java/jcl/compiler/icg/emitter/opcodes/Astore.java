package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Astore extends BuiltInFunctionStructImpl {

	public Astore() {
		super("Emitter function for Java opcode ASTORE",
		      EmitterSymbols.ASTORE.getName(),
		      Parameters.forFunction(EmitterSymbols.ASTORE.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("var-index")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final IntegerStruct varIndex = arguments.getRequiredArgument("var-index", IntegerStruct.class);
		return methodDef.emitAstore(varIndex.toJavaInt());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.ASTORE;
	}
}