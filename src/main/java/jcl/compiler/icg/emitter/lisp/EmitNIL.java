package jcl.compiler.icg.emitter.lisp;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EmitNIL extends BuiltInFunctionStructImpl {

	public EmitNIL() {
		super("Emitter function for the NIL constant",
		      EmitterSymbols.EMIT_NIL.getName(),
		      Parameters.forFunction(EmitterSymbols.EMIT_NIL.getName())
		                .requiredParameter("method-def")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		return methodDef.emitGetstatic(GenerationConstants.NIL_STRUCT_NAME,
		                               GenerationConstants.SINGLETON_INSTANCE,
		                               GenerationConstants.NIL_STRUCT_DESC);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.EMIT_NIL;
	}
}