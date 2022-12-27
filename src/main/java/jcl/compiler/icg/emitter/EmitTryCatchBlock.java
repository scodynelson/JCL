package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EmitTryCatchBlock extends BuiltInFunctionStructImpl {

	public EmitTryCatchBlock() {
		super("Emitter function to start a new try/catch block",
		      EmitterSymbols.EMIT_TRY_CATCH_BLOCK.getName(),
		      Parameters.forFunction(EmitterSymbols.EMIT_TRY_CATCH_BLOCK.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("start")
		                .requiredParameter("end")
		                .requiredParameter("handler")
		                .requiredParameter("type")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final LabelWrapper start = arguments.getRequiredArgument("start", LabelWrapper.class);
		final LabelWrapper end = arguments.getRequiredArgument("end", LabelWrapper.class);
		final LabelWrapper handler = arguments.getRequiredArgument("handler", LabelWrapper.class);
		final StringStruct type = arguments.getRequiredArgument("label", StringStruct.class);
		return methodDef.emitTryCatchBlock(start.getLabel(), end.getLabel(), handler.getLabel(), type.toJavaString());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.EMIT_TRY_CATCH_BLOCK;
	}
}