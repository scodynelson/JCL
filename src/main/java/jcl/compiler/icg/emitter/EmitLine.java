package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class EmitLine extends BuiltInFunctionStructImpl {

	public EmitLine() {
		super("Emitter function to emit a line",
		      EmitterSymbols.EMIT_LINE.getName(),
		      Parameters.forFunction(EmitterSymbols.EMIT_LINE.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("line")
		                .requiredParameter("start")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final IntegerStruct line = arguments.getRequiredArgument("line", IntegerStruct.class);
		final LabelWrapper start = arguments.getRequiredArgument("start", LabelWrapper.class);
		return methodDef.emitLine(line.toJavaInt(), start.getLabel());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.EMIT_LINE;
	}
}