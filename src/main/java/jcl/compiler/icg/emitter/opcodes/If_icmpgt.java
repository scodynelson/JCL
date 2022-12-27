package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.LabelWrapper;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class If_icmpgt extends BuiltInFunctionStructImpl {

	public If_icmpgt() {
		super("Emitter function for Java opcode IF_ICMPGT",
		      EmitterSymbols.IF_ICMPGT.getName(),
		      Parameters.forFunction(EmitterSymbols.IF_ICMPGT.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("label")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final LabelWrapper label = arguments.getRequiredArgument("label", LabelWrapper.class);
		return methodDef.emitIf_icmpgt(label.getLabel());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.IF_ICMPGT;
	}
}