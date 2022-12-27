package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.LabelWrapper;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class If_icmpge extends BuiltInFunctionStructImpl {

	public If_icmpge() {
		super("Emitter function for Java opcode IF_ICMPGE",
		      EmitterSymbols.IF_ICMPGE.getName(),
		      Parameters.forFunction(EmitterSymbols.IF_ICMPGE.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("label")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final LabelWrapper label = arguments.getRequiredArgument("label", LabelWrapper.class);
		return methodDef.emitIf_icmpge(label.getLabel());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.IF_ICMPGE;
	}
}