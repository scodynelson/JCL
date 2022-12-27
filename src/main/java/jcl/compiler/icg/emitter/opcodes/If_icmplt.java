package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.LabelWrapper;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class If_icmplt extends BuiltInFunctionStructImpl {

	public If_icmplt() {
		super("Emitter function for Java opcode IF_ICMPLT",
		      EmitterSymbols.IF_ICMPLT.getName(),
		      Parameters.forFunction(EmitterSymbols.IF_ICMPLT.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("label")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final LabelWrapper label = arguments.getRequiredArgument("label", LabelWrapper.class);
		return methodDef.emitIf_icmplt(label.getLabel());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.IF_ICMPLT;
	}
}