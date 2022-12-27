package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.LabelWrapper;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Ifge extends BuiltInFunctionStructImpl {

	public Ifge() {
		super("Emitter function for Java opcode IFGE",
		      EmitterSymbols.IFGE.getName(),
		      Parameters.forFunction(EmitterSymbols.IFGE.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("label")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final LabelWrapper label = arguments.getRequiredArgument("label", LabelWrapper.class);
		return methodDef.emitIfge(label.getLabel());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.IFGE;
	}
}