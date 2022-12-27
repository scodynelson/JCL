package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.LabelWrapper;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

public final class Iflt extends BuiltInFunctionStructImpl {

	public Iflt() {
		super("Emitter function for Java opcode IFLT",
		      EmitterSymbols.IFLT.getName(),
		      Parameters.forFunction(EmitterSymbols.IFLT.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("label")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final LabelWrapper label = arguments.getRequiredArgument("label", LabelWrapper.class);
		return methodDef.emitIflt(label.getLabel());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.IFLT;
	}
}