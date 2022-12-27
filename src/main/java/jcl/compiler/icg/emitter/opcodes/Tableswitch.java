package jcl.compiler.icg.emitter.opcodes;

import jcl.compiler.icg.emitter.EmitterSymbols;
import jcl.compiler.icg.emitter.LabelWrapper;
import jcl.compiler.icg.emitter.MethodDef;
import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.objectweb.asm.Label;

public final class Tableswitch extends BuiltInFunctionStructImpl {

	public Tableswitch() {
		super("Emitter function for Java opcode TABLESWITCH",
		      EmitterSymbols.TABLESWITCH.getName(),
		      Parameters.forFunction(EmitterSymbols.TABLESWITCH.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("min")
		                .requiredParameter("max")
		                .requiredParameter("default-label")
		                .requiredParameter("handler-labels")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final IntegerStruct min = arguments.getRequiredArgument("min", IntegerStruct.class);
		final IntegerStruct max = arguments.getRequiredArgument("max", IntegerStruct.class);
		final LabelWrapper defaultLabel = arguments.getRequiredArgument("default-label", LabelWrapper.class);

		final ListStruct handlerLabels = arguments.getRequiredArgument("handler-labels", ListStruct.class);
		final Label[] labels
				= handlerLabels.stream()
				               .map(LabelWrapper.class::cast)
				               .map(LabelWrapper::getLabel)
				               .toArray(value -> new Label[handlerLabels.length().toJavaInt()]);

		return methodDef.emitTableswitch(min.toJavaInt(), max.toJavaInt(), defaultLabel.getLabel(), labels);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.TABLESWITCH;
	}
}