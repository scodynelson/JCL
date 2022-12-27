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

public final class Lookupswitch extends BuiltInFunctionStructImpl {

	public Lookupswitch() {
		super("Emitter function for Java opcode LOOKUPSWITCH",
		      EmitterSymbols.LOOKUPSWITCH.getName(),
		      Parameters.forFunction(EmitterSymbols.LOOKUPSWITCH.getName())
		                .requiredParameter("method-def")
		                .requiredParameter("default-label")
		                .requiredParameter("keys")
		                .requiredParameter("labels")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final MethodDef methodDef = arguments.getRequiredArgument("method-def", MethodDef.class);
		final LabelWrapper defaultLabel = arguments.getRequiredArgument("default-label", LabelWrapper.class);

		final ListStruct keys = arguments.getRequiredArgument("keys", ListStruct.class);
		final int[] keysArray
				= keys.stream()
				      .map(IntegerStruct.class::cast)
				      .mapToInt(IntegerStruct::toJavaInt)
				      .toArray();

		final ListStruct labels = arguments.getRequiredArgument("labels", ListStruct.class);
		final Label[] labelsArray
				= labels.stream()
				        .map(LabelWrapper.class::cast)
				        .map(LabelWrapper::getLabel)
				        .toArray(value -> new Label[labels.length().toJavaInt()]);

		return methodDef.emitLookupswitch(defaultLabel.getLabel(), keysArray, labelsArray);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.LOOKUPSWITCH;
	}
}