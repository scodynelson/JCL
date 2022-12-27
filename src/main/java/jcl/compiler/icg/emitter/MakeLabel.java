package jcl.compiler.icg.emitter;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.objectweb.asm.Label;

public final class MakeLabel extends BuiltInFunctionStructImpl {

	public MakeLabel() {
		super("Emitter function to make a new label",
		      EmitterSymbols.MAKE_LABEL.getName(),
		      Parameters.forFunction(EmitterSymbols.MAKE_LABEL.getName())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		return new LabelWrapper(new Label());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return EmitterSymbols.MAKE_LABEL;
	}
}