package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;

public class LoadTimeValueCodeGenerator implements CodeGenerator<ConsElement> {

	public static final LoadTimeValueCodeGenerator INSTANCE = new LoadTimeValueCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		// This list looks like (load-time-value some-field-name)
		// all we have to do is get the value of the field
		codeGenerator.emitter.emitGetstatic(codeGenerator.classNames.peek(), input.getElements().getAllButFirst().getFirst().toString(), "Ljava/lang/Object;");
	}
}
