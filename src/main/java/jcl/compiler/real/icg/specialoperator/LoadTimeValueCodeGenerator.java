package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;

public class LoadTimeValueCodeGenerator implements CodeGenerator<ListStruct> {

	public static final LoadTimeValueCodeGenerator INSTANCE = new LoadTimeValueCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator) {
		// This list looks like (load-time-value some-field-name)
		// all we have to do is get the value of the field
		codeGenerator.emitter.emitGetstatic(codeGenerator.classNames.peek(), input.getRest().getFirst().toString(), "Ljava/lang/Object;");
	}
}
