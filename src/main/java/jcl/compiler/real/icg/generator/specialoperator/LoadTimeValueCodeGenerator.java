package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public class LoadTimeValueCodeGenerator implements CodeGenerator<ListStruct> {

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		// This list looks like (load-time-value some-field-name)
		// all we have to do is get the value of the field
		classBuilder.getEmitter().emitGetstatic(classBuilder.getClassNames().peek(), input.getRest().getFirst().toString(), "Ljava/lang/Object;");
	}
}