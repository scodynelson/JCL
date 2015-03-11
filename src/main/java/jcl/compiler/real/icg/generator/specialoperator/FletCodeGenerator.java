package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FletCodeGenerator implements CodeGenerator<ListStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		prognCodeGenerator.generate(input, codeGenerator, classBuilder);
	}
}
