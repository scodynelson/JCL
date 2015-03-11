package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import org.springframework.stereotype.Component;

@Component
public class TheCodeGenerator implements CodeGenerator<ListStruct> {

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		//TODO unimplemented 'the'
	}
}
