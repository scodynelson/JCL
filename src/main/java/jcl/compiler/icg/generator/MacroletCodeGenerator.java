package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;

//@Component
class MacroletCodeGenerator implements CodeGenerator<InnerLambdaStruct> {

	//	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final InnerLambdaStruct input, final GeneratorState generatorState) {

		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, generatorState);
	}
}
