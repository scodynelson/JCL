package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;

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
