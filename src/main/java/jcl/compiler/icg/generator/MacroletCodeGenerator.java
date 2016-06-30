package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.struct.specialoperator.InnerLambdaStruct;
import jcl.compiler.struct.specialoperator.PrognStruct;

//@Component
final class MacroletCodeGenerator implements CodeGenerator<InnerLambdaStruct> {

	//	@Autowired
	private IntermediateCodeGenerator codeGenerator;

//	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<InnerLambdaStruct> event) {
		final InnerLambdaStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();

		final PrognStruct forms = input.getForms();
		codeGenerator.generate(forms, generatorState);
	}
}
