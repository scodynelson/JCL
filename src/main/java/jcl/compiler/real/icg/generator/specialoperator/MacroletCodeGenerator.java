package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.MacroletStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;

//@Component
public class MacroletCodeGenerator implements CodeGenerator<MacroletStruct> {

	//	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final MacroletStruct input, final JavaClassBuilder classBuilder) {

		final PrognStruct forms = input.getForms();
		prognCodeGenerator.generate(forms, classBuilder);
	}
}
