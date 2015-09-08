package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.TheStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TheCodeGenerator implements CodeGenerator<TheStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final TheStruct input, final GeneratorState generatorState) {
		// TODO: do we want to add the logic here to verify the type information of the generated form???

		final LispStruct form = input.getForm();
		formGenerator.generate(form, generatorState);
	}
}
