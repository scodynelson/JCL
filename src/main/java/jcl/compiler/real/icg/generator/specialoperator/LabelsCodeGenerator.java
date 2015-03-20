package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LabelsCodeGenerator implements CodeGenerator<PrognStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final PrognStruct input, final JavaClassBuilder classBuilder) {
		prognCodeGenerator.generate(input, classBuilder);
	}
}
