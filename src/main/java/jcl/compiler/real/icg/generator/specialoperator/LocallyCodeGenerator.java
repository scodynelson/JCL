package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LocallyEnvironment;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.LocallyStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LocallyCodeGenerator implements CodeGenerator<LocallyStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final LocallyStruct input, final JavaClassBuilder classBuilder) {

		final PrognStruct forms = input.getForms();
		final LocallyEnvironment locallyEnvironment = input.getLocallyEnvironment();

		final Stack<Environment> bindingStack = classBuilder.getBindingStack();

		bindingStack.push(locallyEnvironment);
		prognCodeGenerator.generate(forms, classBuilder);
		bindingStack.pop();
	}
}
