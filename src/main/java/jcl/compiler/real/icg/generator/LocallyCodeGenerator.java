package jcl.compiler.real.icg.generator;

import java.util.Stack;

import jcl.compiler.real.environment.Environment;
import jcl.compiler.real.environment.LocallyEnvironment;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.struct.specialoperator.LocallyStruct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class LocallyCodeGenerator implements CodeGenerator<LocallyStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final LocallyStruct input, final GeneratorState generatorState) {

		final PrognStruct forms = input.getForms();
		final LocallyEnvironment locallyEnvironment = input.getLocallyEnvironment();

		final Stack<Environment> bindingStack = generatorState.getBindingStack();

		bindingStack.push(locallyEnvironment);
		prognCodeGenerator.generate(forms, generatorState);
		bindingStack.pop();
	}
}
