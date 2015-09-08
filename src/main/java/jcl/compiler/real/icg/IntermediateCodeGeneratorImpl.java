package jcl.compiler.real.icg;

import java.util.Deque;

import jcl.compiler.real.icg.generator.LambdaCodeGenerator;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class IntermediateCodeGeneratorImpl implements IntermediateCodeGenerator {

	@Autowired
	private LambdaCodeGenerator lambdaCodeGenerator;

	@Override
	public Deque<JavaClassBuilder> generate(final LambdaStruct lambdaStruct) {
		final GeneratorState classBuilder = new GeneratorState();
		lambdaCodeGenerator.generate(lambdaStruct, classBuilder);
		return classBuilder.getClasses();
	}
}
