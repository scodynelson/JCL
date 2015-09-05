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
	public Deque<ClassDef> generate(final LambdaStruct lambdaStruct) {
		final JavaClassBuilder classBuilder = new JavaClassBuilder();
		lambdaCodeGenerator.generate(lambdaStruct, classBuilder);
		return classBuilder.getClasses();
	}
}
