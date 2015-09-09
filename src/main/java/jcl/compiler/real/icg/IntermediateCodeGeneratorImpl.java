package jcl.compiler.real.icg;

import java.util.Deque;
import java.util.Map;
import javax.annotation.Resource;

import jcl.LispStruct;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
class IntermediateCodeGeneratorImpl implements IntermediateCodeGenerator {

	@Resource
	private Map<Class<? extends LispStruct>, CodeGenerator<LispStruct>> codeGeneratorStrategies;

	@Override
	public Deque<JavaClassBuilder> generate(final LambdaStruct lambdaStruct) {
		final GeneratorState classBuilder = new GeneratorState();
		generate(lambdaStruct, classBuilder);
		return classBuilder.getClasses();
	}

	@Override
	public void generate(final LispStruct input, final GeneratorState generatorState) {

		final CodeGenerator<LispStruct> codeGenerator = codeGeneratorStrategies.get(input.getClass());
		if (codeGenerator == null) {
			throw new ProgramErrorException("ICG: Found thing I can't generate code for class: " + input.getClass().getName());
		}
		codeGenerator.generate(input, generatorState);
	}
}
