package jcl.compiler.icg;

import java.util.Deque;

import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.LispStruct;

public class IntermediateCodeGeneratorImpl implements IntermediateCodeGenerator {

	@Override
	public Deque<JavaClassBuilder> generate(final LambdaStruct lambdaStruct) {
		final GeneratorState classBuilder = new GeneratorState();
		generate(lambdaStruct, classBuilder);
		return classBuilder.getFinalClassBuilderDeque();
	}

	@Override
	public void generate(final LispStruct input, final GeneratorState generatorState) {
		input.generate(generatorState);
	}
}
