package jcl.compiler.icg;

import java.util.Deque;

import jcl.compiler.struct.specialoperator.lambda.LambdaStruct;
import jcl.lang.LispStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;

@Component
class IntermediateCodeGeneratorImpl implements IntermediateCodeGenerator {

	private final ApplicationEventPublisher eventPublisher;

	@Autowired
	IntermediateCodeGeneratorImpl(final ApplicationEventPublisher eventPublisher) {
		this.eventPublisher = eventPublisher;
	}

	@Override
	public Deque<JavaClassBuilder> generate(final LambdaStruct lambdaStruct) {
		final GeneratorState classBuilder = new GeneratorState();
		generate(lambdaStruct, classBuilder);
		return classBuilder.getFinalClassBuilderDeque();
	}

	@Override
	public void generate(final LispStruct input, final GeneratorState generatorState) {
		eventPublisher.publishEvent(GeneratorEvent.of(input, generatorState));
	}
}
