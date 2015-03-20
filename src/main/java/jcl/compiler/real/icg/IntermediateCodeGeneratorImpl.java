package jcl.compiler.real.icg;

import java.util.List;

import jcl.compiler.real.icg.generator.specialoperator.lambda.NewLambdaCodeGenerator;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class IntermediateCodeGeneratorImpl implements IntermediateCodeGenerator {

	@Autowired
	private NewLambdaCodeGenerator lambdaCodeGenerator;

	@Override
	public List<ClassDef> funcall(final LambdaStruct lambdaStruct) {
		final JavaClassBuilder classBuilder = new JavaClassBuilder();
		lambdaCodeGenerator.generate(lambdaStruct, classBuilder);
//        assert(closureDepth == 0) : "Unbalanced closure depth: " + closureDepth;
		return classBuilder.getEmitter().getClasses();
	}
}
