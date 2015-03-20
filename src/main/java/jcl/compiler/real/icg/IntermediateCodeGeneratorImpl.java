package jcl.compiler.real.icg;

import jcl.LispStruct;
import jcl.compiler.real.icg.generator.FormGenerator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class IntermediateCodeGeneratorImpl implements IntermediateCodeGenerator {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public Object funcall(final LispStruct lispFunc) {
		final JavaClassBuilder classBuilder = new JavaClassBuilder();
		formGenerator.generate(lispFunc, classBuilder);
//        assert(closureDepth == 0) : "Unbalanced closure depth: " + closureDepth;
		return classBuilder.getEmitter().getClasses();
	}
}
