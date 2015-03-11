package jcl.compiler.real.icg;

import jcl.characters.CharacterStruct;
import jcl.compiler.real.icg.generator.CharacterCodeGenerator;
import jcl.compiler.real.icg.generator.ComplexCodeGenerator;
import jcl.compiler.real.icg.generator.FloatCodeGenerator;
import jcl.compiler.real.icg.generator.IntegerCodeGenerator;
import jcl.compiler.real.icg.generator.NILCodeGenerator;
import jcl.compiler.real.icg.generator.RatioCodeGenerator;
import jcl.compiler.real.icg.generator.SymbolCodeGenerator;
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Component
@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class IntermediateCodeGeneratorImpl implements IntermediateCodeGenerator {

	private static final Logger LOGGER = LoggerFactory.getLogger(IntermediateCodeGeneratorImpl.class);

	@Autowired
	private NILCodeGenerator nilCodeGenerator;

	@Autowired
	private CharacterCodeGenerator characterCodeGenerator;

	@Autowired
	private IntegerCodeGenerator integerCodeGenerator;

	@Autowired
	private FloatCodeGenerator floatCodeGenerator;

	@Autowired
	private RatioCodeGenerator ratioCodeGenerator;

	@Autowired
	private ComplexCodeGenerator complexCodeGenerator;

	@Autowired
	private SymbolCodeGenerator symbolCodeGenerator;

	@Autowired
	private ListCodeGenerator listCodeGenerator;

	@Override
	public Object funcall(final Object lispFunc) {
		final JavaClassBuilder classBuilder = new JavaClassBuilder();
		icgMainLoop(lispFunc, classBuilder);
//        assert(closureDepth == 0) : "Unbalanced closure depth: " + closureDepth;
		return classBuilder.getEmitter().getClasses();
	}

	@Override
	public void icgMainLoop(final Object obj, final boolean allowMultipleValues, final JavaClassBuilder classBuilder) {
		final boolean currentMV = classBuilder.isAllowMultipleValues();
		try {
			classBuilder.setAllowMultipleValues(allowMultipleValues);
			icgMainLoop(obj, classBuilder);
		} finally {
			classBuilder.setAllowMultipleValues(currentMV);
		}
	}

	@Override
	public void icgMainLoop(final Object obj, final JavaClassBuilder classBuilder) {

		if (obj.equals(NullStruct.INSTANCE)) {
			nilCodeGenerator.generate((NullStruct) obj, this, classBuilder);
		} else if (obj instanceof CharacterStruct) {
			characterCodeGenerator.generate((CharacterStruct) obj, this, classBuilder);
		} else if (obj instanceof IntegerStruct) {
			integerCodeGenerator.generate((IntegerStruct) obj, this, classBuilder);
		} else if (obj instanceof FloatStruct) {
			floatCodeGenerator.generate((FloatStruct) obj, this, classBuilder);
		} else if (obj instanceof RatioStruct) {
			ratioCodeGenerator.generate((RatioStruct) obj, this, classBuilder);
//		} else if (obj instanceof ComplexStruct) {
//			complexCodeGenerator.generate((ComplexStruct) obj, this, classBuilder);
		} else if (obj instanceof SymbolStruct) {
			symbolCodeGenerator.generate((SymbolStruct) obj, this, classBuilder);
		} else if (obj instanceof ConsStruct) {
			listCodeGenerator.generate((ConsStruct) obj, this, classBuilder);
		} else {
			LOGGER.error("ICG: Found thing I can't generate code for: {}, class: {}", obj, obj.getClass().getName());
		}
	}

}
