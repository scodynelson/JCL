package jcl.compiler.real.icg.generator.simple;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.lists.ListStruct;
import jcl.numbers.FloatStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.numbers.RealStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ComplexCodeGenerator implements CodeGenerator<ListStruct> {

	@Autowired
	private IntegerCodeGenerator integerCodeGenerator;

	@Autowired
	private FloatCodeGenerator floatCodeGenerator;

	@Autowired
	private RatioCodeGenerator ratioCodeGenerator;

	@Override
	public void generate(final ListStruct input, final JavaClassBuilder classBuilder) {
		// TODO: we NEED to do Complex numbers better!!
		// TODO: now this is hacked up. we probably shouldn't even have this generator to begin with...
		final RealStruct real = (RealStruct) input.getRest().getFirst();
		final RealStruct imaginary = (RealStruct) input.getRest().getRest().getFirst();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		if (real instanceof IntegerStruct) {
			integerCodeGenerator.generate((IntegerStruct) real, classBuilder);
		} else if (real instanceof FloatStruct) {
			floatCodeGenerator.generate((FloatStruct) real, classBuilder);
		} else if (real instanceof RatioStruct) {
			ratioCodeGenerator.generate((RatioStruct) real, classBuilder);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		if (imaginary instanceof IntegerStruct) {
			integerCodeGenerator.generate((IntegerStruct) imaginary, classBuilder);
		} else if (imaginary instanceof FloatStruct) {
			floatCodeGenerator.generate((FloatStruct) imaginary, classBuilder);
		} else if (imaginary instanceof RatioStruct) {
			ratioCodeGenerator.generate((RatioStruct) imaginary, classBuilder);
		} else {
			throw new RuntimeException("Only reals are valid for the Complex 'real' part.");
		}

		mv.visitMethodInsn(Opcodes.INVOKESTATIC, "jcl/numbers/ComplexStruct", "getInstance", "(Ljcl/numbers/RealStruct;Ljcl/numbers/RealStruct;)Ljcl/numbers/ComplexStruct;", false);
	}
}
