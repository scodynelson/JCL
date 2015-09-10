package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.MultipleValueProg1Struct;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class MultipleValueProg1CodeGenerator extends SpecialOperatorCodeGenerator<MultipleValueProg1Struct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	private MultipleValueProg1CodeGenerator() {
		super("multipleValueProg1_");
	}

	@Override
	protected void generateSpecialOperator(final MultipleValueProg1Struct input, final GeneratorState generatorState,
	                                       final JavaMethodBuilder methodBuilder, final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final LispStruct firstForm = input.getFirstForm();
		final PrognStruct forms = input.getForms();

		codeGenerator.generate(firstForm, generatorState);

		final int firstFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, firstFormStore);

		prognCodeGenerator.generate(forms, generatorState);
		mv.visitInsn(Opcodes.POP);

		mv.visitVarInsn(Opcodes.ALOAD, firstFormStore);

		mv.visitInsn(Opcodes.ARETURN);
	}
}
