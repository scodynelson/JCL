/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.MutableLoadTimeValueStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class MutableLoadTimeValueCodeGenerator implements CodeGenerator<MutableLoadTimeValueStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Override
	public void generate(final MutableLoadTimeValueStruct input, final GeneratorState generatorState) {

		final LispStruct form = input.getForm();

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		codeGenerator.generate(form, generatorState);
		final int initFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

		final Label valuesCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.VALUES_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.VALUES_STRUCT_NAME);
		final int valuesStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

		mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.VALUES_STRUCT_NAME,
				GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_NAME,
				GenerationConstants.VALUES_STRUCT_GET_PRIMARY_VALUE_METHOD_DESC,
				false);
		mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

		mv.visitLabel(valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
	}
}
