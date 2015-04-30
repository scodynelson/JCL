/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.struct.specialoperator.MutableLoadTimeValueStruct;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MutableLoadTimeValueCodeGenerator implements CodeGenerator<MutableLoadTimeValueStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final MutableLoadTimeValueStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct form = input.getForm();

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		formGenerator.generate(form, classBuilder);
		final int initFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

		final Label valuesCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, "jcl/compiler/real/struct/ValuesStruct");
		mv.visitJumpInsn(Opcodes.IFEQ, valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, "jcl/compiler/real/struct/ValuesStruct");
		final int valuesStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, valuesStore);

		mv.visitVarInsn(Opcodes.ALOAD, valuesStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/compiler/real/struct/ValuesStruct", "getPrimaryValue", "()Ljcl/LispStruct;", false);
		mv.visitVarInsn(Opcodes.ASTORE, initFormStore);

		mv.visitLabel(valuesCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, initFormStore);
	}
}
