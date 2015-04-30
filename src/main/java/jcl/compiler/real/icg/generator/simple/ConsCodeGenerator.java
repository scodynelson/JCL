/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import jcl.lists.ConsStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsCodeGenerator implements CodeGenerator<ConsStruct> {

	private static final String CONS_STRUCT_NAME = Type.getInternalName(ConsStruct.class);

	private static final String CONS_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(ConsStruct.class, LispStruct.class, LispStruct.class);

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ConsStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct car = input.getCar();
		final LispStruct cdr = input.getCdr();

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		formGenerator.generate(car, classBuilder);
		final int carStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, carStore);

		formGenerator.generate(cdr, classBuilder);
		final int cdrStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, cdrStore);

		mv.visitTypeInsn(Opcodes.NEW, CONS_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, carStore);
		mv.visitVarInsn(Opcodes.ALOAD, cdrStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, CONS_STRUCT_NAME, GenerationConstants.INIT_METHOD_NAME, CONS_STRUCT_INIT_DESC, false);
	}
}
