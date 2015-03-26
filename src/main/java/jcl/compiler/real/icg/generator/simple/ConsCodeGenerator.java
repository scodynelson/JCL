/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.lists.ConsStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsCodeGenerator implements CodeGenerator<ConsStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final ConsStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct car = input.getCar();
		final LispStruct cdr = input.getCdr();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		formGenerator.generate(car, classBuilder);
		final int carStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, carStore);

		formGenerator.generate(cdr, classBuilder);
		final int cdrStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, cdrStore);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/lists/ConsStruct");
		mv.visitInsn(Opcodes.DUP);

		mv.visitVarInsn(Opcodes.ALOAD, carStore);
		mv.visitVarInsn(Opcodes.ALOAD, cdrStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/lists/ConsStruct", "<init>", "(Ljcl/LispStruct;Ljcl/LispStruct;)V", false);
	}
}
