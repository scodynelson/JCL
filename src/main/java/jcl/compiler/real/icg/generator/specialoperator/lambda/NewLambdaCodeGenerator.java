/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.lambda;

import java.security.SecureRandom;
import java.util.Random;
import java.util.UUID;

import jcl.arrays.StringStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.PrognCodeGenerator;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class NewLambdaCodeGenerator implements CodeGenerator<LambdaStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Override
	public void generate(final LambdaStruct input, final JavaClassBuilder classBuilder) {

		final String uniqueName = UUID.randomUUID().toString().replace('-', '_');

		final String className = "jcl/Temp_" + uniqueName;
		final ClassDef currentClass = new ClassDef(className);
		classBuilder.getClassStack().push(currentClass);
		classBuilder.setCurrentClass(currentClass);
		classBuilder.getClasses().add(currentClass);

		final ClassWriter cw = currentClass.getClassWriter();

		cw.visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, className, null, "jcl/functions/FunctionStruct", null);

		cw.visitSource("Temp.java", null);

		final int thisStore = currentClass.getNextAvailableStore();
		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();

			final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);
			currentClass.setFieldVisitor(fv);

			fv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);

			final StringStruct docString = input.getDocString();
			String documentation = "";
			if (docString != null) {
				documentation = docString.getAsJavaString();
			}
			mv.visitLdcInsn(documentation);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/functions/FunctionStruct", "<init>", "(Ljava/lang/String;)V", false);

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, className, "initLambdaListBindings", "()V", false);

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "initLambdaListBindings", "()V", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			final int requiredStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, requiredStore);

			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			final int optionalStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, optionalStore);

			mv.visitInsn(Opcodes.ACONST_NULL);
			final int restStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, restStore);

			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			final int keyStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, keyStore);

			mv.visitInsn(Opcodes.ICONST_0);
			final int allowOtherKeysStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ISTORE, allowOtherKeysStore);

			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			final int auxStore = currentClass.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, auxStore);

			mv.visitVarInsn(Opcodes.ALOAD, thisStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, requiredStore);
			mv.visitVarInsn(Opcodes.ALOAD, optionalStore);
			mv.visitVarInsn(Opcodes.ALOAD, restStore);
			mv.visitVarInsn(Opcodes.ALOAD, keyStore);
			mv.visitVarInsn(Opcodes.ALOAD, auxStore);
			mv.visitInsn(Opcodes.ICONST_0);

			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings", "<init>", "(Ljava/util/List;Ljava/util/List;Ljcl/compiler/real/environment/binding/lambdalist/RestBinding;Ljava/util/List;Ljava/util/List;Z)V", false);

			mv.visitFieldInsn(Opcodes.PUTFIELD, className, "lambdaListBindings", "Ljcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings;");

			mv.visitInsn(Opcodes.RETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		{
			final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_VARARGS, "apply", "([Ljcl/LispStruct;)Ljcl/LispStruct;", null, null);
			currentClass.setMethodVisitor(mv);
			mv.visitCode();

			final PrognStruct forms = input.getForms();
			prognCodeGenerator.generate(forms, classBuilder);

			mv.visitInsn(Opcodes.ARETURN);

			mv.visitMaxs(-1, -1);
			mv.visitEnd();
		}
		cw.visitEnd();
	}
}
