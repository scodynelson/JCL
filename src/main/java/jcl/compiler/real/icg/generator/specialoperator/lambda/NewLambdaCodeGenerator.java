/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator.lambda;

import java.security.SecureRandom;
import java.util.Random;

import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CharacterCodeGenerator;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class NewLambdaCodeGenerator implements CodeGenerator<LambdaStruct> {

	@Autowired
	private CharacterCodeGenerator characterCodeGenerator;

	@Override
	public void generate(final LambdaStruct input, final JavaClassBuilder classBuilder) {
	}

	public ClassWriter generate() {

		ClassWriter cw = new ClassWriter(0);
		FieldVisitor fv;
		MethodVisitor mv;
		AnnotationVisitor av0;

		cw.visit(52, Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, "jcl/TestLambdaGenerator", null, "jcl/functions/FunctionStruct", null);

		cw.visitSource("TestLambdaGenerator.java", null);

		{
			final Random random = new SecureRandom();
			final long serialVersionUID = random.nextLong();
			fv = cw.visitField(Opcodes.ACC_PRIVATE + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC, "serialVersionUID", "J", null, serialVersionUID);
			fv.visitEnd();
		}
		{
			mv = cw.visitMethod(Opcodes.ACC_PUBLIC, "<init>", "()V", null, null);
			mv.visitCode();
			final Label l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(25, l0);
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitLdcInsn("DocumentationString");
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/functions/FunctionStruct", "<init>", "(Ljava/lang/String;)V", false);
			final Label l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLineNumber(26, l1);
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/TestLambdaGenerator", "initLambdaListBindings", "()V", false);
			final Label l2 = new Label();
			mv.visitLabel(l2);
			mv.visitLineNumber(27, l2);
			mv.visitInsn(Opcodes.RETURN);
			final Label l3 = new Label();
			mv.visitLabel(l3);
			mv.visitLocalVariable("this", "Ljcl/TestLambdaGenerator;", null, l0, l3, 0);
			mv.visitMaxs(2, 1);
			mv.visitEnd();
		}
		{
			mv = cw.visitMethod(Opcodes.ACC_PRIVATE, "initLambdaListBindings", "()V", null, null);
			mv.visitCode();
			final Label l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(30, l0);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			mv.visitVarInsn(Opcodes.ASTORE, 1);
			final Label l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLineNumber(31, l1);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			mv.visitVarInsn(Opcodes.ASTORE, 2);
			final Label l2 = new Label();
			mv.visitLabel(l2);
			mv.visitLineNumber(32, l2);
			mv.visitInsn(Opcodes.ACONST_NULL);
			mv.visitVarInsn(Opcodes.ASTORE, 3);
			final Label l3 = new Label();
			mv.visitLabel(l3);
			mv.visitLineNumber(33, l3);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			mv.visitVarInsn(Opcodes.ASTORE, 4);
			final Label l4 = new Label();
			mv.visitLabel(l4);
			mv.visitLineNumber(34, l4);
			mv.visitInsn(Opcodes.ICONST_0);
			mv.visitVarInsn(Opcodes.ISTORE, 5);
			final Label l5 = new Label();
			mv.visitLabel(l5);
			mv.visitLineNumber(35, l5);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC, "java/util/Collections", "emptyList", "()Ljava/util/List;", false);
			mv.visitVarInsn(Opcodes.ASTORE, 6);
			final Label l6 = new Label();
			mv.visitLabel(l6);
			mv.visitLineNumber(37, l6);
			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings");
			mv.visitInsn(Opcodes.DUP);
			mv.visitVarInsn(Opcodes.ALOAD, 1);
			mv.visitVarInsn(Opcodes.ALOAD, 2);
			mv.visitVarInsn(Opcodes.ALOAD, 3);
			mv.visitVarInsn(Opcodes.ALOAD, 4);
			mv.visitVarInsn(Opcodes.ALOAD, 6);
			mv.visitInsn(Opcodes.ICONST_0);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings", "<init>", "(Ljava/util/List;Ljava/util/List;Ljcl/compiler/real/environment/binding/lambdalist/RestBinding;Ljava/util/List;Ljava/util/List;Z)V", false);
			mv.visitFieldInsn(Opcodes.PUTFIELD, "jcl/TestLambdaGenerator", "lambdaListBindings", "Ljcl/compiler/real/environment/binding/lambdalist/OrdinaryLambdaListBindings;");
			final Label l7 = new Label();
			mv.visitLabel(l7);
			mv.visitLineNumber(38, l7);
			mv.visitInsn(Opcodes.RETURN);
			final Label l8 = new Label();
			mv.visitLabel(l8);
			mv.visitLocalVariable("this", "Ljcl/Temp;", null, l0, l8, 0);
			mv.visitLocalVariable("requiredBindings", "Ljava/util/List;", "Ljava/util/List<Ljcl/compiler/real/environment/binding/lambdalist/RequiredBinding;>;", l1, l8, 1);
			mv.visitLocalVariable("optionalBindings", "Ljava/util/List;", "Ljava/util/List<Ljcl/compiler/real/environment/binding/lambdalist/OptionalBinding;>;", l2, l8, 2);
			mv.visitLocalVariable("restBinding", "Ljcl/compiler/real/environment/binding/lambdalist/RestBinding;", null, l3, l8, 3);
			mv.visitLocalVariable("keyBindings", "Ljava/util/List;", "Ljava/util/List<Ljcl/compiler/real/environment/binding/lambdalist/KeyBinding;>;", l4, l8, 4);
			mv.visitLocalVariable("allowOtherKeys", "Z", null, l5, l8, 5);
			mv.visitLocalVariable("auxBindings", "Ljava/util/List;", "Ljava/util/List<Ljcl/compiler/real/environment/binding/lambdalist/AuxBinding;>;", l6, l8, 6);
			mv.visitMaxs(9, 7);
			mv.visitEnd();
		}
		{
			mv = cw.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_VARARGS, "apply", "([Ljcl/LispStruct;)Ljcl/LispStruct;", null, null);
			mv.visitCode();
			final Label l0 = new Label();
			mv.visitLabel(l0);
			mv.visitLineNumber(42, l0);
			mv.visitTypeInsn(Opcodes.NEW, "jcl/characters/CharacterStruct");
			mv.visitInsn(Opcodes.DUP);
			mv.visitIntInsn(Opcodes.BIPUSH, 97);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/characters/CharacterStruct", "<init>", "(I)V", false);
			mv.visitInsn(Opcodes.ARETURN);
			final Label l1 = new Label();
			mv.visitLabel(l1);
			mv.visitLocalVariable("this", "Ljcl/TestLambdaGenerator;", null, l0, l1, 0);
			mv.visitLocalVariable("lispStructs", "[Ljcl/LispStruct;", null, l0, l1, 1);
			mv.visitMaxs(3, 2);
			mv.visitEnd();
		}
		cw.visitEnd();

		return cw;
	}
}
