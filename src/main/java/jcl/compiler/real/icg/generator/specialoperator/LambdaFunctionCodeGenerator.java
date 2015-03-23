/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.specialoperator.lambda.NewLambdaCodeGenerator;
import jcl.compiler.real.struct.specialoperator.LambdaCompilerFunctionStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaFunctionCodeGenerator implements CodeGenerator<LambdaCompilerFunctionStruct> {

	@Autowired
	private NewLambdaCodeGenerator lambdaCodeGenerator;

	@Override
	public void generate(final LambdaCompilerFunctionStruct input, final JavaClassBuilder classBuilder) {

		final LambdaStruct lambdaStruct = input.getLambdaStruct();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		lambdaCodeGenerator.generate(lambdaStruct, classBuilder);

		final ClassDef generatedLambdaClassDef = classBuilder.getClasses().getFirst();
		final String lambdaClassName = generatedLambdaClassDef.getName();

		mv.visitTypeInsn(Opcodes.NEW, lambdaClassName);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, lambdaClassName, "<init>", "()V", false);
	}
}
