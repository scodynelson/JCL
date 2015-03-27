/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.specialoperator.lambda.NewLambdaCodeGenerator;
import jcl.compiler.real.struct.specialoperator.LambdaFunctionCallStruct;
import jcl.compiler.real.struct.specialoperator.lambda.LambdaStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class LambdaFunctionCallCodeGenerator implements CodeGenerator<LambdaFunctionCallStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Autowired
	private NewLambdaCodeGenerator lambdaCodeGenerator;

	@Override
	public void generate(final LambdaFunctionCallStruct input, final JavaClassBuilder classBuilder) {

		final LambdaStruct lambda = input.getLambdaStruct();
		final List<LispStruct> arguments = input.getArguments();

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		lambdaCodeGenerator.generate(lambda, classBuilder);
		final int functionStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionStore);

		final int numberOfArguments = arguments.size();
		mv.visitLdcInsn(numberOfArguments);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, "jcl/LispStruct");
		final int argumentsArrayStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argumentsArrayStore);

		final int argumentStore = currentClass.getNextAvailableStore();

		for (int index = 0; index < numberOfArguments; index++) {
			final LispStruct argument = arguments.get(index);
			formGenerator.generate(argument, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, argumentStore);

			mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
			mv.visitLdcInsn(index);
			mv.visitVarInsn(Opcodes.ALOAD, argumentStore);
			mv.visitInsn(Opcodes.AASTORE);
		}

		mv.visitVarInsn(Opcodes.ALOAD, functionStore);
		mv.visitVarInsn(Opcodes.ALOAD, argumentsArrayStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, "jcl/functions/FunctionStruct", "apply", "([Ljcl/LispStruct;)Ljcl/LispStruct;", false);
	}
}