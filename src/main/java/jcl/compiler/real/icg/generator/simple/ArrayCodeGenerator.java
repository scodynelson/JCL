/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.ArrayStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import jcl.compiler.real.icg.generator.specialoperator.QuoteCodeGenerator;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ArrayCodeGenerator implements CodeGenerator<ArrayStruct<LispStruct>> {

	private static final String ARRAY_STRUCT_NAME = Type.getInternalName(ArrayStruct.class);

	private static final String ARRAY_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(ArrayStruct.class, List.class, List.class);

	@Autowired
	private QuoteCodeGenerator quoteCodeGenerator;

	@Override
	public void generate(final ArrayStruct<LispStruct> input, final JavaClassBuilder classBuilder) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		final int dimensionsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, dimensionsStore);

		final List<Integer> dimensions = input.getDimensions();
		for (final Integer dimension : dimensions) {
			mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
			mv.visitLdcInsn(dimension);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
					GenerationConstants.JAVA_INTEGER_NAME,
					GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_NAME,
					GenerationConstants.JAVA_INTEGER_VALUE_OF_METHOD_DESC,
					false);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		final int contentsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, contentsStore);

		final int contentStore = methodBuilder.getNextAvailableStore();

		final List<LispStruct> contents = input.getContents();
		for (final LispStruct content : contents) {
			quoteCodeGenerator.generateQuotedObject(content, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, contentStore);

			mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
			mv.visitVarInsn(Opcodes.ALOAD, contentStore);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
					GenerationConstants.JAVA_LIST_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_NAME,
					GenerationConstants.JAVA_LIST_ADD_METHOD_DESC,
					true);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitTypeInsn(Opcodes.NEW, ARRAY_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, dimensionsStore);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				ARRAY_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				ARRAY_STRUCT_INIT_DESC,
				false);
	}
}
