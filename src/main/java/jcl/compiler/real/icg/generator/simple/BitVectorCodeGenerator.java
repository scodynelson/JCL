/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import java.util.List;

import jcl.arrays.BitVectorStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import jcl.numbers.IntegerStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BitVectorCodeGenerator implements CodeGenerator<BitVectorStruct> {

	private static final String BIT_VECTOR_STRUCT_NAME = Type.getInternalName(BitVectorStruct.class);

	private static final String BIT_VECTOR_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(BitVectorStruct.class, List.class);

	@Autowired
	private IntegerCodeGenerator integerCodeGenerator;

	@Override
	public void generate(final BitVectorStruct input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.JAVA_ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
				false);
		final int contentsStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, contentsStore);

		final int contentStore = currentClass.getNextAvailableStore();

		final List<IntegerStruct> contents = input.getContents();
		for (final IntegerStruct content : contents) {
			integerCodeGenerator.generate(content, classBuilder);
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

		mv.visitTypeInsn(Opcodes.NEW, BIT_VECTOR_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				BIT_VECTOR_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				BIT_VECTOR_STRUCT_INIT_DESC,
				false);
	}
}