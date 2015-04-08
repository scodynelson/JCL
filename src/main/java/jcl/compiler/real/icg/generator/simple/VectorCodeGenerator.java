/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.simple;

import java.util.List;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.icg.generator.GeneratorUtils;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class VectorCodeGenerator implements CodeGenerator<VectorStruct<LispStruct>> {

	private static final String VECTOR_STRUCT_NAME = Type.getInternalName(VectorStruct.class);

	private static final String VECTOR_STRUCT_INIT_DESC = GeneratorUtils.getConstructorDescription(VectorStruct.class, List.class);

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final VectorStruct<LispStruct> input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final MethodVisitor mv = currentClass.getMethodVisitor();

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				GenerationConstants.ARRAY_LIST_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				GenerationConstants.ARRAY_LIST_INIT_DESC,
				false);
		final int contentsStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, contentsStore);

		final int contentStore = currentClass.getNextAvailableStore();

		final List<LispStruct> contents = input.getContents();
		for (final LispStruct content : contents) {
			formGenerator.generate(content, classBuilder);
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

		mv.visitTypeInsn(Opcodes.NEW, VECTOR_STRUCT_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ALOAD, contentsStore);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
				VECTOR_STRUCT_NAME,
				GenerationConstants.INIT_METHOD_NAME,
				VECTOR_STRUCT_INIT_DESC,
				false);
	}
}
