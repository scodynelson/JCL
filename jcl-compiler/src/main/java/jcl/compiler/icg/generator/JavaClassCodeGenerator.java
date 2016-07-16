/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.JCLClassWriter;
import jcl.lang.LispStruct;
import org.objectweb.asm.Opcodes;

@SuppressWarnings("all")
public abstract class JavaClassCodeGenerator<I extends LispStruct> implements CodeGenerator<I> {

	private final int access;

	private final String name;

	private final String signature;

	private final String superName;

	private final String[] interfaces;

	protected JavaClassCodeGenerator(final String name, final String superName) {
		this(Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, name, null, superName, null);
	}

	protected JavaClassCodeGenerator(final String name, final String[] interfaces) {
		this(Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, name, null, null, interfaces);
	}

	protected JavaClassCodeGenerator(final String name, final String superName, final String[] interfaces) {
		this(Opcodes.ACC_PUBLIC + Opcodes.ACC_SUPER, name, null, superName, interfaces);
	}

	protected JavaClassCodeGenerator(final int access, final String name, final String signature, final String superName,
	                                 final String[] interfaces) {
		this.access = access;
		this.name = name;
		this.signature = signature;
		this.superName = superName;
		this.interfaces = interfaces;
	}

//	@Override
//	public void generate(final I input, final GeneratorState generatorState) {
//		final String fileName = CodeGenerators.getFileNameFromClassName(name);
//		final JCLClassWriter cw = new JCLClassWriter(generatorState, name, fileName);
//
//		cw.visit(Opcodes.V1_8, access, name, signature, superName, interfaces);
//		cw.visitSource(fileName + GenerationConstants.JAVA_EXTENSION, null);
//
//		generateClassContent(cw);
//
//		cw.visitEnd();
//	}

	protected abstract void generateClassContent(JCLClassWriter cw);
}
