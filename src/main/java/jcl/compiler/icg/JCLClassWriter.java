/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@SuppressWarnings("all")
public class JCLClassWriter extends ClassVisitor {

	private final GeneratorState generatorState;

	private final String className;

	private final String fileName;

	private JCLClassWriter previousCw;

	private JCLMethodWriter currentMv;

	public JCLClassWriter(final GeneratorState generatorState, final String className, final String fileName) {
		super(Opcodes.ASM5, new ClassWriter(ClassWriter.COMPUTE_FRAMES));
		this.generatorState = generatorState;
		this.className = className;
		this.fileName = fileName;
	}

	public String getClassName() {
		return className;
	}

	public String getFileName() {
		return fileName;
	}

	public JCLMethodWriter getCurrentMv() {
		return currentMv;
	}

	public void setCurrentMv(final JCLMethodWriter currentMv) {
		this.currentMv = currentMv;
	}

	@Override
	public void visit(final int version, final int access, final String name, final String signature, final String superName, final String[] interfaces) {
//		previousCw = generatorState.getCurrentCw();
//		generatorState.setCurrentCw(this);
		super.visit(version, access, name, signature, superName, interfaces);
	}

	@Override
	public JCLMethodWriter visitMethod(final int access, final String name, final String desc, final String signature, final String[] exceptions) {
		final MethodVisitor mv = super.visitMethod(access, name, desc, signature, exceptions);
		return new JCLMethodWriter(this, mv);
	}

	@Override
	public void visitEnd() {
		super.visitEnd();
//		generatorState.setCurrentCw(previousCw);
	}
}
