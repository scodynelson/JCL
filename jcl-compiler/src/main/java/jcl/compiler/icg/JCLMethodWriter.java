/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg;

import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

@SuppressWarnings("all")
public class JCLMethodWriter extends MethodVisitor {

	private final JCLClassWriter cw;

	private JCLMethodWriter previousMv;

	private int nextAvailableStore;

	public JCLMethodWriter(final JCLClassWriter cw, final MethodVisitor mv) {
		super(Opcodes.ASM9, mv);
		this.cw = cw;
	}

	public int visitVarInsn(final int opcode) {
		final int nextVar = nextAvailableStore++;
		visitVarInsn(opcode, nextVar);
		return nextVar;
	}

	@Override
	public void visitCode() {
		previousMv = cw.getCurrentMv();
		cw.setCurrentMv(this);
		super.visitCode();
	}

	@Override
	public void visitEnd() {
		super.visitEnd();
		cw.setCurrentMv(previousMv);
	}
}
