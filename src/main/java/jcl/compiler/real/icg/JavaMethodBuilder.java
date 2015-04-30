/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import org.objectweb.asm.MethodVisitor;

public class JavaMethodBuilder {

	private final MethodVisitor methodVisitor;

	private int nextAvailableStore;

	public JavaMethodBuilder(final MethodVisitor methodVisitor) {
		this.methodVisitor = methodVisitor;
	}

	public MethodVisitor getMethodVisitor() {
		return methodVisitor;
	}

	public int getNextAvailableStore() {
		return nextAvailableStore++;
	}
}
