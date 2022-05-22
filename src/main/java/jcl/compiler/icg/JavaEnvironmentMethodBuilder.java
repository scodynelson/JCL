/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg;

import lombok.Getter;
import org.objectweb.asm.MethodVisitor;

@Getter
public class JavaEnvironmentMethodBuilder extends JavaMethodBuilder {

	private final int thisStore;
	private final int environmentStore;

	public JavaEnvironmentMethodBuilder(final MethodVisitor methodVisitor) {
		super(methodVisitor);
		thisStore = getNextAvailableStore();
		environmentStore = getNextAvailableStore();
	}
}
