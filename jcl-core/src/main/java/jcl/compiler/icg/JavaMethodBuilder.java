/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.objectweb.asm.MethodVisitor;

@Getter
@RequiredArgsConstructor
public class JavaMethodBuilder {

	private final MethodVisitor methodVisitor;

	private int nextAvailableStore;

	public int getNextAvailableStore() {
		return nextAvailableStore++;
	}
}
