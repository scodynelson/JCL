/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import org.objectweb.asm.Label;

public class TagbodyLabel {

	private final GoStruct<?> tag;

	private final int index;

	private final Label label;

	public TagbodyLabel(final GoStruct<?> tag, final int index, final Label label) {
		this.tag = tag;
		this.index = index;
		this.label = label;
	}

	public GoStruct<?> getTag() {
		return tag;
	}

	public int getIndex() {
		return index;
	}

	public Label getLabel() {
		return label;
	}
}
