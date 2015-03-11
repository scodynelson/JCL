/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.specialoperator;

import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;

/**
 * Reads all the tags in the TAGBODY form and inserts them into a stack
 * which is returned. Its necessary to do this first since a GO can be
 * executed for a tag declared later in the form.
 */
public class TagbodyLabel {

	private final SymbolStruct<?> symbol;
	private final Label label;
	private final int index;

	public TagbodyLabel(final SymbolStruct<?> symbol, final Label label, final int index) {
		this.symbol = symbol;
		this.label = label;
		this.index = index;
	}

	public SymbolStruct<?> getSymbol() {
		return symbol;
	}

	public Label getLabel() {
		return label;
	}

	public int getIndex() {
		return index;
	}
}
