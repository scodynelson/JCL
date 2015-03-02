/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.system.EnhancedLinkedList;

public interface ListElement extends SequenceElement {

	@Override
	EnhancedLinkedList<SimpleElement> getElements();

	boolean isDotted();
}
