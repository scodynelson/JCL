/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import java.util.List;

public interface SequenceElement extends SimpleElement {

	List<? extends SimpleElement> getElements();
}
