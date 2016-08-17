/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.number;

import jcl.lang.classes.BuiltInClassStruct;
import jcl.type.LispType;
import org.apfloat.Apcomplex;

abstract class InternalNumberStruct<A extends Apcomplex> extends BuiltInClassStruct {

	final A ap;

	protected InternalNumberStruct(final LispType type, final A ap) {
		super(type, null, null);
		this.ap = ap;
	}
}
