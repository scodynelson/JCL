package jcl.numbers.newImpl;

import jcl.LispType;
import jcl.classes.BuiltInClassStruct;
import org.apfloat.Apcomplex;

abstract class InternalNumberStruct<A extends Apcomplex> extends BuiltInClassStruct {

	final A ap;

	protected InternalNumberStruct(final LispType type, final A ap) {
		super(type, null, null);
		this.ap = ap;
	}
}
