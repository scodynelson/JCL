package jcl.lang.internal;

import jcl.lang.DoubleFloatStruct;
import jcl.lang.LongFloatStruct;
import jcl.lang.internal.number.FloatStructImpl;
import lombok.EqualsAndHashCode;
import org.apfloat.Apfloat;

@EqualsAndHashCode(callSuper = true)
public class DoubleFloatStructImpl extends FloatStructImpl implements DoubleFloatStruct, LongFloatStruct {

	private final double value;

	public DoubleFloatStructImpl(final double value) {
		super(new Apfloat(value));
		this.value = value;
	}
}
