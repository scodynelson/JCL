/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.util.List;

import jcl.LispStruct;
import jcl.types.RealType;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link RealStruct} is the object representation of a Lisp 'real' type.
 */
public abstract class RealStruct extends NumberStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7159935653316309907L;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RealStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(RealType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the real object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RealStruct(final RealType type,
	                     final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		if (base instanceof RealStruct) {
			final double number = doubleValue();
			final double baseVal = ((RealStruct) base).doubleValue();
			final double log = FastMath.log(baseVal, number);
			return new FloatStruct(new BigDecimal(log));
		}
		return super.log(base);
	}

	public abstract double doubleValue();

	public abstract boolean plusp();

	public abstract boolean minusp();

	public abstract boolean isLessThan(LispStruct obj);

	public abstract boolean isGreaterThan(LispStruct obj);

	public abstract boolean isLessThanOrEqualTo(LispStruct obj);

	public abstract boolean isGreaterThanOrEqualTo(LispStruct obj);

	public abstract RationalStruct rational();

	public RealStruct MOD(final RealStruct divisor) {
		final RealStruct result = truncate(divisor).getQuotient();
		// TODO: this doesn't return both values...
		if (!result.zerop()) {
			if (divisor.minusp()) {
				if (plusp()) {
					return (RealStruct) result.add(divisor);
				}
			} else {
				if (minusp()) {
					return (RealStruct) result.add(divisor);
				}
			}
		}
		return result;
	}

	public abstract RealStruct max(RealStruct real);

	public abstract RealStruct min(RealStruct real);

	public ComplexStruct cis() {
		return new ComplexStruct((RealStruct) cos(), (RealStruct) sin());
	}

	public abstract RealStruct atan(RealStruct real);

	public TruncateResult truncate() {
		return truncate(IntegerStruct.ONE);
	}

	public abstract TruncateResult truncate(RealStruct divisor);

	public TruncateResult ftruncate() {
		return ftruncate(IntegerStruct.ONE);
	}

	public TruncateResult ftruncate(final RealStruct second) {
		if (zerop()) {
			return new TruncateResult(this, FloatStruct.ZERO);
		}

		final TruncateResult truncateResult = truncate(second);
		RealStruct q = truncateResult.getQuotient(); // an integer
		if (q.zerop()) {
			if (minusp()) {
				if (second.minusp()) {
					q = new FloatStruct(new BigDecimal("0.0"));
				} else {
					q = new FloatStruct(new BigDecimal("-0.0"));
				}
			} else if (second.minusp()) {
				q = new FloatStruct(new BigDecimal("-0.0"));
			} else {
				q = new FloatStruct(new BigDecimal("0.0"));
			}
		} else {
			q = new FloatStruct(new BigDecimal(((IntegerStruct) q).getBigInteger()));
		}
		return new TruncateResult(q, truncateResult.getRemainder());
	}
}
