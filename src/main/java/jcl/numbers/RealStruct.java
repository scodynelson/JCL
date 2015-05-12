/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import jcl.LispStruct;
import jcl.types.RealType;

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
	RealStruct(final RealType type,
	           final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	public abstract boolean plusp();

	public abstract boolean minusp();

	public abstract boolean isLessThan(final LispStruct obj);

	public abstract boolean isGreaterThan(final LispStruct obj);

	public abstract boolean isLessThanOrEqualTo(final LispStruct obj);

	public abstract boolean isGreaterThanOrEqualTo(final LispStruct obj);

	public abstract RationalStruct rational();

	public RealStruct MOD(final RealStruct divisor) {
		final RealStruct result = truncate(divisor);
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

	public abstract RealStruct max(final RealStruct real);

	public abstract RealStruct min(final RealStruct real);

	public ComplexStruct cis() {
		return new ComplexStruct((RealStruct) cos(), (RealStruct) sin());
	}

	public abstract RealStruct atan(final RealStruct real);

	public RealStruct truncate() {
		return truncate(new IntegerStruct(BigInteger.ONE));
	}

	public RealStruct truncate(final RealStruct divisor) {
		return null;
	}

	public RealStruct ftruncate() {
		return ftruncate(new IntegerStruct(BigInteger.ONE));
	}

	public RealStruct ftruncate(final RealStruct second) {
		if (zerop()) {
			RealStruct q = this;
			NumberStruct r = new FloatStruct(BigDecimal.ZERO);
			return q;
		}

		RealStruct q = truncate(second); // an integer
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
		return q;
	}
}
