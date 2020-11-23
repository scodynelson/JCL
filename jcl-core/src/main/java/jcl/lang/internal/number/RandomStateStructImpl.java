/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.number;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigInteger;
import java.security.SecureRandom;

import jcl.lang.BignumStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.DoubleFloatStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.LongnumStruct;
import jcl.lang.RandomStateStruct;
import jcl.lang.RealStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.LispStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link RandomStateStructImpl} is the object representation of a Lisp 'random-state' type.
 */
public final class RandomStateStructImpl extends LispStructImpl implements RandomStateStruct {

	/**
	 * The {@link SecureRandom} used to retrieve random real numbers.
	 */
	private final SecureRandom random;

	/**
	 * Public constructor.
	 */
	public RandomStateStructImpl() {
		random = new SecureRandom();
	}

	/**
	 * Public constructor.
	 *
	 * @param randomState
	 * 		the {@link RandomStateStruct} to initial from
	 */
	public RandomStateStructImpl(final RandomStateStruct randomState) {
		try {
			final ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
			final ObjectOutputStream out = new ObjectOutputStream(byteOut);
			out.writeObject(randomState.randomGenerator());
			out.close();

			final ByteArrayInputStream byteIn = new ByteArrayInputStream(byteOut.toByteArray());
			final ObjectInputStream in = new ObjectInputStream(byteIn);
			random = (SecureRandom) in.readObject();
			in.close();
		} catch (final IOException | ClassNotFoundException e) {
			throw new ErrorException("Unable to copy random state.", e);
		}
	}

	@Override
	public SecureRandom randomGenerator() {
		return random;
	}

	@Override
	public RealStruct random(final RealStruct limit) {
		if (!limit.plusp().toJavaPBoolean()) {
			throw new TypeErrorException("Limit is not a POSITIVE INTEGER or POSITIVE FLOAT: " + limit);
		}

		if (limit instanceof FixnumStruct) {
			final int limitVal = ((FixnumStruct) limit).toJavaInt();
			final int rand = random.nextInt(limitVal);
			return IntegerStruct.toLispInteger(rand);
		} else if (limit instanceof LongnumStruct) {
			final long limitVal = ((LongnumStruct) limit).toJavaPLong();
			final long rand = limitVal * random.nextLong();
			return IntegerStruct.toLispInteger(rand);
		} else if (limit instanceof BignumStruct) {
			final BigInteger limitVal = ((BignumStruct) limit).toJavaBigInteger();
			final int bitLength = limitVal.bitLength();
			final BigInteger rand = new BigInteger(bitLength + 1, random);
			final BigInteger remainder = rand.remainder(limitVal);
			return IntegerStruct.toLispInteger(remainder);
		} else if (limit instanceof SingleFloatStruct) {
			final float limitVal = ((SingleFloatStruct) limit).toJavaPFloat();
			final float rand = limitVal * random.nextFloat();
			return SingleFloatStruct.toLispFloat(rand);
		} else if (limit instanceof DoubleFloatStruct) {
			final double limitVal = ((DoubleFloatStruct) limit).toJavaPDouble();
			final double rand = limitVal * random.nextDouble();
			return DoubleFloatStruct.toLispFloat(rand);
		} else {
			throw new TypeErrorException("Unsupported REAL Limit value: " + limit);
		}
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.RANDOM_STATE;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.RANDOM_STATE;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.RANDOM_STATE) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.RANDOM_STATE) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
