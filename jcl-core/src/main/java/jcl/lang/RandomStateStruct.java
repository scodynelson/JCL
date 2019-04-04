package jcl.lang;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.condition.exception.TypeErrorException;

/**
 * Created by codynelson on 8/1/16.
 */
public interface RandomStateStruct extends LispStruct {

	/**
	 * Retrieves a random {@link BigInteger} from the internal random seed.
	 * TODO: fix??
	 *
	 * @param limit
	 * 		the upper limit of the random {@link BigInteger}
	 *
	 * @return the random {@link BigInteger}
	 */
	BigInteger randomInteger(final long limit);

	/**
	 * Retrieves a random {@link BigDecimal} from the internal random seed.
	 * TODO: fix??
	 *
	 * @param limit
	 * 		the upper limit of the random {@link BigDecimal}
	 *
	 * @return the random {@link BigDecimal}
	 */
	BigDecimal randomFloat(final double limit);

	default LispStruct random(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			// TODO: fix??
			final IntegerStruct number = (IntegerStruct) real;
			final BigInteger randomInteger = randomInteger(number.toJavaPLong());
			return IntegerStruct.toLispInteger(randomInteger);
		} else if (real instanceof FloatStruct) {
			// TODO: fix??
			final FloatStruct number = (FloatStruct) real;
			final BigDecimal randomFloat = randomFloat(number.toJavaDouble());
			return DoubleFloatStruct.toLispFloat(randomFloat.doubleValue());
		} else {
			throw new TypeErrorException("Real argument not of type Integer or Float: " + real);
		}
	}
}
