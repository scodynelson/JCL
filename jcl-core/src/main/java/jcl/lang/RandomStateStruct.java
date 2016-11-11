package jcl.lang;

import java.math.BigDecimal;
import java.math.BigInteger;

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
}
