package jcl.typespecifiers.designator;

import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.TypeSpecifierUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * This class represents an interval designator, found within compound type specifiers.
 *
 * @param <N> the number type used for the interval boundaries
 */
public class IntervalDesignator<N extends Number> implements CompoundTypeSpecifier {

	// NOTE: Both of these are inclusive; inclusive/exclusive is to be taken care of before create of this
	private final N lowerBound;
	private final N upperBound;

	/**
	 * Constructor for creating an interval for the provided lower and upper boundaries.
	 *
	 * @param lowerBound the lower boundary
	 * @param upperBound the upper boundary
	 */
	public IntervalDesignator(final N lowerBound, final N upperBound) {
		if ((lowerBound != null) && (upperBound != null) && (lowerBound.getClass() != upperBound.getClass())) {
			throw new IllegalArgumentException("Both upper and lower bounds must be of the same class type.");
		}

		this.lowerBound = lowerBound;
		this.upperBound = upperBound;
	}

	/**
	 * Getter for the lower bound value.
	 *
	 * @return the lower bound value
	 */
	public N getLowerBound() {
		return lowerBound;
	}

	/**
	 * Getter for the upper bound value.
	 *
	 * @return the upper bound value
	 */
	public N getUpperBound() {
		return upperBound;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if ((obj == null) || (getClass() != obj.getClass())) {
			return false;
		}

		final IntervalDesignator<?> intervalDesignator = (IntervalDesignator) obj;

		return isWithinBounds(intervalDesignator.lowerBound) && isWithinBounds(intervalDesignator.upperBound);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder()
				.append(lowerBound)
				.append(upperBound)
				.toHashCode();
	}

	/**
	 * This method checks to see whether the number provided is within the bound of this interval.
	 *
	 * @param number the number to verify is within bounds
	 * @return true if the number is within the interval; false otherwise
	 */
	private boolean isWithinBounds(final Number number) {
		final int lowerCompare = TypeSpecifierUtils.numberCompareTo(lowerBound, number);
		final int upperCompare = TypeSpecifierUtils.numberCompareTo(upperBound, number);

		return (lowerCompare >= 0) && (upperCompare <= 0);
	}

	@Override
	public String toString() {
		return "IntervalDesignator{"
				+ "lowerBound=" + lowerBound
				+ ", upperBound=" + upperBound
				+ '}';
	}
}
