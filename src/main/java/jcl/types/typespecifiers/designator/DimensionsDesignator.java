package jcl.types.typespecifiers.designator;

import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.ObjectUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * This class represents a dimensions designator, found within compound {@code Array}s.
 */
public class DimensionsDesignator implements CompoundTypeSpecifier {

	private final List<Integer> dimensions;

	/**
	 * Constructor for creating a dimensions wrapper for the provided dimensions.
	 *
	 * @param dimensions an array of dimensions
	 */
	public DimensionsDesignator(final Integer... dimensions) {
		this.dimensions = new ArrayList<>(Arrays.asList(dimensions));
	}

	/**
	 * Constructor for creating a dimensions wrapper for the provided dimensions list.
	 *
	 * @param dimensions a list of dimensions
	 */
	public DimensionsDesignator(final List<Integer> dimensions) {
		this.dimensions = dimensions;
	}

	public List<Integer> getDimensions() {
		return dimensions;
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if ((obj == null) || (getClass() != obj.getClass())) {
			return false;
		}

		final DimensionsDesignator dimensionsDesignator = (DimensionsDesignator) obj;

		return (dimensions == null) || ((dimensionsDesignator.dimensions != null) && checkDimensions(dimensions, dimensionsDesignator.dimensions));

	}

	@Override
	public int hashCode() {
		return (dimensions != null) ? dimensions.hashCode() : 0;
	}

	/**
	 * This method test the two provided dimensions to ensure that the they are equivalent to one another, null being
	 * equivalent to everything.
	 *
	 * @param dimensions1 the primary dimensions list
	 * @param dimensions2 the secondary dimensions list
	 * @return true if the dimensions are equivalent; false otherwise
	 */
	private static boolean checkDimensions(final List<Integer> dimensions1, final List<Integer> dimensions2) {

		if (dimensions1.size() != dimensions2.size()) {
			return false;
		}

		final int size = dimensions1.size();

		for (int i = 0; i < size; i++) {
			final Integer fromDimension = dimensions1.get(i);
			final Integer toDimension = dimensions2.get(i);

			if ((fromDimension != null) && ObjectUtils.notEqual(fromDimension, toDimension)) {
				return false;
			}
		}

		return true;
	}

	@Override
	public String toString() {
		return "DimensionsDesignator{"
				+ "dimensions=" + dimensions
				+ '}';
	}
}
