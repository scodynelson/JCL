/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

public class ValuesStruct implements LispStruct {

	private final List<LispStruct> valuesList;

	public ValuesStruct(final LispStruct... values) {
		this(Arrays.asList(values));
	}

	public ValuesStruct(final List<LispStruct> valuesList) {
		this.valuesList = valuesList;
	}

	public List<LispStruct> getValuesList() {
		return valuesList;
	}

	public LispStruct getPrimaryValue() {
		if (valuesList.isEmpty()) {
			return NILStruct.INSTANCE;
		}
		return valuesList.get(0);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(valuesList)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final ValuesStruct rhs = (ValuesStruct) obj;
		return new EqualsBuilder().append(valuesList, rhs.valuesList)
		                          .isEquals();
	}

	@Override
	public String toString() {

		final StringBuilder stringBuilder = new StringBuilder();

		final int numberOfValues = valuesList.size();

		for (int i = 0; i < numberOfValues; i++) {
			final LispStruct value = valuesList.get(i);

			final String printedValue = value.toString();
			stringBuilder.append(printedValue);

			if (i < (numberOfValues - 1)) {
				stringBuilder.append('\n');
			}
		}

		return stringBuilder.toString();
	}
}
