/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.symbols.NILStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(valuesList)
		                                                                .toString();
	}
}
