/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import java.util.Arrays;
import java.util.List;

import jcl.lang.list.NILStruct;

public class ValuesStruct implements LispStruct {

	private final List<LispStruct> valuesList;

	private ValuesStruct(final LispStruct... values) {
		this(Arrays.asList(values));
	}

	private ValuesStruct(final List<LispStruct> valuesList) {
		this.valuesList = valuesList;
	}

	public static ValuesStruct valueOf(final LispStruct... values) {
		return new ValuesStruct(values);
	}

	public static ValuesStruct valueOf(final List<LispStruct> valuesList) {
		return new ValuesStruct(valuesList);
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

	public static LispStruct extractPrimaryValue(final LispStruct lispStruct) {
		if (lispStruct instanceof ValuesStruct) {
			return ((ValuesStruct) lispStruct).getPrimaryValue();
		}
		return lispStruct;
	}

	public static void addValuesToList(final List<LispStruct> lispStructs, final LispStruct lispStruct) {
		if (lispStruct instanceof ValuesStruct) {
			final List<LispStruct> valuesList = ((ValuesStruct) lispStruct).valuesList;
			lispStructs.addAll(valuesList);
		} else {
			lispStructs.add(lispStruct);
		}
	}
}
