package jcl.readtables;

import jcl.variables.LispVariable;

public class ReadtableVariable implements LispVariable<ReadtableStruct> {

	public static final ReadtableVariable INSTANCE = new ReadtableVariable(new ReadtableStruct());

	private ReadtableStruct value;

	private ReadtableVariable(final ReadtableStruct value) {
		this.value = value;
	}

	@Override
	public ReadtableStruct getValue() {
		return value;
	}

	public void setValue(final ReadtableStruct value) {
		this.value = value;
	}
}
