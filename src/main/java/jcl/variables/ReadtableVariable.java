package jcl.variables;

import jcl.structs.ReadtableStruct;

public class ReadtableVariable implements LispVariable<ReadtableStruct> {

	public static final ReadtableVariable INSTANCE = new ReadtableVariable(GlobalReadtableStruct.Readtable);

	private ReadtableStruct value;

	private ReadtableVariable(final ReadtableStruct value) {
		this.value = value;
	}

	public ReadtableStruct getValue() {
		return value;
	}

	public void setValue(final ReadtableStruct value) {
		this.value = value;
	}
}
