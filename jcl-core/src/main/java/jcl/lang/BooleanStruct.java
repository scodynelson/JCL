package jcl.lang;

public interface BooleanStruct extends SymbolStruct {

	BooleanStruct T = TStruct.INSTANCE;
	BooleanStruct NIL = NILStruct.INSTANCE;

	boolean toJavaPBoolean();

	default Boolean toJavaBoolean() {
		return toJavaPBoolean();
	}

	static BooleanStruct toLispBoolean(final boolean booleanValue) {
		return booleanValue ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
