package jcl.lang;

public interface BooleanStruct extends SymbolStruct {

	boolean toJavaPBoolean();

	default Boolean toJavaBoolean() {
		return toJavaPBoolean();
	}

	static BooleanStruct toLispBoolean(final boolean booleanValue) {
		return booleanValue ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}
}
