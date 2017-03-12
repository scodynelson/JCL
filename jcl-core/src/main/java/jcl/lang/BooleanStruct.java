package jcl.lang;

public interface BooleanStruct extends SymbolStruct {

	BooleanStruct T = TStruct.INSTANCE;
	BooleanStruct NIL = NILStruct.INSTANCE;

	boolean booleanValue();
}
