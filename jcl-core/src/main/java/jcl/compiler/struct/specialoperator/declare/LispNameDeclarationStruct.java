package jcl.compiler.struct.specialoperator.declare;

import jcl.lang.SymbolStruct;

public class LispNameDeclarationStruct implements DeclarationStruct {

	private final SymbolStruct functionSymbolName;
	private final String className;

	public LispNameDeclarationStruct(final SymbolStruct functionSymbolName, final String className) {
		this.functionSymbolName = functionSymbolName;
		this.className = className;
	}

	public SymbolStruct getFunctionSymbolName() {
		return functionSymbolName;
	}

	public String getClassName() {
		return className;
	}
}
