package jcl.compiler.struct.specialoperator.declare;

import jcl.lang.SymbolStruct;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class LispNameDeclarationStruct implements DeclarationStruct {

	private final SymbolStruct functionSymbolName;
	private final String className;
}
