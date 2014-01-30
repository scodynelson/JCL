package jcl.structs.readtables;

import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.AttributeType;
import jcl.reader.syntax.CaseSpec;
import jcl.reader.syntax.SyntaxType;
import jcl.structs.LispStruct;
import jcl.types.LispType;
import jcl.types.readtables.Readtable;

import java.util.HashMap;
import java.util.Map;

public class ReadtableStruct implements LispStruct {

	private final Map<Integer, ReaderMacroFunction> macroTableMap = new HashMap<>();
	private final Map<Integer, DispatchTable> dispatchTableMap = new HashMap<>();

	private final AttributeTable attributeTable = new AttributeTable();
	private final SyntaxTable syntaxTable = new SyntaxTable();

	private CaseSpec readtableCase;

	private ReadtableStruct(final CaseSpec caseSpec) {
		readtableCase = caseSpec;
	}

	@Override
	public LispType getType() {
		return Readtable.INSTANCE;
	}

	public CaseSpec getReadtableCase() {
		return readtableCase;
	}

	public void setReadtableCase(final CaseSpec readtableCase) {
		this.readtableCase = readtableCase;
	}

	public ReaderMacroFunction getMacroCharacter(final int codePoint) {
		return macroTableMap.get(codePoint);
	}

	public void setMacroCharacter(final int codePoint, final ReaderMacroFunction readerMacroFunction, final boolean nonTerminatingP) {
		if (!nonTerminatingP) {
			syntaxTable.setSyntaxType(codePoint, SyntaxType.TERMINATING);
		}
		macroTableMap.put(codePoint, readerMacroFunction);
	}

	public DispatchTable getDispatchTable(final int codePoint) {
		return dispatchTableMap.get(codePoint);
	}

	public boolean makeDispatchMacroCharacter(final int codePoint, final boolean nonTerminatingP) {

		final DispatchTable dispatchTable = new DispatchTable();
		setMacroCharacter(codePoint, dispatchTable, nonTerminatingP);
		dispatchTableMap.put(codePoint, dispatchTable);
		return nonTerminatingP;
	}

	public ReaderMacroFunction getDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint) {
		return dispatchTableMap.get(dispatchCodePoint).getMacroCharacter(subCodePoint);
	}

	public void setDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint, final ReaderMacroFunction readerMacroFunction) {
		dispatchTableMap.get(dispatchCodePoint).setMacroCharacter(subCodePoint, readerMacroFunction);
	}

	public AttributeType getAttributeType(final int codePoint) {
		return attributeTable.getAttribute(codePoint);
	}

	public SyntaxType getSyntaxType(final int codePoint) {
		return syntaxTable.getSyntaxType(codePoint);
	}

	// BUILDERS

	public static ReadtableStruct getStruct() {
		return new ReadtableStruct(CaseSpec.UPCASE);
	}

	public static ReadtableStruct getStruct(final CaseSpec caseSpec) {
		return new ReadtableStruct(caseSpec);
	}
}
