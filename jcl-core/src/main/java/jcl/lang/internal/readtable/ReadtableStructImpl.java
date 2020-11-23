/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.readtable;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.BooleanStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.internal.LispStructImpl;
import jcl.lang.AttributeType;
import jcl.lang.ReadtableCase;
import jcl.lang.SyntaxType;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link ReadtableStructImpl} is the object representation of a Lisp 'readtable' type.
 */
public final class ReadtableStructImpl extends LispStructImpl implements ReadtableStruct {

	/**
	 * Internal map storing the {@link Integer} code point mappings to appropriate {@link FunctionStruct}s.
	 */
	private final Map<Integer, FunctionStruct> macroTableMap = new ConcurrentHashMap<>();

	/**
	 * Internal map storing the {@link Integer} code point mappings to appropriate {@link Map}s for
	 * dispatching on specializing {@link FunctionStruct}s.
	 */
	private final Map<Integer, Map<Integer, FunctionStruct>> dispatchTableMap = new ConcurrentHashMap<>();

	/**
	 * Internal {@link AttributeTable} storing the {@link Integer} code point mappings to {@link AttributeType}s.
	 */
	private final AttributeTable attributeTable = new AttributeTable();

	/**
	 * Internal {@link SyntaxTable} storing the {@link Integer} code point mappings to {@link SyntaxType}s.
	 */
	private final SyntaxTable syntaxTable = new SyntaxTable();

	/**
	 * The readtable case.
	 */
	private ReadtableCase readtableCase;

	/**
	 * Public constructor.
	 */
	public ReadtableStructImpl() {
		this(ReadtableCase.UPCASE);
	}

	/**
	 * Public constructor.
	 *
	 * @param readtableCase
	 * 		the readtable case
	 */
	public ReadtableStructImpl(final ReadtableCase readtableCase) {
		this.readtableCase = readtableCase;
	}

	@Override
	public boolean makeDispatchMacroCharacter(final FunctionStruct dispatchTable, final int codePoint, final boolean nonTerminatingP) {
		setMacroCharacter(codePoint, dispatchTable, nonTerminatingP);
		dispatchTableMap.put(codePoint, new HashMap<>());
		return nonTerminatingP;
	}

	@Override
	public void setMacroCharacter(final int codePoint, final FunctionStruct readerMacroFunction, final boolean nonTerminatingP) {
		if (!nonTerminatingP) {
			syntaxTable.setSyntaxType(codePoint, SyntaxType.TERMINATING);
		}
		macroTableMap.put(codePoint, readerMacroFunction);
	}

	@Override
	public ReadtableCase getReadtableCase() {
		return readtableCase;
	}

	@Override
	public void setReadtableCase(final ReadtableCase readtableCase) {
		this.readtableCase = readtableCase;
	}

	@Override
	public FunctionStruct getMacroCharacter(final int codePoint) {
		return macroTableMap.get(codePoint);
	}

	@Override
	public FunctionStruct getDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint) {
		return dispatchTableMap.get(dispatchCodePoint).get(subCodePoint);
	}

	@Override
	public void setDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint, final FunctionStruct readerMacroFunction) {
		dispatchTableMap.get(dispatchCodePoint).put(subCodePoint, readerMacroFunction);
	}

	@Override
	public AttributeType getAttributeType(final int codePoint, final IntegerStruct readBase) {
		return attributeTable.getAttribute(codePoint, readBase);
	}

	@Override
	public SyntaxType getSyntaxType(final int codePoint) {
		return syntaxTable.getSyntaxType(codePoint);
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.READTABLE;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.READTABLE;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.READTABLE) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.READTABLE) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
