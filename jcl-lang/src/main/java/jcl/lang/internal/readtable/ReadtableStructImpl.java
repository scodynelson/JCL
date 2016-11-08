/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.readtable;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import jcl.lang.FunctionStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.readtable.AttributeType;
import jcl.lang.readtable.DispatchingReaderMacroFunction;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.readtable.SyntaxType;
import jcl.type.ReadtableType;

/**
 * The {@link ReadtableStructImpl} is the object representation of a Lisp 'readtable' type.
 */
public final class ReadtableStructImpl extends BuiltInClassStruct implements ReadtableStruct {

	/**
	 * Internal map storing the {@link Integer} code point mappings to appropriate {@link FunctionStruct}s.
	 */
	private final Map<Integer, FunctionStruct> macroTableMap = new ConcurrentHashMap<>();

	/**
	 * Internal map storing the {@link Integer} code point mappings to appropriate {@link DispatchingReaderMacroFunction}s for
	 * dispatching on specializing {@link FunctionStruct}s.
	 */
	private final Map<Integer, DispatchingReaderMacroFunction> dispatchTableMap = new ConcurrentHashMap<>();

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
	private ReadtableStructImpl() {
		this(ReadtableCase.UPCASE);
	}

	/**
	 * Public constructor.
	 *
	 * @param readtableCase
	 * 		the readtable case
	 */
	private ReadtableStructImpl(final ReadtableCase readtableCase) {
		super(ReadtableType.INSTANCE, null, null);
		this.readtableCase = readtableCase;
	}

	public static ReadtableStruct valueOf() {
		return new ReadtableStructImpl();
	}

	public static ReadtableStruct valueOf(final ReadtableCase readtableCase) {
		return new ReadtableStructImpl(readtableCase);
	}

	@Override
	public boolean makeDispatchMacroCharacter(final DispatchingReaderMacroFunction dispatchTable, final int codePoint, final boolean nonTerminatingP) {
		setMacroCharacter(codePoint, dispatchTable, nonTerminatingP);
		dispatchTableMap.put(codePoint, dispatchTable);
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
		return dispatchTableMap.get(dispatchCodePoint).getMacroFunction(subCodePoint);
	}

	@Override
	public void setDispatchMacroCharacter(final int dispatchCodePoint, final int subCodePoint, final FunctionStruct readerMacroFunction) {
		dispatchTableMap.get(dispatchCodePoint).setMacroCharacter(subCodePoint, readerMacroFunction);
	}

	@Override
	public AttributeType getAttributeType(final int codePoint, final IntegerStruct readBase) {
		return attributeTable.getAttribute(codePoint, readBase);
	}

	@Override
	public SyntaxType getSyntaxType(final int codePoint) {
		return syntaxTable.getSyntaxType(codePoint);
	}
}
