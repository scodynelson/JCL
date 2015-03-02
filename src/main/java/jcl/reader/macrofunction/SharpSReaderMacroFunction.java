/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ListElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.streams.ReadPeekResult;
import jcl.system.EnhancedLinkedList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#s' Lisp reader macro.
 */
@Component
public class SharpSReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3540324881853180103L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpSReaderMacroFunction.class);

	@Autowired
	private ListReaderMacroFunction listReaderMacroFunction;

	@Autowired
	private Printer printer;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_S, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_S, this);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_S) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_S);

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			final SimpleElement lispToken = reader.read();
			if (LOGGER.isDebugEnabled()) {
				final String printedToken = printer.print(lispToken);
				LOGGER.debug("{} suppressed.", printedToken);
			}
			return null;
		}

		final ReadPeekResult readResult = reader.readChar();
		final int nextCodePoint = readResult.getResult();
		if (nextCodePoint != CharacterConstants.LEFT_PARENTHESIS) {
			throw new ReaderErrorException("Non-list following #S");
		}

		final ListElement listToken = listReaderMacroFunction.readList(reader);
		if (listToken == null) {
			throw new ReaderErrorException("Non-list following #S");
		}

		final EnhancedLinkedList<SimpleElement> elements = listToken.getElements();
		if (elements.isEmpty()) {
			throw new ReaderErrorException("Structure type was not supplied");
		}

		final SimpleElement structureType = elements.getFirst();
		if (!(structureType instanceof SymbolElement)) {
			throw new ReaderErrorException("Structure type is not a symbol: " + structureType);
		}

		// TODO: Find class object from structureType value
//		if (!(classObj instanceof StructureClass)) {
//			throw new ReaderErrorException(structureType + " is not a defined structure type.");
//		}

		// TODO: Get default constructor for the structure
//		if (defCon == null) {
//			throw new ReaderErrorException("The " + structureType + " structure does not have a default constructor.");
//		}

		// TODO: Call constructor to create Structure object

		return null;
	}
}
