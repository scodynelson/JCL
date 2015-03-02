/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;
import jcl.system.EnhancedLinkedList;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the ',' Lisp reader macro.
 */
@Component
public class CommaReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8890411312426952661L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CommaReaderMacroFunction.class);

	@Autowired
	private Printer printer;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.COMMA, this, false);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.GRAVE_ACCENT;

		if (BackquoteReaderMacroFunction.backQuoteCount <= 0) {
			if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
				if (LOGGER.isDebugEnabled()) {
					LOGGER.debug(", error suppressed.");
				}
				return null;
			}

			throw new ReaderErrorException("Comma not inside a backquote.");
		}

		final ReadPeekResult c = reader.readChar();
		final int nextCodePoint = c.getResult();

		BackquoteReaderMacroFunction.backQuoteCount--;
		try {

			final ConsElement consElement;

			if (nextCodePoint == CharacterConstants.AT_SIGN) {
				final SimpleElement code = reader.read();

				if (code instanceof NullElement) {
					consElement = new ConsElement(BackquoteReaderMacroFunction.Flag.BQ_AT_FLAG);
				} else if (code instanceof ConsElement) {
					final ConsElement codeConsElement = (ConsElement) code;
					final EnhancedLinkedList<SimpleElement> codeElements = codeConsElement.getElements();

					final EnhancedLinkedList<SimpleElement> bqReturnThingElements = new EnhancedLinkedList<>();
					bqReturnThingElements.add(BackquoteReaderMacroFunction.Flag.BQ_AT_FLAG);
					bqReturnThingElements.addAll(codeElements);

					final boolean isDotted = codeConsElement.isDotted();
					consElement = new ConsElement(isDotted, bqReturnThingElements);
				} else {
					// This is fine to create ConsElement like this here since the second element is not a ListElement type
					consElement = new ConsElement(true, BackquoteReaderMacroFunction.Flag.BQ_AT_FLAG, code);
				}
			} else if (nextCodePoint == CharacterConstants.FULL_STOP) {
				final SimpleElement code = reader.read();

				if (code instanceof NullElement) {
					consElement = new ConsElement(BackquoteReaderMacroFunction.Flag.BQ_DOT_FLAG);
				} else if (code instanceof ConsElement) {
					final ConsElement codeConsElement = (ConsElement) code;
					final EnhancedLinkedList<SimpleElement> codeElements = codeConsElement.getElements();

					final EnhancedLinkedList<SimpleElement> bqReturnThingElements = new EnhancedLinkedList<>();
					bqReturnThingElements.add(BackquoteReaderMacroFunction.Flag.BQ_DOT_FLAG);
					bqReturnThingElements.addAll(codeElements);

					final boolean isDotted = codeConsElement.isDotted();
					consElement = new ConsElement(isDotted, bqReturnThingElements);
				} else {
					// This is fine to create ConsElement like this here since the second element is not a ListElement type
					consElement = new ConsElement(true, BackquoteReaderMacroFunction.Flag.BQ_DOT_FLAG, code);
				}
			} else {
				reader.unreadChar(nextCodePoint);
				final SimpleElement code = reader.read();

				if (code instanceof NullElement) {
					consElement = new ConsElement(BackquoteReaderMacroFunction.Flag.BQ_COMMA_FLAG);
				} else if (code instanceof ConsElement) {
					final ConsElement codeConsElement = (ConsElement) code;
					final EnhancedLinkedList<SimpleElement> codeElements = codeConsElement.getElements();

					final EnhancedLinkedList<SimpleElement> bqReturnThingElements = new EnhancedLinkedList<>();
					bqReturnThingElements.add(BackquoteReaderMacroFunction.Flag.BQ_COMMA_FLAG);
					bqReturnThingElements.addAll(codeElements);

					final boolean isDotted = codeConsElement.isDotted();
					consElement = new ConsElement(isDotted, bqReturnThingElements);
				} else {
					// This is fine to create ConsElement like this here since the second element is not a ListElement type
					consElement = new ConsElement(true, BackquoteReaderMacroFunction.Flag.BQ_COMMA_FLAG, code);
				}
			}
			return consElement;
		} finally {
			BackquoteReaderMacroFunction.backQuoteCount++;
		}
	}
}
