/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.arrays.VectorStruct;
import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.List;

/**
 * Implements the '#(...)' Lisp reader macro.
 */
@Component
public class SharpLeftParenthesisReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1826957244403929085L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpLeftParenthesisReaderMacroFunction.class);

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LEFT_PARENTHESIS, this);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		final ConsElement listToken = ListReaderMacroFunction.readList(reader);

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", listToken);
			}
			return null;
		}

		if (listToken == null) {
			throw new ReaderErrorException("Ill-formed vector: #");
		}

		if (listToken.isDotted()) {
			throw new ReaderErrorException("Ill-formed vector: #" + listToken);
		}

		final List<SimpleElement> lispTokens = listToken.getElements();

		if (numArg == null) {
			return createVector(lispTokens);
		}

		return handleNumArg(lispTokens, numArg, listToken.toLispStruct().printStruct()); // TODO: fix
	}

	/**
	 * Handles the processing of the number argument when parsing the provided list of {@link LispStruct}s into a
	 * {@link VectorStruct}.
	 *
	 * @param lispTokens
	 * 		the vector contents
	 * @param numArg
	 * 		the number argument passed to be used as the vector length
	 * @param listToken
	 * 		the printed representation of the vector contents
	 *
	 * @return the properly created {@link VectorStruct} taking care of the proper vector length
	 */
	private static ConsElement handleNumArg(final List<SimpleElement> lispTokens, final BigInteger numArg, final String listToken) {

		final int numberOfTokens = lispTokens.size();
		final int numArgInt = numArg.intValueExact();
		if (numberOfTokens > numArgInt) {
			throw new ReaderErrorException("Vector is longer than specified length: #" + numArg + listToken);
		}

		SimpleElement lastToken = null;
		if (CollectionUtils.isNotEmpty(lispTokens)) {
			lastToken = lispTokens.get(numberOfTokens - 1);
		}

		final int fillAmount = numArgInt - numberOfTokens;
		for (int i = 0; i < fillAmount; i++) {
			lispTokens.add(lastToken);
		}

		return createVector(lispTokens);
	}

	/**
	 * Creates creates the {@link ListStruct} calling the appropriate function needed to produce the {@link
	 * VectorStruct} from the provided {@code lispTokens}.
	 *
	 * @param lispTokens
	 * 		the {@link LispStruct} tokens used to create the {@link VectorStruct}
	 *
	 * @return the {@link ListStruct} calling the appropriate function needed to produce the {@link VectorStruct}
	 */
	private static ConsElement createVector(final List<SimpleElement> lispTokens) {
		final int numberOfTokens = lispTokens.size();
		final BigInteger numberOfTokensBI = BigInteger.valueOf(numberOfTokens);

		final SymbolElement makeArrayFnSymbol = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "MAKE-ARRAY");
		final IntegerElement dimensions = new IntegerElement(numberOfTokensBI);
		final SymbolElement elementTypeKeyword = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "ELEMENT-TYPE");
		final SymbolElement elementType = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "T");
		final SymbolElement initialContentsKeyword = new SymbolElement(GlobalPackageStruct.KEYWORD.getName(), "INITIAL-CONTENTS");

		final SimpleElement initialContents;
		if (lispTokens.isEmpty()) {
			initialContents = NullElement.INSTANCE;
		} else {
			initialContents = new ConsElement(lispTokens);
		}

		return new ConsElement(makeArrayFnSymbol, dimensions, elementTypeKeyword, elementType, initialContentsKeyword, initialContents);
	}
}
