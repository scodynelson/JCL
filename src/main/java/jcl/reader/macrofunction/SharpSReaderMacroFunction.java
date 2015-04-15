/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.streams.ReadPeekResult;
import jcl.structures.StructureClassStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#s' Lisp reader macro.
 */
@Component
public class SharpSReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3540324881853180103L;

	/**
	 * {@link Autowired} {@link ListReaderMacroFunction} used for reading {@link ListStruct}s.
	 */
	@Autowired
	private ListReaderMacroFunction listReaderMacroFunction;

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
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_S) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_S);

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			reader.read(true, NullStruct.INSTANCE, true);
			return NullStruct.INSTANCE;
		}

		final ReadPeekResult readResult = reader.readChar(true, NullStruct.INSTANCE, false);
		final int nextCodePoint = readResult.getResult();
		if (nextCodePoint != CharacterConstants.LEFT_PARENTHESIS) {
			throw new ReaderErrorException("Non-list following #S");
		}

		final ListStruct listToken = listReaderMacroFunction.readList(reader);
		if (listToken == null) {
			throw new ReaderErrorException("Non-list following #S");
		}

		if (listToken.size() == 0) {
			throw new ReaderErrorException("Structure type was not supplied");
		}

		final LispStruct structureType = listToken.getFirst();
		if (!(structureType instanceof SymbolStruct)) {
			throw new ReaderErrorException("Structure type is not a symbol: " + structureType);
		}

		final SymbolStruct<?> structureSymbol = (SymbolStruct<?>) structureType;
		final StructureClassStruct structureClass = structureSymbol.getStructureClass();
		if (structureClass == null) {
			throw new ReaderErrorException(structureType + " is not a defined structure type for symbol: " + structureSymbol);
		}

		final FunctionStruct defaultConstructor = structureClass.getDefaultConstructor();
		if (defaultConstructor == null) {
			throw new ReaderErrorException("The " + structureType + " structure does not have a default constructor.");
		}

		final ListStruct arguments = listToken.getRest();
		final List<LispStruct> argumentsAsJavaList = arguments.getAsJavaList();

		LispStruct[] argumentsArray = new LispStruct[argumentsAsJavaList.size()];
		argumentsArray = argumentsAsJavaList.toArray(argumentsArray);
		return defaultConstructor.apply(argumentsArray);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(listReaderMacroFunction)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final SharpSReaderMacroFunction rhs = (SharpSReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(listReaderMacroFunction, rhs.listReaderMacroFunction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(listReaderMacroFunction)
		                                                                .toString();
	}
}
