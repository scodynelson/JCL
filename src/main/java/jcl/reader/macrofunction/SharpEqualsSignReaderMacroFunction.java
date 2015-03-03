/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.Map;
import java.util.UUID;

/**
 * Implements the '#=' Lisp reader macro.
 */
@Component
public class SharpEqualsSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -23240558522016014L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.EQUALS_SIGN, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.EQUALS_SIGN;

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("Missing label for #=.");
		}

		final Map<BigInteger, LispStruct> sharpEqualFinalTable = reader.getSharpEqualFinalTable();
		final Map<BigInteger, UUID> sharpEqualTempTable = reader.getSharpEqualTempTable();

		if (sharpEqualFinalTable.containsKey(numArg)
				|| sharpEqualTempTable.containsKey(numArg)) {
			throw new ReaderErrorException("Label already defined: #" + numArg + '=');
		}

		final UUID tag = UUID.randomUUID();
		sharpEqualTempTable.put(numArg, tag);

		final LispStruct token = reader.read();
		reader.getSharpEqualReplTable().put(tag, token);

		sharpEqualFinalTable.put(numArg, token);

		return null;
	}
}
