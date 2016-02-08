/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.printer.Printer;
import jcl.streams.InputStream;
import jcl.streams.OutputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

abstract class AbstractStreamFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	protected TypeValidator validator;

	@Autowired
	protected Printer printer;

	protected AbstractStreamFunction(final String documentation) {
		super(documentation);
	}

	protected InputStream getInputStreamFromDesignator(final LispStruct lispStruct) {
		if (TStruct.INSTANCE.equals(lispStruct)) {
			return StreamVariables.TERMINAL_IO.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct) || NullStruct.INSTANCE.equals(lispStruct)) {
			return StreamVariables.STANDARD_INPUT.getVariableValue();
		} else if (lispStruct instanceof InputStream) {
			return (InputStream) lispStruct;
		} else {
			throw new TypeErrorException("Not Stream designator.");
		}
	}

	protected OutputStream getOutputStreamFromDesignator(final LispStruct lispStruct) {
		if (TStruct.INSTANCE.equals(lispStruct)) {
			return StreamVariables.TERMINAL_IO.getVariableValue();
		} else if (NILStruct.INSTANCE.equals(lispStruct) || NullStruct.INSTANCE.equals(lispStruct)) {
			return StreamVariables.STANDARD_OUTPUT.getVariableValue();
		} else if (lispStruct instanceof OutputStream) {
			return (OutputStream) lispStruct;
		} else {
			throw new TypeErrorException("Not Stream designator.");
		}
	}
}
