/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.functions;

import jcl.LispStruct;
import jcl.functions.FunctionStruct;
import jcl.reader.Reader;
import jcl.streams.InputStream;
import jcl.symbols.BooleanStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class ReadFunction extends FunctionStruct {

	private static final long serialVersionUID = 907539293814708746L;

	@Autowired
	private ApplicationContext context;

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		// TODO
		return null;
	}

	public LispStruct read(final InputStream inputStream, final BooleanStruct eofErrorP, final LispStruct eofValue,
	                       final BooleanStruct recursiveP) {

		final Reader reader = context.getBean(Reader.class, inputStream);
		return reader.read(eofErrorP.booleanValue(), eofValue, recursiveP.booleanValue());
	}
}
