/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;

import java.math.BigInteger;

@FunctionalInterface
public interface ReaderMacroFunction {

	LispStruct readMacro(int codePoint, Reader reader, BigInteger numArg);
}
