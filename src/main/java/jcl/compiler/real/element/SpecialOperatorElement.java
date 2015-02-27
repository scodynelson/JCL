/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.packages.GlobalPackageStruct;

public final class SpecialOperatorElement extends SymbolElement {

	public static final SpecialOperatorElement BLOCK = new SpecialOperatorElement("BLOCK");

	public static final SpecialOperatorElement CATCH = new SpecialOperatorElement("CATCH");

	public static final SpecialOperatorElement EVAL_WHEN = new SpecialOperatorElement("EVAL_WHEN");

	public static final SpecialOperatorElement FLET = new SpecialOperatorElement("FLET");

	public static final SpecialOperatorElement FUNCTION = new SpecialOperatorElement("FUNCTION");

	public static final SpecialOperatorElement GO = new SpecialOperatorElement("GO");

	public static final SpecialOperatorElement IF = new SpecialOperatorElement("IF");

	public static final SpecialOperatorElement LABELS = new SpecialOperatorElement("LABELS");

	public static final SpecialOperatorElement LET = new SpecialOperatorElement("LET");

	public static final SpecialOperatorElement LET_STAR = new SpecialOperatorElement("LET_STAR");

	public static final SpecialOperatorElement LOAD_TIME_VALUE = new SpecialOperatorElement("LOAD_TIME_VALUE");

	public static final SpecialOperatorElement LOCALLY = new SpecialOperatorElement("LOCALLY");

	public static final SpecialOperatorElement MACROLET = new SpecialOperatorElement("MACROLET");

	public static final SpecialOperatorElement MULTIPLE_VALUE_CALL = new SpecialOperatorElement("MULTIPLE_VALUE_CALL");

	public static final SpecialOperatorElement MULTIPLE_VALUE_PROG1 = new SpecialOperatorElement("MULTIPLE_VALUE_PROG1");

	public static final SpecialOperatorElement PROGN = new SpecialOperatorElement("PROGN");

	public static final SpecialOperatorElement PROGV = new SpecialOperatorElement("PROGV");

	public static final SpecialOperatorElement QUOTE = new SpecialOperatorElement("QUOTE");

	public static final SpecialOperatorElement RETURN_FROM = new SpecialOperatorElement("RETURN_FROM");

	public static final SpecialOperatorElement SETQ = new SpecialOperatorElement("SETQ");

	public static final SpecialOperatorElement SYMBOL_MACROLET = new SpecialOperatorElement("SYMBOL_MACROLET");

	public static final SpecialOperatorElement TAGBODY = new SpecialOperatorElement("TAGBODY");

	public static final SpecialOperatorElement THE = new SpecialOperatorElement("THE");

	public static final SpecialOperatorElement THROW = new SpecialOperatorElement("THROW");

	public static final SpecialOperatorElement UNWIND_PROTECT = new SpecialOperatorElement("UNWIND_PROTECT");

	public static final SpecialOperatorElement DECLARE = new SpecialOperatorElement("DECLARE");

	public static final SpecialOperatorElement LAMBDA = new SpecialOperatorElement("LAMBDA");

	public static final SpecialOperatorElement DEFSTRUCT = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%DEFSTRUCT");

	public static final SpecialOperatorElement FUNCTION_MARKER = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%FUNCTION-MARKER");

	public static final SpecialOperatorElement LAMBDA_MARKER = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%LAMBDA");

	public static final SpecialOperatorElement FLET_MARKER = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%FLET");

	public static final SpecialOperatorElement LABELS_MARKER = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%LABELS");

	public static final SpecialOperatorElement LET_MARKER = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%LET");

	public static final SpecialOperatorElement MACRO_MARKER = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%MACRO");

	public static final SpecialOperatorElement STATIC_FIELD = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%STATIC-FIELD");

	public static final SpecialOperatorElement TAIL_CALL = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%TAIL-CALL");

	public static final SpecialOperatorElement TAIL_RECURSION = new SpecialOperatorElement(GlobalPackageStruct.COMPILER.getName(), "%TAIL-RECURSION");

	private static final long serialVersionUID = -7262542814301280400L;

	private SpecialOperatorElement(final String symbolName) {
		super(GlobalPackageStruct.COMMON_LISP.getName(), symbolName);
	}

	private SpecialOperatorElement(final String packageName, final String symbolName) {
		super(GlobalPackageStruct.COMMON_LISP.getName(), symbolName);
	}
}
