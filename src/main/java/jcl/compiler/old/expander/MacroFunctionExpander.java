package jcl.compiler.old.expander;

import jcl.LispStruct;

/**
 * Defines an interface for MacroFunction lisp type
 */
public interface MacroFunctionExpander {

	LispStruct expand(Object arg1, Object arg2);
}