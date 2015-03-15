package jcl.compiler.old;

import jcl.LispStruct;
import jcl.lists.ListStruct;

/**
 * Defines an interface for MacroFunction lisp type
 */
public interface MacroFunctionExpander {

	LispStruct expand(ListStruct arg1);
}